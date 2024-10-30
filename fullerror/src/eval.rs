use crate::syntax::{Binding, Context, DeBruijnBinding, DeBruijnTerm, DeBruijnTy, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTy {
    fn compute(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TyAbb(t)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    fn simplify(&self, ctx: &Context) -> Rc<Self> {
        let mut t = Rc::new(self.clone());
        while let Ok(t_) = t.compute(ctx) {
            t = t_;
        }
        t
    }

    fn eqv(&self, other: &Self, ctx: &Context) -> bool {
        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::Bot, Self::Bot) | (Self::Top, Self::Top) | (Self::Bool, Self::Bool) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Var(i), Self::Var(j)) => i == j,
            _ => false,
        }
    }

    fn subtype(&self, other: &Self, ctx: &Context) -> bool {
        if self.eqv(other, ctx) {
            return true;
        }

        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::Bot, _) | (_, Self::Top) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1_.subtype(ty1, ctx) && ty2.subtype(ty2_, ctx)
            }
            _ => false,
        }
    }

    fn join(self: &Rc<Self>, other: &Rc<Self>, ctx: &Context) -> Rc<Self> {
        if self.subtype(other, ctx) {
            return other.clone();
        }
        if other.subtype(self, ctx) {
            return self.clone();
        }

        match (self.as_ref(), other.as_ref()) {
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                Self::arr(ty1.meet(ty1_, ctx), ty2.join(ty2_, ctx))
            }
            _ => Self::top(),
        }
    }

    fn meet(self: &Rc<Self>, other: &Rc<Self>, ctx: &Context) -> Rc<Self> {
        if self.subtype(other, ctx) {
            return self.clone();
        }
        if other.subtype(self, ctx) {
            return other.clone();
        }

        match (self.as_ref(), other.as_ref()) {
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                Self::arr(ty1.join(ty1_, ctx), ty2.meet(ty2_, ctx))
            }
            _ => Self::bot(),
        }
    }
}

impl DeBruijnTerm {
    const fn is_val(&self, _ctx: &Context) -> bool {
        matches!(self, Self::True | Self::False | Self::Abs(_, _, _))
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                Self::Error => Ok(Self::error()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TermAbb(t, _)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Error => Ok(Self::error()),
                Self::Abs(_, _, t) if t2.is_val(ctx) => Ok(t.subst_top(t2)),
                _ => {
                    if t1.is_val(ctx) {
                        match t2.as_ref() {
                            Self::Error => Ok(Self::error()),
                            _ => Ok(Self::app(t1.clone(), t2.eval1(ctx)?)),
                        }
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
            },
            Self::Try(t1, t2) => match t1.as_ref() {
                Self::Error => Ok(t2.clone()),
                t if t.is_val(ctx) => Ok(t1.clone()),
                _ => Ok(Self::try_(t1.eval1(ctx)?, t2.clone())),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    pub fn eval(self: &Rc<Self>, ctx: &Context) -> Rc<Self> {
        let mut t = Rc::clone(self);
        while let Ok(t_) = t.eval1(ctx) {
            t = t_;
        }
        t
    }

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i)? {
                Binding::Var(ty) => Ok(ty),
                Binding::TermAbb(_, Some(ty)) => Ok(ty),
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, ty1, t2) => {
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let ty2 = t2.type_of(ctx)?;
                    Ok(Ty::arr(ty1.clone(), ty2.shift(-1)))
                })
            }
            Self::App(t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                let ty2 = t2.type_of(ctx)?;
                match ty1.simplify(ctx).as_ref() {
                    Ty::Arr(ty11, ty12) => {
                        if ty2.subtype(ty11, ctx) {
                            Ok(ty12.clone())
                        } else {
                            Err(Error::TypeError("parameter type mismatch".to_string()))
                        }
                    }
                    Ty::Bot => Ok(Ty::bot()),
                    _ => Err(Error::TypeError("arrow type expected".to_string())),
                }
            }
            Self::True | Self::False => Ok(Ty::bool()),
            Self::If(t1, t2, t3) => {
                if t1.type_of(ctx)?.subtype(&Ty::Bool, ctx) {
                    Ok(t2.type_of(ctx)?.join(&t3.type_of(ctx)?, ctx))
                } else {
                    Err(Error::TypeError(
                        "guard of conditional not a boolean".to_string(),
                    ))
                }
            }
            Self::Error => Ok(Ty::bot()),
            Self::Try(t1, t2) => Ok(t1.type_of(ctx)?.join(&t2.type_of(ctx)?, ctx)),
        }
    }
}

impl Term {
    pub fn eval1(&self, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval1(ctx)?.to_named(ctx)
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval(ctx).to_named(ctx)
    }

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        self.to_de_bruijn(ctx)?.type_of(ctx)?.to_named(ctx)
    }
}

impl DeBruijnBinding {
    pub fn eval(&self, ctx: &Context) -> Self {
        match self {
            Self::TermAbb(t, ty) => Self::TermAbb(t.eval(ctx), ty.clone()),
            _ => self.clone(),
        }
    }

    pub fn check(&self, ctx: &mut Context) -> Result<Self> {
        match self {
            Self::TermAbb(t, ty) => {
                let ty_ = t.type_of(ctx)?;
                if let Some(ty) = ty {
                    if ty_.subtype(ty, ctx) {
                        Ok(Self::TermAbb(t.clone(), Some(ty.clone())))
                    } else {
                        Err(Error::TypeMismatch)
                    }
                } else {
                    Ok(Self::TermAbb(t.clone(), Some(ty_)))
                }
            }
            _ => Ok(self.clone()),
        }
    }
}
