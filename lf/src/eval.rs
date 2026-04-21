use crate::syntax::{
    Binding, Context, DeBruijnBinding, DeBruijnKind, DeBruijnTerm, DeBruijnTy, Kind, Term, Ty,
};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnKind {
    pub fn check(&self, ctx: &mut Context) -> Result<()> {
        match self {
            Self::Star => Ok(()),
            Self::Pi(x, ty, kn) => match ty.kind_of(ctx)?.as_ref() {
                Self::Star => {
                    ctx.with_binding(x.clone(), Binding::Var(ty.clone()), |ctx| kn.check(ctx))
                }
                _ => Err(Error::KindError("Star kind expected".to_string())),
            },
        }
    }
}

impl DeBruijnTy {
    fn eqv(&self, other: &Self, ctx: &mut Context) -> bool {
        match (self, other) {
            (Self::Var(x), Self::Var(y)) => x == y,
            (Self::Pi(x, ty1, ty2), Self::Pi(_, ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx)
                    && ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                        ty2.eqv(ty2_, ctx)
                    })
            }
            (Self::App(ty, t), Self::App(ty_, t_)) => ty.eqv(ty_, ctx) && t.eqv(t_, ctx),
            _ => false,
        }
    }

    pub fn kind_of(&self, ctx: &mut Context) -> Result<Rc<DeBruijnKind>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i)? {
                Binding::TyVar(kn) => Ok(kn),
                _ => Err(Error::KindError(format!(
                    "Wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Pi(x, ty1, ty2) => {
                let kn1 = ty1.kind_of(ctx)?;
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let kn2 = ty2.kind_of(ctx)?;
                    if matches!(kn1.as_ref(), Kind::Star) && matches!(kn2.as_ref(), Kind::Star) {
                        Ok(Kind::star())
                    } else {
                        Err(Error::KindError("Star kind expected".to_string()))
                    }
                })
            }
            Self::App(ty, t) => match ty.kind_of(ctx)?.as_ref() {
                Kind::Pi(_, ty1, kn2) => {
                    if t.type_of(ctx)?.eqv(ty1, ctx) {
                        kn2.subst_top_term(t)
                    } else {
                        Err(Error::KindError("Parameter type mismatch".to_string()))
                    }
                }
                _ => Err(Error::KindError("Pi kind expected".to_string())),
            },
        }
    }
}

impl Ty {
    pub fn kind_of(&self, ctx: &mut Context) -> Result<Rc<Kind>> {
        self.to_de_bruijn(ctx)?.kind_of(ctx)?.to_named(ctx)
    }
}

impl DeBruijnTerm {
    const fn is_val(&self, _ctx: &Context) -> bool {
        matches!(self, Self::Abs(_, _, _))
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, t) if t2.is_val(ctx) => t.subst_top(t2),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
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

    fn eval1_wh(&self, _ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, t) => t.subst_top(t2),
                _ => Ok(Self::app(t1.eval1_wh(_ctx)?, t2.clone())),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    fn whnf(&self, ctx: &Context) -> Rc<Self> {
        let mut t = Rc::new(self.clone());
        while let Ok(t_) = t.eval1_wh(ctx) {
            t = t_;
        }
        t
    }

    fn eqv_wh(&self, other: &Self, ctx: &mut Context) -> bool {
        match (self, other) {
            (Self::Var(x), Self::Var(y)) => x == y,
            (Self::App(t1, t2), Self::App(t1_, t2_)) => t1.eqv_wh(t1_, ctx) && t2.eqv_wh(t2_, ctx),
            (Self::Abs(x, ty1, t1), Self::Abs(_, ty1_, t1_)) => {
                ty1.eqv(ty1_, ctx)
                    && ctx
                        .with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| t1.eqv(t1_, ctx))
            }
            (Self::Abs(x, ty, t), s) | (s, Self::Abs(x, ty, t)) => {
                ctx.with_binding(x.clone(), Binding::Var(ty.clone()), |ctx| {
                    let t_ = Self::app(Rc::new(s.clone()), Rc::new(Self::Var(0)));
                    t_.eqv(t, ctx)
                })
            }
            _ => false,
        }
    }

    fn eqv(&self, other: &Self, ctx: &mut Context) -> bool {
        let self_ = self.whnf(ctx);
        let other_ = other.whnf(ctx);

        self_.eqv_wh(&other_, ctx)
    }

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i)? {
                Binding::Var(ty) => Ok(ty),
                _ => Err(Error::TypeError(format!(
                    "Wrong type of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, ty, t) => {
                if matches!(ty.kind_of(ctx)?.as_ref(), Kind::Star) {
                    ctx.with_binding(x.clone(), Binding::Var(ty.clone()), |ctx| {
                        Ok(Ty::pi(x.clone(), ty.clone(), t.type_of(ctx)?))
                    })
                } else {
                    Err(Error::KindError("Star kind expected".to_string()))
                }
            }
            Self::App(t1, t2) => match t1.type_of(ctx)?.as_ref() {
                Ty::Pi(_, ty1, ty2) => {
                    if t2.type_of(ctx)?.eqv(ty1, ctx) {
                        ty2.subst_top_term(t2)
                    } else {
                        Err(Error::TypeError("Parameter type mismatch".to_string()))
                    }
                }
                _ => Err(Error::TypeError("Pi type expected".to_string())),
            },
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
    pub fn check(&self, ctx: &mut Context) -> Result<Self> {
        match self {
            Self::Var(ty) => {
                if matches!(ty.kind_of(ctx)?.as_ref(), Kind::Star) {
                    Ok(Self::Var(ty.clone()))
                } else {
                    Err(Error::KindError("Star kind expected".to_string()))
                }
            }
            Self::TyVar(kn) => {
                kn.check(ctx)?;
                Ok(Self::TyVar(kn.clone()))
            }
            _ => Ok(self.clone()),
        }
    }
}

impl Binding {
    pub fn check(&self, ctx: &mut Context) -> Result<DeBruijnBinding> {
        self.to_de_bruijn(ctx)?.check(ctx)
    }
}
