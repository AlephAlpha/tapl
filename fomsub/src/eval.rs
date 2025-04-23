use crate::syntax::{Binding, Context, DeBruijnTerm, DeBruijnTy, Kind, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTy {
    fn promote(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(ty1, ty2) => Ok(Self::app(ty1.promote(ctx)?, ty2.clone())),
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TyVar(ty)) => Ok(ty),
                _ => Err(Error::NoRuleApplies),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    fn compute(&self, _ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(ty1, ty2) => match ty1.as_ref() {
                Self::Abs(_, _, ty) => ty.subst_top(ty2),
                _ => Ok(Self::app(ty1.compute(_ctx)?, ty2.clone())),
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

    fn lcst(&self, ctx: &Context) -> Rc<Self> {
        let mut ty = self.simplify(ctx);
        while let Ok(ty_) = ty.promote(ctx) {
            ty = ty_.simplify(ctx);
        }
        ty
    }

    fn eqv(&self, other: &Self, ctx: &mut Context) -> bool {
        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Var(i), Self::Var(j)) => i == j,
            (Self::Abs(x, kn, ty), Self::Abs(_, kn_, ty_)) => {
                kn == kn_ && ctx.with_name(x.clone(), |ctx| ty.eqv(ty_, ctx))
            }
            (Self::App(ty1, ty2), Self::App(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::All(x, ty1, ty2), Self::All(_, kn_, ty_)) => {
                ty1 == kn_ && ctx.with_name(x.clone(), |ctx| ty2.eqv(ty_, ctx))
            }
            (Self::Top, Self::Top) => true,
            _ => false,
        }
    }

    pub fn kind_of(&self, ctx: &mut Context) -> Result<Rc<Kind>> {
        match self {
            Self::Arr(ty1, ty2) => {
                if matches!(ty1.kind_of(ctx)?.as_ref(), Kind::Star)
                    && matches!(ty2.kind_of(ctx)?.as_ref(), Kind::Star)
                {
                    Ok(Kind::star())
                } else {
                    Err(Error::KindError("star kind expected".to_string()))
                }
            }
            Self::Var(i) => match ctx.get_binding_shifting(*i)? {
                Binding::TyVar(ty) => ty.kind_of(ctx),
                _ => Err(Error::KindError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, kn1, ty2) => {
                ctx.with_binding(x.clone(), Binding::TyVar(kn1.make_top()), |ctx| {
                    let kn2 = ty2.kind_of(ctx)?;
                    Ok(Kind::arr(kn1.clone(), kn2))
                })
            }
            Self::App(ty1, ty2) => {
                let kn1 = ty1.kind_of(ctx)?;
                let kn2 = ty2.kind_of(ctx)?;
                match kn1.as_ref() {
                    Kind::Arr(kn11, kn12) => {
                        if *kn11 == kn2 {
                            Ok(kn12.clone())
                        } else {
                            Err(Error::KindError("parameter kind mismatch".to_string()))
                        }
                    }
                    _ => Err(Error::KindError("arrow kind expected".to_string())),
                }
            }
            Self::All(x, ty1, ty2) => {
                ctx.with_binding(x.clone(), Binding::TyVar(ty1.clone()), |ctx| {
                    let kn2 = ty2.kind_of(ctx)?;
                    if matches!(kn2.as_ref(), Kind::Star) {
                        Ok(Kind::star())
                    } else {
                        Err(Error::KindError("star kind expected".to_string()))
                    }
                })
            }
            _ => Ok(Kind::star()),
        }
    }

    fn subtype(&self, other: &Self, ctx: &mut Context) -> bool {
        if self.eqv(other, ctx) {
            return true;
        }

        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (_, Self::Top) => self_
                .kind_of(ctx)
                .is_ok_and(|kn| matches!(kn.as_ref(), Kind::Star)),
            (Self::Var(_), _) => self_.promote(ctx).is_ok_and(|ty| ty.subtype(other, ctx)),
            (Self::All(x, ty1, ty2), Self::All(_, ty1_, ty2_)) => {
                ty1.subtype(ty1_, ctx)
                    && ty1_.subtype(ty1, ctx)
                    && ctx.with_binding(x.clone(), Binding::TyVar(ty1_.clone()), |ctx| {
                        ty2.subtype(ty2_, ctx)
                    })
            }
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1_.subtype(ty1, ctx) && ty2.subtype(ty2_, ctx)
            }
            (Self::Abs(x, kn1, ty2), Self::Abs(_, kn1_, ty2_)) => {
                kn1 == kn1_
                    && ctx.with_binding(x.clone(), Binding::TyVar(kn1.make_top()), |ctx| {
                        ty2.subtype(ty2_, ctx)
                    })
            }
            (Self::App(_, _), _) => self_.promote(ctx).is_ok_and(|ty| ty.subtype(other, ctx)),
            _ => false,
        }
    }
}

impl Ty {
    pub fn kind_of(&self, ctx: &mut Context) -> Result<Rc<Kind>> {
        self.to_de_bruijn(ctx)?.kind_of(ctx)
    }
}

impl DeBruijnTerm {
    const fn is_val(&self, _ctx: &Context) -> bool {
        matches!(self, Self::TAbs(_, _, _) | Self::Abs(_, _, _))
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
            Self::TApp(t, ty) => match t.as_ref() {
                Self::TAbs(_, _, t) => t.subst_top_ty(ty),
                _ => Ok(Self::t_app(t.eval1(ctx)?, ty.clone())),
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
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, ty1, t2) => {
                if matches!(ty1.kind_of(ctx)?.as_ref(), Kind::Star) {
                    ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                        Ok(Ty::arr(ty1.clone(), t2.type_of(ctx)?.shift(-1)?))
                    })
                } else {
                    Err(Error::TypeError("star kind expected".to_string()))
                }
            }
            Self::App(t1, t2) => match t1.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::Arr(ty11, ty12) => {
                    if t2.type_of(ctx)?.subtype(ty11, ctx) {
                        Ok(ty12.clone())
                    } else {
                        Err(Error::TypeError("parameter type mismatch".to_string()))
                    }
                }
                _ => Err(Error::TypeError("arrow type expected".to_string())),
            },
            Self::TAbs(x, ty1, t2) => {
                ctx.with_binding(x.clone(), Binding::TyVar(ty1.clone()), |ctx| {
                    Ok(Ty::all(x.clone(), ty1.clone(), t2.type_of(ctx)?))
                })
            }
            Self::TApp(t1, ty2) => match t1.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::All(_, ty11, ty12) => {
                    if ty2.subtype(ty11, ctx) {
                        ty12.subst_top(ty2)
                    } else {
                        Err(Error::TypeError("type argument has wrong kind".to_string()))
                    }
                }
                _ => Err(Error::TypeError("universal type expected".to_string())),
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
