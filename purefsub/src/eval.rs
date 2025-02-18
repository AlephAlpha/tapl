use crate::syntax::{Binding, Context, DeBruijnTerm, DeBruijnTy, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTy {
    fn promote(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TyVar(ty)) => Ok(ty),
                _ => Err(Error::NoRuleApplies),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    fn lcst(&self, ctx: &Context) -> Rc<Self> {
        let mut ty = Rc::new(self.clone());
        while let Ok(ty_) = ty.promote(ctx) {
            ty = ty_;
        }
        ty
    }

    fn subtype(&self, other: &Self, ctx: &mut Context) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            (_, Self::Top) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1_.subtype(ty1, ctx) && ty2.subtype(ty2_, ctx)
            }
            (Self::Var(_), _) => self.promote(ctx).is_ok_and(|ty| ty.subtype(other, ctx)),
            (Self::All(x, ty1, ty2), Self::All(_, ty1_, ty2_)) => {
                ty1.subtype(ty1_, ctx)
                    && ty1_.subtype(ty1, ctx)
                    && ctx.with_binding(x.clone(), Binding::TyVar(ty1_.clone()), |ctx| {
                        ty2.subtype(ty2_, ctx)
                    })
            }
            _ => false,
        }
    }
}

impl DeBruijnTerm {
    const fn is_val(&self, _ctx: &Context) -> bool {
        matches!(self, Self::TAbs(_, _, _) | Self::Abs(_, _, _))
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, t) if t2.is_val(ctx) => Ok(t.subst_top(t2)),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
            },
            Self::TApp(t, ty) => match t.as_ref() {
                Self::TAbs(_, _, t) => Ok(t.subst_top_ty(ty)),
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
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    Ok(Ty::arr(ty1.clone(), t2.type_of(ctx)?.shift(-1)))
                })
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
            Self::TAbs(x, ty, t) => {
                ctx.with_binding(x.clone(), Binding::TyVar(ty.clone()), |ctx| {
                    Ok(Ty::all(x.clone(), ty.clone(), t.type_of(ctx)?))
                })
            }
            Self::TApp(t, ty) => match t.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::All(_, ty1, ty2) => {
                    if ty.subtype(ty1, ctx) {
                        Ok(ty2.subst_top(ty))
                    } else {
                        Err(Error::TypeError("type parameter type mismatch".to_string()))
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
