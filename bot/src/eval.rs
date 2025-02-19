use crate::syntax::{Binding, Context, DeBruijnTerm, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl Ty {
    fn subtype(&self, other: &Self, _ctx: &Context) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (Self::Bot, _) | (_, Self::Top) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1_.subtype(ty1, _ctx) && ty2.subtype(ty2_, _ctx)
            }
            _ => false,
        }
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

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Var(i) => match ctx.get_binding(*i)? {
                Binding::Var(ty) => Ok(ty.clone()),
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, ty1, t2) => {
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let ty2 = t2.type_of(ctx)?;
                    Ok(Ty::arr(ty1.clone(), ty2))
                })
            }
            Self::App(t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                let ty2 = t2.type_of(ctx)?;
                match ty1.as_ref() {
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
        self.to_de_bruijn(ctx)?.type_of(ctx)
    }
}
