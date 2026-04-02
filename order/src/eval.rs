use crate::syntax::{Binding, Context, DeBruijnTerm, PreTy, Qualifier, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTerm {
    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::Bool(_, _) | Self::Abs(_, _, _, _) => true,
            Self::Pair(_, t1, t2) => t1.is_val(_ctx) && t2.is_val(_ctx),
            _ => false,
        }
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::Bool(_, true) => Ok(t2.clone()),
                Self::Bool(_, false) => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Pair(q, t1, t2) => {
                if !t1.is_val(ctx) {
                    Ok(Self::pair(*q, t1.eval1(ctx)?, t2.clone()))
                } else if !t2.is_val(ctx) {
                    Ok(Self::pair(*q, t1.clone(), t2.eval1(ctx)?))
                } else {
                    Err(Error::NoRuleApplies)
                }
            }
            Self::Split(t1, x1, x2, t2) => match t1.as_ref() {
                Self::Pair(q, t11, t12) if t1.is_val(ctx) => {
                    t2.subst_top(t12.shift(1)?.as_ref())?.subst_top(t11)
                }
                _ => Ok(Self::split(
                    t1.eval1(ctx)?,
                    x1.clone(),
                    x2.clone(),
                    t2.clone(),
                )),
            },
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, _, t) if t2.is_val(ctx) => t.subst_top(t2),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    t2.subst_top(t1)
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx)?, t2.clone()))
                }
            }
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

    fn type_of_walk(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Var(i) => match ctx.use_binding(*i)? {
                Binding::Var(ty) => Ok(ty),
                Binding::Used => Err(Error::TypeError(format!(
                    "variable {} has already been used",
                    ctx.index_to_name(*i).unwrap()
                ))),
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Bool(q, _) => Ok(Ty::new(*q, PreTy::bool())),
            Self::If(t1, t2, t3) => {
                let ty1 = t1.type_of_walk(ctx)?;
                if *ty1.pre_ty == PreTy::Bool {
                    let mut ctx_ = ctx.clone();
                    let ty2 = t2.type_of_walk(ctx)?;
                    let ty3 = t3.type_of_walk(&mut ctx_)?;
                    if ty2 == ty3 && ctx == &ctx_ {
                        Ok(ty2)
                    } else {
                        Err(Error::TypeError(
                            "arms of conditional have different types".to_string(),
                        ))
                    }
                } else {
                    Err(Error::TypeError(
                        "guard of conditional not a boolean".to_string(),
                    ))
                }
            }
            Self::Pair(q, t1, t2) => {
                let ty2 = t2.type_of_walk(ctx)?;
                let ty1 = t1.type_of_walk(ctx)?;
                if ty1.qualifier >= *q && ty2.qualifier >= *q {
                    Ok(Ty::new(*q, PreTy::pair(ty1, ty2)))
                } else if q == &Qualifier::Un {
                    Err(Error::TypeError(
                        "linear components in unrestricted pair".to_string(),
                    ))
                } else {
                    Err(Error::TypeError(
                        "ordered components in non-ordered pair".to_string(),
                    ))
                }
            }
            Self::Split(t1, x1, x2, t2) => {
                let ty1 = t1.type_of_walk(ctx)?;
                match ty1.pre_ty.as_ref() {
                    PreTy::Pair(ty11, ty12) => {
                        ctx.with_linear_binding(x1.clone(), Binding::Var(ty11.clone()), |ctx| {
                            ctx.with_linear_binding(x2.clone(), Binding::Var(ty12.clone()), |ctx| {
                                t2.type_of_walk(ctx)
                            })
                        })
                    }
                    _ => Err(Error::TypeError("expected pair type".to_string())),
                }
            }
            Self::Abs(q, x, ty1, t2) => {
                let ctx_ = ctx.clone();
                let ty = ctx.with_linear_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let ty2 = t2.type_of_walk(ctx)?;
                    Ok(Ty::new(*q, PreTy::arr(ty1.clone(), ty2)))
                });

                if q != &Qualifier::Un || ctx == &ctx_ {
                    ty
                } else if q == &Qualifier::Un {
                    Err(Error::TypeError(
                        "linear variable captured by unrestricted function".to_string(),
                    ))
                } else {
                    Err(Error::TypeError(
                        "ordered variable captured by non-ordered function".to_string(),
                    ))
                }
            }
            Self::App(t1, t2) => {
                let ty2 = t2.type_of(ctx)?;
                let ty1 = t1.type_of(ctx)?;
                match ty1.pre_ty.as_ref() {
                    PreTy::Arr(ty11, ty12) => {
                        if ty2 == *ty11 {
                            Ok(ty12.clone())
                        } else {
                            Err(Error::TypeError("parameter type mismatch".to_string()))
                        }
                    }
                    _ => Err(Error::TypeError("arrow type expected".to_string())),
                }
            }
            Self::Let(x, t1, t2) => {
                let ty1 = t1.type_of_walk(ctx)?;
                ctx.with_linear_binding(x.clone(), Binding::Var(ty1), |ctx| t2.type_of_walk(ctx))
            }
        }
    }

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        let old_ctx = ctx.clone();

        self.type_of_walk(ctx)
            .and_then(|ty| {
                ctx.check_unused_lin()?;
                Ok(ty)
            })
            .inspect_err(|_| {
                *ctx = old_ctx;
            })
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
