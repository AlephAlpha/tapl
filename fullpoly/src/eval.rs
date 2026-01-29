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

    fn eqv(&self, other: &Self, ctx: &mut Context) -> bool {
        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::Bool, Self::Bool)
            | (Self::Nat, Self::Nat)
            | (Self::Unit, Self::Unit)
            | (Self::String, Self::String)
            | (Self::Float, Self::Float) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                fields.len() == fields_.len()
                    && fields
                        .iter()
                        .all(|(l, ty)| fields_.iter().any(|(l_, ty_)| l == l_ && ty.eqv(ty_, ctx)))
            }
            (Self::Some(x, ty), Self::Some(_, ty_)) => {
                ctx.with_name(x.clone(), |ctx| ty.eqv(ty_, ctx))
            }
            (Self::Id(b1), Self::Id(b2)) => b1 == b2,
            (Self::Var(i), Self::Var(j)) => i == j,
            (Self::All(x, ty), Self::All(_, ty_)) => {
                ctx.with_name(x.clone(), |ctx| ty.eqv(ty_, ctx))
            }
            _ => false,
        }
    }
}

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::String(_)
            | Self::TAbs(_, _)
            | Self::True
            | Self::False
            | Self::Abs(_, _, _)
            | Self::Unit
            | Self::Float(_) => true,
            Self::Pack(_, t, _) => t.is_val(_ctx),
            Self::Record(fields) => fields.iter().all(|(_, t)| t.is_val(_ctx)),
            t => t.is_numeric_val(),
        }
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::Ascribe(t, ty) => {
                if t.is_val(ctx) {
                    Ok(t.clone())
                } else {
                    Ok(Self::ascribe(t.eval1(ctx)?, ty.clone()))
                }
            }
            Self::Unpack(x1, x2, t1, t2) => match t1.as_ref() {
                Self::Pack(ty11, t12, _) if t12.is_val(ctx) => {
                    t2.subst_top(t12.shift(1)?.as_ref())?.subst_top_ty(ty11)
                }
                _ => Ok(Self::unpack(
                    x1.clone(),
                    x2.clone(),
                    t1.eval1(ctx)?,
                    t2.clone(),
                )),
            },
            Self::Pack(ty1, t2, ty3) => Ok(Self::pack(ty1.clone(), t2.eval1(ctx)?, ty3.clone())),
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
                Self::TAbs(_, t) => t.subst_top_ty(ty),
                _ => Ok(Self::t_app(t.eval1(ctx)?, ty.clone())),
            },
            Self::Record(fields) => {
                if let Some(i) = fields.iter().position(|(_, t)| !t.is_val(ctx)) {
                    let mut fields_ = fields.clone();
                    fields_[i].1 = fields[i].1.eval1(ctx)?;
                    Ok(Self::record(fields_))
                } else {
                    Err(Error::NoRuleApplies)
                }
            }
            Self::Proj(t, l) => match t.as_ref() {
                Self::Record(fields) if t.is_val(ctx) => {
                    if let Some((_, t)) = fields.iter().find(|(l_, _)| l_ == l) {
                        Ok(t.clone())
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::proj(t.eval1(ctx)?, l.clone())),
            },
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Succ(t) => Ok(Self::succ(t.eval1(ctx)?)),
            Self::Pred(t) => match t.as_ref() {
                Self::Zero => Ok(Self::zero()),
                Self::Succ(t) if t.is_numeric_val() => Ok(t.clone()),
                _ => Ok(Self::pred(t.eval1(ctx)?)),
            },
            Self::IsZero(t) => match t.as_ref() {
                Self::Zero => Ok(Self::true_()),
                Self::Succ(t) if t.is_numeric_val() => Ok(Self::false_()),
                _ => Ok(Self::is_zero(t.eval1(ctx)?)),
            },
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TermAbb(t, _)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
            Self::TimesFloat(t1, t2) => match t1.as_ref() {
                Self::Float(f1) => match t2.as_ref() {
                    Self::Float(f2) => Ok(Self::float(f1 * f2)),
                    _ => Ok(Self::times_float(t1.clone(), t2.eval1(ctx)?)),
                },
                _ => Ok(Self::times_float(t1.eval1(ctx)?, t2.clone())),
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    t2.subst_top(t1)
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx)?, t2.clone()))
                }
            }
            Self::Fix(t) => match t.as_ref() {
                Self::Abs(_, _, t1) => t1.subst_top(&Self::fix(t.clone())),
                t if t.is_val(ctx) => Err(Error::NoRuleApplies),
                _ => Ok(Self::fix(t.eval1(ctx)?)),
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
            Self::Ascribe(t, ty) => {
                if t.type_of(ctx)?.eqv(ty, ctx) {
                    Ok(ty.clone())
                } else {
                    Err(Error::TypeError(
                        "body of as-term does not have the expected type".to_string(),
                    ))
                }
            }
            Self::String(_) => Ok(Ty::string()),
            Self::Pack(ty1, t2, ty3) => match ty3.simplify(ctx).as_ref() {
                Ty::Some(_, ty) => {
                    if t2.type_of(ctx)?.eqv(ty.subst_top(ty1)?.as_ref(), ctx) {
                        Ok(ty3.clone())
                    } else {
                        Err(Error::TypeError("doesn't match declared type".to_string()))
                    }
                }
                _ => Err(Error::TypeError("existential type expected".to_string())),
            },
            Self::Unpack(x1, x2, t1, t2) => match t1.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Some(_, ty1) => ctx.with_binding(x1.clone(), Binding::TyVar, |ctx| {
                    ctx.with_binding(x2.clone(), Binding::Var(ty1.clone()), |ctx| {
                        t2.type_of(ctx)?.shift(-2)
                    })
                }),
                _ => Err(Error::TypeError("existential type expected".to_string())),
            },
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
                    Ok(Ty::arr(ty1.clone(), t2.type_of(ctx)?.shift(-1)?))
                })
            }
            Self::App(t1, t2) => match t1.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Arr(ty11, ty12) => {
                    if t2.type_of(ctx)?.eqv(ty11, ctx) {
                        Ok(ty12.clone())
                    } else {
                        Err(Error::TypeError("parameter type mismatch".to_string()))
                    }
                }
                _ => Err(Error::TypeError("arrow type expected".to_string())),
            },
            Self::TAbs(x, t) => ctx.with_binding(x.clone(), Binding::TyVar, |ctx| {
                Ok(Ty::all(x.clone(), t.type_of(ctx)?))
            }),
            Self::TApp(t, ty) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::All(_, ty_) => ty_.subst_top(ty),
                _ => Err(Error::TypeError("universal type expected".to_string())),
            },
            Self::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(l, t)| t.type_of(ctx).map(|ty| (l.clone(), ty)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Ty::record(fields))
            }
            Self::Proj(t, l) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Record(fields) => fields
                    .iter()
                    .find(|(l_, _)| l_ == l)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| Error::TypeError(format!("label {l} not found"))),
                _ => Err(Error::TypeError("expected record type".to_string())),
            },
            Self::True | Self::False => Ok(Ty::bool()),
            Self::If(t1, t2, t3) => {
                if t1.type_of(ctx)?.eqv(&Ty::Bool, ctx) {
                    let ty2 = t2.type_of(ctx)?;
                    if t3.type_of(ctx)?.eqv(&ty2, ctx) {
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
            Self::Zero => Ok(Ty::nat()),
            Self::Succ(t) => {
                if t.type_of(ctx)?.eqv(&Ty::Nat, ctx) {
                    Ok(Ty::nat())
                } else {
                    Err(Error::TypeError(
                        "argument of succ is not a number".to_string(),
                    ))
                }
            }
            Self::Pred(t) => {
                if t.type_of(ctx)?.eqv(&Ty::Nat, ctx) {
                    Ok(Ty::nat())
                } else {
                    Err(Error::TypeError(
                        "argument of pred is not a number".to_string(),
                    ))
                }
            }
            Self::IsZero(t) => {
                if t.type_of(ctx)?.eqv(&Ty::Nat, ctx) {
                    Ok(Ty::bool())
                } else {
                    Err(Error::TypeError(
                        "argument of iszero is not a number".to_string(),
                    ))
                }
            }
            Self::Unit => Ok(Ty::unit()),
            Self::Float(_) => Ok(Ty::float()),
            Self::TimesFloat(t1, t2) => {
                if t1.type_of(ctx)?.eqv(&Ty::Float, ctx) && t2.type_of(ctx)?.eqv(&Ty::Float, ctx) {
                    Ok(Ty::float())
                } else {
                    Err(Error::TypeError(
                        "argument of timesfloat is not a number".to_string(),
                    ))
                }
            }
            Self::Let(x, t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                ctx.with_binding(x.clone(), Binding::Var(ty1), |ctx| {
                    t2.type_of(ctx)?.shift(-1)
                })
            }
            Self::Inert(ty) => Ok(ty.clone()),
            Self::Fix(t) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Arr(ty1, ty2) => {
                    if ty1.eqv(ty2, ctx) {
                        Ok(ty1.clone())
                    } else {
                        Err(Error::TypeError(
                            "result of body not compatible with domain".to_string(),
                        ))
                    }
                }
                _ => Err(Error::TypeError("arrow type expected".to_string())),
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
    fn eval(&self, ctx: &Context) -> Self {
        match self {
            Self::TermAbb(t, ty) => Self::TermAbb(t.eval(ctx), ty.clone()),
            _ => self.clone(),
        }
    }

    fn check(&self, ctx: &mut Context) -> Result<Self> {
        match self {
            Self::TermAbb(t, ty) => {
                let ty_ = t.type_of(ctx)?;
                if let Some(ty) = ty {
                    if ty_.eqv(ty, ctx) {
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

    pub fn check_and_eval(&self, ctx: &mut Context) -> Result<Self> {
        self.check(ctx).map(|b| b.eval(ctx))
    }
}

impl Binding {
    pub fn check_and_eval(&self, ctx: &mut Context) -> Result<DeBruijnBinding> {
        self.to_de_bruijn(ctx)?.check_and_eval(ctx)
    }
}
