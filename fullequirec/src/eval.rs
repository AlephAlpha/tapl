use crate::syntax::{Binding, Context, DeBruijnBinding, DeBruijnTerm, DeBruijnTy, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTy {
    fn compute(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::Rec(_, ty) => ty.subst_top(self),
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

    fn eqv_walk(&self, other: &Self, seen: &mut Vec<(Self, Self)>, ctx: &Context) -> bool {
        if seen.iter().any(|(ty, ty_)| ty == self && ty_ == other) {
            return true;
        }

        seen.push((self.clone(), other.clone()));

        match (self, other) {
            (Self::Unit, Self::Unit)
            | (Self::Float, Self::Float)
            | (Self::String, Self::String)
            | (Self::Bool, Self::Bool)
            | (Self::Nat, Self::Nat) => true,
            (Self::Rec(_, ty), _) => ty
                .subst_top(self)
                .is_ok_and(|ty_| ty_.eqv_walk(other, seen, ctx)),
            (_, Self::Rec(_, ty)) => ty
                .subst_top(other)
                .is_ok_and(|ty_| self.eqv_walk(&ty_, seen, ctx)),
            (Self::Var(i), _) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TyAbb(ty)) => ty.eqv_walk(other, seen, ctx),
                _ => match other {
                    Self::Var(j) => i == j,
                    _ => false,
                },
            },
            (_, Self::Var(j)) => match ctx.get_binding_shifting(*j) {
                Ok(Binding::TyAbb(ty)) => self.eqv_walk(&ty, seen, ctx),
                _ => false,
            },
            (Self::Id(b1), Self::Id(b2)) => b1 == b2,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv_walk(ty1_, seen, ctx) && ty2.eqv_walk(ty2_, seen, ctx)
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                fields.len() == fields_.len()
                    && fields.iter().all(|(l, ty)| {
                        fields_
                            .iter()
                            .any(|(l_, ty_)| l == l_ && ty.eqv_walk(ty_, seen, ctx))
                    })
            }
            (Self::Variant(fields), Self::Variant(fields_)) => {
                fields.len() == fields_.len()
                    && fields
                        .iter()
                        .zip(fields_)
                        .all(|((l, ty), (l_, ty_))| l == l_ && ty.eqv_walk(ty_, seen, ctx))
            }
            _ => false,
        }
    }

    fn eqv(&self, other: &Self, ctx: &Context) -> bool {
        let mut seen = Vec::new();
        self.eqv_walk(other, &mut seen, ctx)
    }
}

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::String(_)
            | Self::Float(_)
            | Self::True
            | Self::False
            | Self::Abs(_, _, _)
            | Self::Unit => true,
            Self::Record(fields) => fields.iter().all(|(_, t)| t.is_val(_ctx)),
            Self::Tag(_, t, _) => t.is_val(_ctx),
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
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TermAbb(t, _)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
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
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    t2.subst_top(t1)
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx)?, t2.clone()))
                }
            }
            Self::Tag(l, t, ty) => Ok(Self::tag(l.clone(), t.eval1(ctx)?, ty.clone())),
            Self::Case(t, cases) => match t.as_ref() {
                Self::Tag(l, t1, _) if t1.is_val(ctx) => {
                    if let Some((_, _, t2)) = cases.iter().find(|(l_, _, _)| l_ == l) {
                        t2.subst_top(t1)
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::case(t.eval1(ctx)?, cases.clone())),
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
                Self::Record(fields) => {
                    if let Some((_, t)) = fields.iter().find(|(l_, _)| l_ == l) {
                        Ok(t.clone())
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::proj(t.eval1(ctx)?, l.clone())),
            },
            Self::Fix(t) => match t.as_ref() {
                Self::Abs(_, _, t1) => t1.subst_top(&Self::fix(t.clone())),
                t if t.is_val(ctx) => Err(Error::NoRuleApplies),
                _ => Ok(Self::fix(t.eval1(ctx)?)),
            },
            Self::TimesFloat(t1, t2) => match t1.as_ref() {
                Self::Float(f1) => match t2.as_ref() {
                    Self::Float(f2) => Ok(Self::float(f1 * f2)),
                    _ => Ok(Self::times_float(t1.clone(), t2.eval1(ctx)?)),
                },
                _ => Ok(Self::times_float(t1.eval1(ctx)?, t2.clone())),
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
            Self::Case(t, cases) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Variant(fields) => {
                    for (l, _) in fields.iter() {
                        if !cases.iter().any(|(l_, _, _)| l_ == l) {
                            return Err(Error::TypeError(format!("label {l} not found")));
                        }
                    }

                    let case_tys = cases
                        .iter()
                        .map(|(l, x, t)| {
                            let ty = fields
                                .iter()
                                .find(|(l_, _)| l_ == l)
                                .map(|(_, ty)| ty.clone())
                                .ok_or_else(|| {
                                    Error::TypeError(format!("label {x} not in type"))
                                })?;
                            ctx.with_binding(x.clone(), Binding::Var(ty), |ctx| {
                                t.type_of(ctx)?.shift(-1)
                            })
                        })
                        .collect::<Result<Vec<_>>>()?;

                    let ty1 = case_tys[0].clone();

                    if case_tys.iter().all(|ty| ty.eqv(&ty1, ctx)) {
                        Ok(ty1)
                    } else {
                        Err(Error::TypeError(
                            "fields do not have the same type".to_string(),
                        ))
                    }
                }
                _ => Err(Error::TypeError("expected variant type".to_string())),
            },
            Self::Tag(l, t, ty) => match ty.simplify(ctx).as_ref() {
                Ty::Variant(fields) => {
                    if let Some((_, ty_)) = fields.iter().find(|(l_, _)| l_ == l) {
                        if t.type_of(ctx)?.eqv(ty_, ctx) {
                            Ok(ty.clone())
                        } else {
                            Err(Error::TypeError(
                                "field does not have expected type".to_string(),
                            ))
                        }
                    } else {
                        Err(Error::TypeError(format!("label {l} not found")))
                    }
                }
                _ => Err(Error::TypeError(
                    "annotation is not a variant type".to_string(),
                )),
            },
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
            Self::Var(i) => match ctx.get_binding_shifting(*i)? {
                Binding::Var(ty) => Ok(ty),
                Binding::TermAbb(_, Some(ty)) => Ok(ty),
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Let(x, t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                ctx.with_binding(x.clone(), Binding::Var(ty1), |ctx| {
                    t2.type_of(ctx)?.shift(-1)
                })
            }
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
            Self::Abs(x, ty1, t2) => {
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let ty2 = t2.type_of(ctx)?;
                    Ok(Ty::arr(ty1.clone(), ty2.shift(-1)?))
                })
            }
            Self::App(t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                let ty2 = t2.type_of(ctx)?;
                match ty1.simplify(ctx).as_ref() {
                    Ty::Arr(ty11, ty12) => {
                        if ty2.eqv(ty11, ctx) {
                            Ok(ty12.clone())
                        } else {
                            Err(Error::TypeError("parameter type mismatch".to_string()))
                        }
                    }
                    _ => Err(Error::TypeError("arrow type expected".to_string())),
                }
            }
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
            Self::Inert(ty) => Ok(ty.clone()),
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
}
