use crate::syntax::{
    Binding, Context, DeBruijnBinding, DeBruijnTerm, DeBruijnTy, Kind, Term, Ty, Variance,
};
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

    fn compute(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(ty1, ty2) => match ty1.as_ref() {
                Self::Abs(_, _, ty) => ty.subst_top(ty2),
                _ => Ok(Self::app(ty1.compute(ctx)?, ty2.clone())),
            },
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TyAbb(ty, _)) => Ok(ty),
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
            (Self::Top, Self::Top)
            | (Self::Bool, Self::Bool)
            | (Self::Nat, Self::Nat)
            | (Self::String, Self::String)
            | (Self::Float, Self::Float)
            | (Self::Unit, Self::Unit) => true,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Abs(x, kn, ty), Self::Abs(_, kn_, ty_)) => {
                kn == kn_ && ctx.with_name(x.clone(), |ctx| ty.eqv(ty_, ctx))
            }
            (Self::App(ty1, ty2), Self::App(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                fields.len() == fields_.len()
                    && fields.iter().all(|(l, var, ty)| {
                        fields_
                            .iter()
                            .any(|(l_, var_, ty_)| l == l_ && var == var_ && ty.eqv(ty_, ctx))
                    })
            }
            (Self::Some(x, ty1, ty2), Self::Some(_, kn_, ty_)) => {
                ty1 == kn_ && ctx.with_name(x.clone(), |ctx| ty2.eqv(ty_, ctx))
            }
            (Self::Id(b1), Self::Id(b2)) => b1 == b2,
            (Self::Var(i), Self::Var(j)) => i == j,
            (Self::All(x, ty1, ty2), Self::All(_, kn_, ty_)) => {
                ty1 == kn_ && ctx.with_name(x.clone(), |ctx| ty2.eqv(ty_, ctx))
            }
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
                Binding::TyAbb(_, Some(kn)) => Ok(kn),
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
            Self::Record(fields) => fields.iter().try_fold(Kind::star(), |acc, (_, _, ty)| {
                if matches!(ty.kind_of(ctx)?.as_ref(), Kind::Star) {
                    Ok(acc)
                } else {
                    Err(Error::KindError("star kind expected".to_string()))
                }
            }),
            Self::Some(x, ty1, ty2) => {
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
            (Self::Record(fields), Self::Record(fields_)) => {
                fields_.iter().all(|(l_, var_, ty_)| {
                    fields.iter().any(|(l, var, ty)| {
                        l_ == l
                            && match var_ {
                                Variance::Covariant => ty.subtype(ty_, ctx),
                                Variance::Invariant => var == var_ && ty.eqv(ty_, ctx),
                            }
                    })
                })
            }
            (Self::Some(x, ty1, ty2), Self::Some(_, ty1_, ty2_)) => {
                ty1.subtype(ty1_, ctx)
                    && ty1_.subtype(ty1, ctx)
                    && ctx.with_binding(x.clone(), Binding::TyVar(ty1_.clone()), |ctx| {
                        ty2.subtype(ty2_, ctx)
                    })
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

    fn join(self: &Rc<Self>, other: &Rc<Self>, ctx: &mut Context) -> Rc<Self> {
        if self.subtype(other, ctx) {
            return other.clone();
        }
        if other.subtype(self, ctx) {
            return self.clone();
        }

        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::All(x, ty1, ty2), Self::All(_, ty1_, ty2_)) => {
                if ty1.subtype(ty1_, ctx) && ty1_.subtype(ty1, ctx) {
                    Self::all(
                        x.clone(),
                        ty1.clone(),
                        ctx.with_binding(x.clone(), Binding::TyVar(ty1.clone()), |ctx| {
                            ty2.join(ty2_, ctx)
                        }),
                    )
                } else {
                    Self::top()
                }
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                let common_fields = fields
                    .iter()
                    .filter_map(|(l, var, ty)| {
                        fields_
                            .iter()
                            .find(|(l_, _, _)| l == l_)
                            .map(|(_, var_, ty_)| {
                                let v = match (var, var_) {
                                    (Variance::Covariant, Variance::Covariant) => {
                                        Variance::Covariant
                                    }
                                    (Variance::Covariant, Variance::Invariant)
                                    | (Variance::Invariant, Variance::Covariant) => {
                                        Variance::Invariant
                                    }
                                    (Variance::Invariant, Variance::Invariant) => {
                                        if ty.eqv(ty_, ctx) {
                                            Variance::Invariant
                                        } else {
                                            Variance::Covariant
                                        }
                                    }
                                };
                                (l.clone(), v, ty.join(ty_, ctx))
                            })
                    })
                    .collect::<Vec<_>>();
                Self::record(common_fields)
            }
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => ty1
                .meet(ty1_, ctx)
                .map_or_else(Self::top, |ty11| Self::arr(ty11, ty2.join(ty2_, ctx))),
            _ => Self::top(),
        }
    }

    fn meet(self: &Rc<Self>, other: &Rc<Self>, ctx: &mut Context) -> Option<Rc<Self>> {
        if self.subtype(other, ctx) {
            return Some(self.clone());
        }
        if other.subtype(self, ctx) {
            return Some(other.clone());
        }

        let self_ = self.simplify(ctx);
        let other_ = other.simplify(ctx);
        match (self_.as_ref(), other_.as_ref()) {
            (Self::All(x, ty1, ty2), Self::All(_, ty1_, ty2_)) => {
                if ty1.subtype(ty1_, ctx) && ty1_.subtype(ty1, ctx) {
                    Some(Self::all(
                        x.clone(),
                        ty1.clone(),
                        ctx.with_binding(x.clone(), Binding::TyVar(ty1.clone()), |ctx| {
                            ty2.meet(ty2_, ctx)
                        })?,
                    ))
                } else {
                    None
                }
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                let common_fields = fields
                    .iter()
                    .filter_map(|(l, var, ty)| {
                        fields_
                            .iter()
                            .find(|(l_, _, _)| l == l_)
                            .map(|(_, var_, ty_)| match (var, var_) {
                                (Variance::Covariant, Variance::Covariant) => {
                                    Some((l.clone(), Variance::Covariant, ty.meet(ty_, ctx)?))
                                }
                                (Variance::Covariant, Variance::Invariant)
                                | (Variance::Invariant, Variance::Covariant) => {
                                    Some((l.clone(), Variance::Invariant, ty.meet(ty_, ctx)?))
                                }
                                (Variance::Invariant, Variance::Invariant) => {
                                    if ty.eqv(ty_, ctx) {
                                        Some((l.clone(), Variance::Invariant, ty.clone()))
                                    } else {
                                        None
                                    }
                                }
                            })
                    })
                    .collect::<Option<Vec<_>>>()?;

                let all_fields = common_fields
                    .into_iter()
                    .chain(
                        fields
                            .iter()
                            .filter(|(l, _, _)| !fields_.iter().any(|(l_, _, _)| l == l_))
                            .cloned(),
                    )
                    .chain(
                        fields_
                            .iter()
                            .filter(|(l, _, _)| !fields.iter().any(|(l_, _, _)| l == l_))
                            .cloned(),
                    )
                    .collect::<Vec<_>>();
                Some(Self::record(all_fields))
            }
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                Some(Self::arr(ty1.join(ty1_, ctx), ty2.meet(ty2_, ctx)?))
            }
            _ => None,
        }
    }
}

impl Ty {
    pub fn kind_of(&self, ctx: &mut Context) -> Result<Rc<Kind>> {
        self.to_de_bruijn(ctx)?.kind_of(ctx)
    }
}

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::String(_)
            | Self::TAbs(_, _, _)
            | Self::True
            | Self::False
            | Self::Abs(_, _, _)
            | Self::Unit
            | Self::Float(_) => true,
            Self::Pack(_, t, _) => t.is_val(_ctx),
            Self::Record(fields) => fields.iter().all(|(_, _, t)| t.is_val(_ctx)),
            t => t.is_numeric_val(),
        }
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
            Self::Ascribe(t, ty) => {
                if t.is_val(ctx) {
                    Ok(t.clone())
                } else {
                    Ok(Self::ascribe(t.eval1(ctx)?, ty.clone()))
                }
            }
            Self::TApp(t, ty) => match t.as_ref() {
                Self::TAbs(_, _, t) => t.subst_top_ty(ty),
                _ => Ok(Self::t_app(t.eval1(ctx)?, ty.clone())),
            },
            Self::Record(fields) => {
                if let Some(i) = fields.iter().position(|(_, _, t)| !t.is_val(ctx)) {
                    let mut fields_ = fields.clone();
                    fields_[i].2 = fields[i].2.eval1(ctx)?;
                    Ok(Self::record(fields_))
                } else {
                    Err(Error::NoRuleApplies)
                }
            }
            Self::Proj(t, l) => match t.as_ref() {
                Self::Record(fields) if t.is_val(ctx) => {
                    if let Some((_, _, t)) = fields.iter().find(|(l_, _, _)| l_ == l) {
                        Ok(t.clone())
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::proj(t.eval1(ctx)?, l.clone())),
            },
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
            Self::Update(t1, l, t2) => match t1.as_ref() {
                Self::Record(fields) if t1.is_val(ctx) => {
                    if t2.is_val(ctx) {
                        fields.iter().position(|(l_, _, _)| l_ == l).map_or(
                            Err(Error::NoRuleApplies),
                            |i| {
                                let mut fields_ = fields.clone();
                                fields_[i].2 = t2.clone();
                                Ok(Self::record(fields_))
                            },
                        )
                    } else {
                        Ok(Self::update(t1.clone(), l.clone(), t2.eval1(ctx)?))
                    }
                }
                _ => Ok(Self::update(t1.eval1(ctx)?, l.clone(), t2.clone())),
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
            Self::Ascribe(t, ty) => {
                if matches!(ty.kind_of(ctx)?.as_ref(), Kind::Star) {
                    if t.type_of(ctx)?.subtype(ty, ctx) {
                        Ok(ty.clone())
                    } else {
                        Err(Error::TypeError(
                            "body of as-term does not have the expected type".to_string(),
                        ))
                    }
                } else {
                    Err(Error::TypeError("star kind expected".to_string()))
                }
            }
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
            Self::String(_) => Ok(Ty::string()),
            Self::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(l, var, t)| t.type_of(ctx).map(|ty| (l.clone(), *var, ty)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Ty::record(fields))
            }
            Self::Proj(t, l) => match t.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::Record(fields) => fields
                    .iter()
                    .find(|(l_, _, _)| l_ == l)
                    .map(|(_, _, ty)| ty.clone())
                    .ok_or_else(|| Error::TypeError(format!("label {l} not found"))),
                _ => Err(Error::TypeError("expected record type".to_string())),
            },
            Self::Pack(ty1, t2, ty3) => {
                if matches!(ty3.kind_of(ctx)?.as_ref(), Kind::Star) {
                    match ty3.simplify(ctx).as_ref() {
                        Ty::Some(_, ty1_, ty2_) => {
                            if ty1.subtype(ty1_, ctx) {
                                if t2.type_of(ctx)?.subtype(ty2_.subst_top(ty1)?.as_ref(), ctx) {
                                    Ok(ty3.clone())
                                } else {
                                    Err(Error::TypeError("doesn't match declared type".to_string()))
                                }
                            } else {
                                Err(Error::TypeError(
                                    "hidden type not a subtype of bound".to_string(),
                                ))
                            }
                        }
                        _ => Err(Error::TypeError("existential type expected".to_string())),
                    }
                } else {
                    Err(Error::TypeError("star kind expected".to_string()))
                }
            }
            Self::Unpack(x1, x2, t1, t2) => match t1.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::Some(_, ty1, ty2) => {
                    // This follows the reference OCaml implementation,
                    // but it seems to be incorrect.
                    // We should ensure that the resulting type does not mention x1.
                    // See chapter 28.7 in the book Types and Programming Languages.
                    ctx.with_binding(x1.clone(), Binding::TyVar(ty1.clone()), |ctx| {
                        ctx.with_binding(x2.clone(), Binding::Var(ty2.clone()), |ctx| {
                            t2.type_of(ctx)?.shift(-2)
                        })
                    })
                }
                _ => Err(Error::TypeError("existential type expected".to_string())),
            },
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
            Self::Zero => Ok(Ty::nat()),
            Self::Succ(t) => {
                if t.type_of(ctx)?.subtype(&Ty::Nat, ctx) {
                    Ok(Ty::nat())
                } else {
                    Err(Error::TypeError(
                        "argument of succ is not a number".to_string(),
                    ))
                }
            }
            Self::Pred(t) => {
                if t.type_of(ctx)?.subtype(&Ty::Nat, ctx) {
                    Ok(Ty::nat())
                } else {
                    Err(Error::TypeError(
                        "argument of pred is not a number".to_string(),
                    ))
                }
            }
            Self::IsZero(t) => {
                if t.type_of(ctx)?.subtype(&Ty::Nat, ctx) {
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
                if t1.type_of(ctx)?.subtype(&Ty::Float, ctx)
                    && t2.type_of(ctx)?.subtype(&Ty::Float, ctx)
                {
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
            Self::Fix(t) => match t.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::Arr(ty1, ty2) => {
                    if ty1.subtype(ty2, ctx) {
                        Ok(ty1.clone())
                    } else {
                        Err(Error::TypeError(
                            "result of body not compatible with domain".to_string(),
                        ))
                    }
                }
                _ => Err(Error::TypeError("arrow type expected".to_string())),
            },
            Self::Update(t1, l, t2) => match t1.type_of(ctx)?.lcst(ctx).as_ref() {
                Ty::Record(fields) => fields
                    .iter()
                    .find(|(l_, _, _)| l_ == l)
                    .ok_or_else(|| Error::TypeError(format!("label {l} not found")))
                    .and_then(|(_, var, ty)| {
                        if *var != Variance::Invariant {
                            Err(Error::TypeError("field not invariant".to_string()))
                        } else if t2.type_of(ctx)?.subtype(ty, ctx) {
                            t1.type_of(ctx)
                        } else {
                            Err(Error::TypeError(
                                "type of new field value doesn't match".to_string(),
                            ))
                        }
                    }),
                _ => Err(Error::TypeError("expected record type".to_string())),
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
            Self::TyAbb(ty, kn) => {
                let kn_ = ty.kind_of(ctx)?;
                if let Some(kn) = kn {
                    if kn_ == *kn {
                        Ok(Self::TyAbb(ty.clone(), Some(kn.clone())))
                    } else {
                        Err(Error::TypeMismatch)
                    }
                } else {
                    Ok(Self::TyAbb(ty.clone(), Some(kn_)))
                }
            }
            _ => Ok(self.clone()),
        }
    }
}
