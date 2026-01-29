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
            (Self::Bot, Self::Bot)
            | (Self::String, Self::String)
            | (Self::Float, Self::Float)
            | (Self::Unit, Self::Unit)
            | (Self::Top, Self::Top)
            | (Self::Bool, Self::Bool)
            | (Self::Nat, Self::Nat) => true,
            (Self::Id(b), Self::Id(b_)) => b == b_,
            (Self::Arr(ty1, ty2), Self::Arr(ty1_, ty2_)) => {
                ty1.eqv(ty1_, ctx) && ty2.eqv(ty2_, ctx)
            }
            (Self::Ref(ty), Self::Ref(ty_)) => ty.eqv(ty_, ctx),
            (Self::Source(ty), Self::Source(ty_)) => ty.eqv(ty_, ctx),
            (Self::Sink(ty), Self::Sink(ty_)) => ty.eqv(ty_, ctx),
            (Self::Variant(fields), Self::Variant(fields_)) => {
                fields.len() == fields_.len()
                    && fields
                        .iter()
                        .zip(fields_)
                        .all(|((l, ty), (l_, ty_))| l == l_ && ty.eqv(ty_, ctx))
            }
            (Self::Record(fields), Self::Record(fields_)) => {
                fields.len() == fields_.len()
                    && fields
                        .iter()
                        .all(|(l, ty)| fields_.iter().any(|(l_, ty_)| l == l_ && ty.eqv(ty_, ctx)))
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
            (Self::Ref(ty), Self::Ref(ty_)) => ty.subtype(ty_, ctx) && ty_.subtype(ty, ctx),
            (Self::Ref(ty) | Self::Source(ty), Self::Source(ty_)) => ty.subtype(ty_, ctx),
            (Self::Ref(ty) | Self::Sink(ty), Self::Sink(ty_)) => ty_.subtype(ty, ctx),
            (Self::Variant(fields), Self::Variant(fields_)) => fields.iter().all(|(l, ty)| {
                fields_
                    .iter()
                    .any(|(l_, ty_)| l == l_ && ty.subtype(ty_, ctx))
            }),
            (Self::Record(fields), Self::Record(fields_)) => fields_
                .iter()
                .all(|(l_, ty_)| fields.iter().any(|(l, ty)| l_ == l && ty.subtype(ty_, ctx))),
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
            (Self::Ref(ty), Self::Ref(ty_)) => {
                if ty.subtype(ty_, ctx) && ty_.subtype(ty, ctx) {
                    ty.clone()
                } else {
                    // Warning: this is incomplete...
                    Self::source(ty.join(ty_, ctx))
                }
            }
            (Self::Source(ty) | Self::Ref(ty), Self::Source(ty_))
            | (Self::Source(ty), Self::Ref(ty_)) => Self::source(ty.join(ty_, ctx)),
            (Self::Sink(ty) | Self::Ref(ty), Self::Sink(ty_))
            | (Self::Sink(ty), Self::Ref(ty_)) => Self::sink(ty.meet(ty_, ctx)),
            (Self::Record(fields), Self::Record(fields_)) => {
                let common_fields = fields
                    .iter()
                    .filter_map(|(l, ty)| {
                        fields_
                            .iter()
                            .find(|(l_, _)| l == l_)
                            .map(|(_, ty_)| (l.clone(), ty.join(ty_, ctx)))
                    })
                    .collect::<Vec<_>>();
                Self::record(common_fields)
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
            (Self::Ref(ty), Self::Ref(ty_)) => {
                if ty.subtype(ty_, ctx) && ty_.subtype(ty, ctx) {
                    ty.clone()
                } else {
                    // Warning: this is incomplete...
                    Self::source(ty.meet(ty_, ctx))
                }
            }
            (Self::Source(ty) | Self::Ref(ty), Self::Source(ty_))
            | (Self::Source(ty), Self::Ref(ty_)) => Self::source(ty.meet(ty_, ctx)),
            (Self::Sink(ty) | Self::Ref(ty), Self::Sink(ty_))
            | (Self::Sink(ty), Self::Ref(ty_)) => Self::sink(ty.join(ty_, ctx)),
            (Self::Record(fields), Self::Record(fields_)) => {
                let all_fields = fields
                    .iter()
                    .filter_map(|(l, ty)| {
                        fields_
                            .iter()
                            .find(|(l_, _)| l == l_)
                            .map(|(_, ty_)| (l.clone(), ty.meet(ty_, ctx)))
                    })
                    .chain(
                        fields
                            .iter()
                            .filter(|(l, _)| !fields_.iter().any(|(l_, _)| l == l_))
                            .cloned(),
                    )
                    .chain(
                        fields_
                            .iter()
                            .filter(|(l, _)| !fields.iter().any(|(l_, _)| l == l_))
                            .cloned(),
                    )
                    .collect::<Vec<_>>();
                Self::record(all_fields)
            }
            _ => Self::bot(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Store {
    store: Vec<Rc<DeBruijnTerm>>,
}

impl Store {
    pub const fn new() -> Self {
        Self { store: Vec::new() }
    }

    const fn len(&self) -> usize {
        self.store.len()
    }

    fn extend(&mut self, v: Rc<DeBruijnTerm>) -> usize {
        let l = self.len();
        self.store.push(v);
        l
    }

    fn loop_up(&self, l: usize) -> Option<Rc<DeBruijnTerm>> {
        self.store.get(l).cloned()
    }

    fn update(&mut self, l: usize, v: Rc<DeBruijnTerm>) -> Option<()> {
        self.store.get_mut(l).map(|t| *t = v)
    }

    pub fn shift(&mut self, d: isize) -> Result<()> {
        for t in self.store.iter_mut() {
            *t = t.shift(d)?;
        }
        Ok(())
    }
}

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::String(_)
            | Self::Unit
            | Self::Loc(_)
            | Self::True
            | Self::False
            | Self::Float(_)
            | Self::Abs(_, _, _) => true,
            Self::Tag(_, t, _) => t.is_val(_ctx),
            Self::Record(fields) => fields.iter().all(|(_, t)| t.is_val(_ctx)),
            t => t.is_numeric_val(),
        }
    }

    pub fn eval1(&self, ctx: &Context, store: &mut Store) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, t) if t2.is_val(ctx) => t.subst_top(t2),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx, store)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx, store)?, t2.clone()))
                    }
                }
            },
            Self::Ascribe(t, ty) => {
                if t.is_val(ctx) {
                    Ok(t.clone())
                } else {
                    Ok(Self::ascribe(t.eval1(ctx, store)?, ty.clone()))
                }
            }
            Self::Ref(t) => {
                if t.is_val(ctx) {
                    let l = store.extend(t.clone());
                    Ok(Self::loc(l))
                } else {
                    Ok(Self::ref_(t.eval1(ctx, store)?))
                }
            }
            Self::Deref(t) => match t.as_ref() {
                Self::Loc(l) => store.loop_up(*l).ok_or(Error::NoRuleApplies),
                _ => Ok(Self::deref(t.eval1(ctx, store)?)),
            },
            Self::Assign(t1, t2) => match t1.as_ref() {
                Self::Loc(l) => {
                    if t2.is_val(ctx) {
                        store.update(*l, t2.clone());
                        Ok(Self::unit())
                    } else {
                        Ok(Self::assign(t1.clone(), t2.eval1(ctx, store)?))
                    }
                }
                _ => Ok(Self::assign(t1.eval1(ctx, store)?, t2.clone())),
            },
            Self::Tag(l, t, ty) => Ok(Self::tag(l.clone(), t.eval1(ctx, store)?, ty.clone())),
            Self::Case(t, cases) => match t.as_ref() {
                Self::Tag(l, t1, _) if t1.is_val(ctx) => {
                    if let Some((_, _, t2)) = cases.iter().find(|(l_, _, _)| l_ == l) {
                        t2.subst_top(t1)
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::case(t.eval1(ctx, store)?, cases.clone())),
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    t2.subst_top(t1)
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx, store)?, t2.clone()))
                }
            }
            Self::Fix(t) => match t.as_ref() {
                Self::Abs(_, _, t1) => t1.subst_top(&Self::fix(t.clone())),
                t if t.is_val(ctx) => Err(Error::NoRuleApplies),
                _ => Ok(Self::fix(t.eval1(ctx, store)?)),
            },
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx, store)?, t2.clone(), t3.clone())),
            },
            Self::TimesFloat(t1, t2) => match t1.as_ref() {
                Self::Float(f1) => match t2.as_ref() {
                    Self::Float(f2) => Ok(Self::float(f1 * f2)),
                    _ => Ok(Self::times_float(t1.clone(), t2.eval1(ctx, store)?)),
                },
                _ => Ok(Self::times_float(t1.eval1(ctx, store)?, t2.clone())),
            },
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TermAbb(t, _)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
            Self::Record(fields) => {
                if let Some(i) = fields.iter().position(|(_, t)| !t.is_val(ctx)) {
                    let mut fields_ = fields.clone();
                    fields_[i].1 = fields[i].1.eval1(ctx, store)?;
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
                _ => Ok(Self::proj(t.eval1(ctx, store)?, l.clone())),
            },
            Self::Succ(t) => Ok(Self::succ(t.eval1(ctx, store)?)),
            Self::Pred(t) => match t.as_ref() {
                Self::Zero => Ok(Self::zero()),
                Self::Succ(t) if t.is_numeric_val() => Ok(t.clone()),
                _ => Ok(Self::pred(t.eval1(ctx, store)?)),
            },
            Self::IsZero(t) => match t.as_ref() {
                Self::Zero => Ok(Self::true_()),
                Self::Succ(t) if t.is_numeric_val() => Ok(Self::false_()),
                _ => Ok(Self::is_zero(t.eval1(ctx, store)?)),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    pub fn eval(self: &Rc<Self>, ctx: &Context, store: &mut Store) -> Rc<Self> {
        let mut t = Rc::clone(self);
        while let Ok(t_) = t.eval1(ctx, store) {
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
                    Ok(Ty::arr(ty1.clone(), ty2.shift(-1)?))
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
            Self::Ascribe(t, ty) => {
                if t.type_of(ctx)?.subtype(ty, ctx) {
                    Ok(ty.clone())
                } else {
                    Err(Error::TypeError(
                        "body of as-term does not have the expected type".to_string(),
                    ))
                }
            }
            Self::String(_) => Ok(Ty::string()),
            Self::Unit => Ok(Ty::unit()),
            Self::Ref(t) => Ok(Ty::ref_(t.type_of(ctx)?)),
            Self::Loc(_) => Err(Error::TypeError(
                "locations are not supposed to occur in source programs".to_string(),
            )),
            Self::Let(x, t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                ctx.with_binding(x.clone(), Binding::Var(ty1), |ctx| {
                    t2.type_of(ctx)?.shift(-1)
                })
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
            Self::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(l, t)| Ok((l.clone(), t.type_of(ctx)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Ty::record(fields))
            }
            Self::Proj(t, l) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Record(fields) => fields
                    .iter()
                    .find(|(l_, _)| l_ == l)
                    .map(|(_, ty)| ty.clone())
                    .ok_or_else(|| Error::TypeError(format!("label {l} not found"))),
                Ty::Bot => Ok(Ty::bot()),
                _ => Err(Error::TypeError("expected record type".to_string())),
            },
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

                    case_tys
                        .iter()
                        .try_fold(Ty::bot(), |acc, ty| Ok(acc.join(ty, ctx)))
                }
                Ty::Bot => Ok(Ty::bot()),
                _ => Err(Error::TypeError("expected variant type".to_string())),
            },
            Self::Tag(l, t, ty) => match ty.simplify(ctx).as_ref() {
                Ty::Variant(fields) => {
                    if let Some((_, ty_)) = fields.iter().find(|(l_, _)| l_ == l) {
                        if t.type_of(ctx)?.subtype(ty_, ctx) {
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
            Self::Fix(t) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Arr(ty1, ty2) => {
                    if ty1.subtype(ty2, ctx) {
                        Ok(ty1.clone())
                    } else {
                        Err(Error::TypeError(
                            "result of body not compatible with domain".to_string(),
                        ))
                    }
                }
                Ty::Bot => Ok(Ty::bot()),
                _ => Err(Error::TypeError("arrow type expected".to_string())),
            },
            Self::Deref(t) => match t.type_of(ctx)?.simplify(ctx).as_ref() {
                Ty::Ref(ty) | Ty::Source(ty) => Ok(ty.clone()),
                Ty::Bot => Ok(Ty::bot()),
                _ => Err(Error::TypeError(
                    "argument of ! is not a Ref or Source".to_string(),
                )),
            },
            Self::Assign(t1, t2) => {
                let ty1 = t1.type_of(ctx)?;
                let ty2 = t2.type_of(ctx)?;
                match ty1.simplify(ctx).as_ref() {
                    Ty::Ref(ty) | Ty::Sink(ty) => {
                        if ty2.subtype(ty, ctx) {
                            Ok(Ty::unit())
                        } else {
                            Err(Error::TypeError(
                                "arguments of := are incompatible".to_string(),
                            ))
                        }
                    }
                    Ty::Bot => Ok(Ty::bot()),
                    _ => Err(Error::TypeError(
                        "argument of ! is not a Ref or Sink".to_string(),
                    )),
                }
            }
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
            Self::Inert(ty) => Ok(ty.clone()),
        }
    }
}

impl Term {
    pub fn eval1(&self, ctx: &mut Context, store: &mut Store) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval1(ctx, store)?.to_named(ctx)
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context, store: &mut Store) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval(ctx, store).to_named(ctx)
    }

    pub fn type_of(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        self.to_de_bruijn(ctx)?.type_of(ctx)?.to_named(ctx)
    }
}

impl DeBruijnBinding {
    fn eval(&self, ctx: &Context, store: &mut Store) -> Self {
        match self {
            Self::TermAbb(t, ty) => Self::TermAbb(t.eval(ctx, store), ty.clone()),
            _ => self.clone(),
        }
    }

    fn check(&self, ctx: &mut Context) -> Result<Self> {
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

    pub fn check_and_eval(&self, ctx: &mut Context, store: &mut Store) -> Result<Self> {
        self.check(ctx).map(|b| b.eval(ctx, store))
    }
}

impl Binding {
    pub fn check_and_eval(&self, ctx: &mut Context, store: &mut Store) -> Result<DeBruijnBinding> {
        self.to_de_bruijn(ctx)?.check_and_eval(ctx, store)
    }
}
