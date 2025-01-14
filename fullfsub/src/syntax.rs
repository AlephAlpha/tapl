use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{error::Result, BindingShift, RcTerm};

pub const KEYWORDS: &[&str] = &[
    "true",
    "false",
    "if",
    "then",
    "else",
    "unit",
    "timesfloat",
    "as",
    "let",
    "in",
    "lambda",
    "fix",
    "inert",
    "succ",
    "pred",
    "iszero",
    "letrec",
    "_",
    "Top",
    "Unit",
    "Float",
    "String",
    "Bool",
    "Nat",
    "Some",
    "All",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty<V = String> {
    Top,
    Var(#[rc_term(into)] V),
    String,
    All(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    Some(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    Arr(Rc<Self>, Rc<Self>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Bool,
    Nat,
    Unit,
    Id(#[rc_term(into)] String),
    Float,
}

pub type DeBruijnTy = Ty<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Ascribe(Rc<Self>, Rc<Ty<V>>),
    String(#[rc_term(into)] String),
    Pack(Rc<Ty<V>>, Rc<Self>, Rc<Ty<V>>),
    Unpack(
        #[rc_term(into)] String,
        #[rc_term(into)] String,
        Rc<Self>,
        Rc<Self>,
    ),
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    TAbs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    TApp(Rc<Self>, Rc<Ty<V>>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Proj(Rc<Self>, #[rc_term(into)] String),
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Zero,
    Succ(Rc<Self>),
    Pred(Rc<Self>),
    IsZero(Rc<Self>),
    Unit,
    Float(f64),
    TimesFloat(Rc<Self>, Rc<Self>),
    Let(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    Inert(Rc<Ty<V>>),
    Fix(Rc<Self>),
}

pub type DeBruijnTerm = Term<usize>;

impl<V> Term<V> {
    pub fn is_int(&self) -> bool {
        match self {
            Self::Zero => true,
            Self::Succ(t) => t.is_int(),
            _ => false,
        }
    }

    pub fn to_int(&self) -> Option<u64> {
        let mut t = self;
        let mut n = 0;
        while let Self::Succ(t_) = t {
            t = t_;
            n += 1;
        }
        if matches!(t, Self::Zero) {
            Some(n)
        } else {
            None
        }
    }

    pub fn from_int(n: u64) -> Rc<Self> {
        let mut t = Self::zero();
        for _ in 0..n {
            t = Self::succ(t);
        }
        t
    }
}

impl<V: Display> Ty<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String => write!(f, "String"),
            Self::Top => write!(f, "Top"),
            Self::Var(x) => write!(f, "{x}"),
            Self::Some(x, ty1, ty2) => {
                if matches!(ty1.as_ref(), Self::Top) {
                    write!(f, "Some {x}. {ty2}")
                } else {
                    write!(f, "Some {x} <: {ty1}. {ty2}")
                }
            }
            Self::Record(fields) => {
                write!(f, "{{")?;
                for (i, (l, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if l == &i.to_string() {
                        write!(f, "{ty}")?;
                    } else {
                        write!(f, "{l}: {ty}")?;
                    }
                }
                write!(f, "}}")
            }
            Self::Bool => write!(f, "Bool"),
            Self::Nat => write!(f, "Nat"),
            Self::Unit => write!(f, "Unit"),
            Self::Id(b) => write!(f, "{b}"),
            Self::Float => write!(f, "Float"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_arrow(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arr(t1, t2) => {
                t1.fmt_atom(f)?;
                write!(f, " -> ")?;
                t2.fmt_arrow(f)
            }
            t => t.fmt_atom(f),
        }
    }
}

impl<V: Display> Display for Ty<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::All(x, ty1, ty2) => {
                if matches!(ty1.as_ref(), Self::Top) {
                    write!(f, "All {x}. {ty2}")
                } else {
                    write!(f, "All {x} <: {ty1}. {ty2}")
                }
            }
            _ => self.fmt_arrow(f),
        }
    }
}

impl<V: Display> Term<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s:?}"),
            Self::Pack(ty1, t2, ty3) => {
                write!(f, "{{*{ty1}, {t2}}} as {ty3}")
            }
            Self::Var(x) => write!(f, "{x}"),
            Self::Record(fields) => {
                write!(f, "{{")?;
                for (i, (l, term)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if l == &i.to_string() {
                        write!(f, "{term}")?;
                    } else {
                        write!(f, "{l}={term}")?;
                    }
                }
                write!(f, "}}")
            }
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Zero => write!(f, "0"),
            Self::Succ(t) if t.is_int() => {
                write!(f, "{}", t.to_int().unwrap() + 1)
            }
            Self::Unit => write!(f, "unit"),
            Self::Float(x) => write!(f, "{x}"),
            Self::Inert(ty) => write!(f, "inert[{ty}]"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_ascribe(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascribe(t, ty) => {
                t.fmt_app(f)?;
                write!(f, " as {ty}")
            }
            _ => self.fmt_atom(f),
        }
    }

    fn fmt_path(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proj(t, l) => {
                t.fmt_path(f)?;
                write!(f, ".{l}")
            }
            _ => self.fmt_ascribe(f),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::App(t1, t2) => {
                t1.fmt_app(f)?;
                write!(f, " ")?;
                t2.fmt_atom(f)
            }
            Self::TApp(t, ty) => {
                t.fmt_path(f)?;
                write!(f, " [{ty}]")
            }
            Self::Succ(t) if !t.is_int() => {
                write!(f, "succ ")?;
                t.fmt_atom(f)
            }
            Self::Pred(t) => {
                write!(f, "pred ")?;
                t.fmt_atom(f)
            }
            Self::IsZero(t) => {
                write!(f, "iszero ")?;
                t.fmt_atom(f)
            }
            Self::TimesFloat(t1, t2) => {
                write!(f, "timesfloat ")?;
                t1.fmt_atom(f)?;
                write!(f, " ")?;
                t2.fmt_atom(f)
            }
            Self::Fix(t) => {
                write!(f, "fix ")?;
                t.fmt_atom(f)
            }
            _ => self.fmt_path(f),
        }
    }
}

impl<V: Display> Display for Term<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unpack(x1, x2, t1, t2) => {
                write!(f, "let {{{x1}, {x2}}} = {t1} in {t2}")
            }
            Self::Abs(x, ty, t) => {
                write!(f, "lambda {x}: {ty}. {t}")
            }
            Self::TAbs(x, ty, t) => {
                if matches!(ty.as_ref(), Ty::Top) {
                    write!(f, "lambda {x}. {t}")
                } else {
                    write!(f, "lambda {x} <: {ty}. {t}")
                }
            }
            Self::If(t1, t2, t3) => {
                write!(f, "if {t1} then {t2} else {t3}")
            }
            Self::Let(x, t1, t2) => {
                write!(f, "let {x} = {t1} in {t2}")
            }
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding<V = String> {
    #[default]
    Name,
    Var(Rc<Ty<V>>),
    TermAbb(Rc<Term<V>>, Option<Rc<Ty<V>>>),
    TyVar(Rc<Ty<V>>),
    TyAbb(Rc<Ty<V>>),
}

pub type DeBruijnBinding = Binding<usize>;
pub type Context = util::Context<DeBruijnBinding>;

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Bind(String, Binding),
    Type(Rc<Term>),
    Noop,
}

impl DeBruijnTy {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Rc<Self>,
    ) -> Rc<Self> {
        match self {
            Self::Top => Self::top(),
            Self::String => Self::string(),
            Self::Var(x) => on_var(cutoff, *x),
            Self::All(x, ty1, ty2) => Self::all(
                x.clone(),
                ty1.map_vars_walk(cutoff, on_var),
                ty2.map_vars_walk(cutoff + 1, on_var),
            ),
            Self::Some(x, ty1, ty2) => Self::some(
                x.clone(),
                ty1.map_vars_walk(cutoff, on_var),
                ty2.map_vars_walk(cutoff + 1, on_var),
            ),
            Self::Arr(t1, t2) => Self::arr(
                t1.map_vars_walk(cutoff, on_var),
                t2.map_vars_walk(cutoff, on_var),
            ),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, ty)| (label.clone(), ty.map_vars_walk(cutoff, on_var)))
                    .collect::<Vec<_>>(),
            ),
            Self::Bool => Self::bool(),
            Self::Nat => Self::nat(),
            Self::Id(s) => Self::id(s.clone()),
            Self::Unit => Self::unit(),
            Self::Float => Self::float(),
        }
    }

    fn map_vars(&self, cutoff: usize, on_var: impl FnMut(usize, usize) -> Rc<Self>) -> Rc<Self> {
        let mut on_var = on_var;
        self.map_vars_walk(cutoff, &mut on_var)
    }

    fn shift_above(&self, d: isize, cutoff: usize) -> Rc<Self> {
        self.map_vars(cutoff, |c, x| {
            if x >= c {
                // TODO: This assertion is not true for all cases.
                // So we should return an error instead of panicking.
                // But this will change the return type of this function and
                // many other functions that call it.
                // This will require a lot of changes, so I will leave it as is for now.
                // See chapter 25.5 in the book Types and Programming Languages.
                assert!(x as isize + d >= 0);
                Self::var((x as isize + d) as usize)
            } else {
                Self::var(x)
            }
        })
    }

    pub fn shift(&self, d: isize) -> Rc<Self> {
        self.shift_above(d, 0)
    }

    fn subst(&self, j: usize, s: &Self) -> Rc<Self> {
        self.map_vars(j, |c, x| {
            if x == c {
                s.shift(c as isize)
            } else {
                Self::var(x)
            }
        })
    }

    pub fn subst_top(&self, s: &Self) -> Rc<Self> {
        self.subst(0, &s.shift(1)).shift(-1)
    }
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Rc<Self>,
        on_type: &mut impl FnMut(usize, &Rc<DeBruijnTy>) -> Rc<DeBruijnTy>,
    ) -> Rc<Self> {
        match self {
            Self::Ascribe(t, ty) => Self::ascribe(
                t.map_vars_walk(cutoff, on_var, on_type),
                on_type(cutoff, ty),
            ),
            Self::String(s) => Self::string(s.clone()),
            Self::Pack(ty1, t2, ty3) => Self::pack(
                on_type(cutoff, ty1),
                t2.map_vars_walk(cutoff, on_var, on_type),
                on_type(cutoff, ty3),
            ),
            Self::Unpack(x1, x2, t1, t2) => Self::unpack(
                x1.clone(),
                x2.clone(),
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff + 2, on_var, on_type),
            ),
            Self::Var(x) => on_var(cutoff, *x),
            Self::Abs(x, ty, t) => Self::abs(
                x.clone(),
                on_type(cutoff, ty),
                t.map_vars_walk(cutoff + 1, on_var, on_type),
            ),
            Self::App(t1, t2) => Self::app(
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff, on_var, on_type),
            ),
            Self::TAbs(x, ty, t) => Self::t_abs(
                x.clone(),
                on_type(cutoff, ty),
                t.map_vars_walk(cutoff + 1, on_var, on_type),
            ),
            Self::TApp(t, ty) => Self::t_app(
                t.map_vars_walk(cutoff, on_var, on_type),
                on_type(cutoff, ty),
            ),
            Self::Proj(t, l) => Self::proj(t.map_vars_walk(cutoff, on_var, on_type), l.clone()),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, term)| {
                        (label.clone(), term.map_vars_walk(cutoff, on_var, on_type))
                    })
                    .collect::<Vec<_>>(),
            ),
            Self::True => Self::true_(),
            Self::False => Self::false_(),
            Self::If(t1, t2, t3) => Self::if_(
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff, on_var, on_type),
                t3.map_vars_walk(cutoff, on_var, on_type),
            ),
            Self::Zero => Self::zero(),
            Self::Succ(t) => Self::succ(t.map_vars_walk(cutoff, on_var, on_type)),
            Self::Pred(t) => Self::pred(t.map_vars_walk(cutoff, on_var, on_type)),
            Self::IsZero(t) => Self::is_zero(t.map_vars_walk(cutoff, on_var, on_type)),
            Self::Let(x, t1, t2) => Self::let_(
                x.clone(),
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff + 1, on_var, on_type),
            ),
            Self::Unit => Self::unit(),
            Self::Inert(ty) => Self::inert(on_type(cutoff, ty)),
            Self::Float(x) => Self::float(*x),
            Self::TimesFloat(t1, t2) => Self::times_float(
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff, on_var, on_type),
            ),
            Self::Fix(t) => Self::fix(t.map_vars_walk(cutoff, on_var, on_type)),
        }
    }

    fn map_vars(
        &self,
        cutoff: usize,
        on_var: impl FnMut(usize, usize) -> Rc<Self>,
        on_type: impl FnMut(usize, &Rc<DeBruijnTy>) -> Rc<DeBruijnTy>,
    ) -> Rc<Self> {
        let mut on_var = on_var;
        let mut on_type = on_type;
        self.map_vars_walk(cutoff, &mut on_var, &mut on_type)
    }

    pub fn shift(&self, d: isize) -> Rc<Self> {
        self.map_vars(
            0,
            |c, x| {
                if x >= c {
                    assert!(x as isize + d >= 0);
                    Self::var((x as isize + d) as usize)
                } else {
                    Self::var(x)
                }
            },
            |c, ty| ty.shift_above(d, c),
        )
    }

    fn subst(&self, j: usize, s: &Self) -> Rc<Self> {
        self.map_vars(
            j,
            |c, x| {
                if x == c {
                    s.shift(c as isize)
                } else {
                    Self::var(x)
                }
            },
            |_, ty| ty.clone(),
        )
    }

    pub fn subst_top(&self, s: &Self) -> Rc<Self> {
        self.subst(0, &s.shift(1)).shift(-1)
    }

    fn subst_ty(&self, j: usize, ty: &DeBruijnTy) -> Rc<Self> {
        self.map_vars(j, |_, x| Self::var(x), |c, ty_| ty_.subst(c, ty))
    }

    pub fn subst_top_ty(&self, ty: &DeBruijnTy) -> Rc<Self> {
        self.subst_ty(0, &ty.shift(1)).shift(-1)
    }
}

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Self {
        match self {
            Self::Name => Self::Name,
            Self::TermAbb(t, ty) => Self::TermAbb(t.shift(d), ty.as_ref().map(|ty| ty.shift(d))),
            Self::Var(ty) => Self::Var(ty.shift(d)),
            Self::TyVar(ty) => Self::TyVar(ty.shift(d)),
            Self::TyAbb(ty) => Self::TyAbb(ty.shift(d)),
        }
    }
}

impl DeBruijnTy {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Top => Ok(Ty::top()),
            Self::Var(x) => Ok(Ty::var(ctx.index_to_name(*x)?)),
            Self::String => Ok(Ty::string()),
            Self::Some(x, ty1, ty2) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Ty::some(
                    name.clone(),
                    ty1.to_named(ctx)?,
                    ctx.with_name(name, |ctx| ty2.to_named(ctx))?,
                ))
            }
            Self::Arr(t1, t2) => Ok(Ty::arr(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::All(x, ty1, ty2) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Ty::all(
                    name.clone(),
                    ty1.to_named(ctx)?,
                    ctx.with_name(name, |ctx| ty2.to_named(ctx))?,
                ))
            }
            Self::Record(fields) => Ok(Ty::record(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Bool => Ok(Ty::bool()),
            Self::Nat => Ok(Ty::nat()),
            Self::Unit => Ok(Ty::unit()),
            Self::Id(s) => Ok(Ty::id(s.clone())),
            Self::Float => Ok(Ty::float()),
        }
    }
}

impl Ty {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Top => Ok(DeBruijnTy::top()),
            Self::Var(x) => Ok(ctx
                .name_to_index(x)
                .map_or_else(|_| DeBruijnTy::id(x.clone()), DeBruijnTy::var)),
            Self::String => Ok(DeBruijnTy::string()),
            Self::Some(x, ty1, ty2) => Ok(DeBruijnTy::some(
                x.clone(),
                ty1.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| ty2.to_de_bruijn(ctx))?,
            )),
            Self::Arr(t1, t2) => Ok(DeBruijnTy::arr(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::All(x, ty1, ty2) => Ok(DeBruijnTy::all(
                x.clone(),
                ty1.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| ty2.to_de_bruijn(ctx))?,
            )),
            Self::Record(fields) => Ok(DeBruijnTy::record(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Bool => Ok(DeBruijnTy::bool()),
            Self::Nat => Ok(DeBruijnTy::nat()),
            Self::Unit => Ok(DeBruijnTy::unit()),
            Self::Id(s) => Ok(DeBruijnTy::id(s.clone())),
            Self::Float => Ok(DeBruijnTy::float()),
        }
    }
}

impl DeBruijnTerm {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Term>> {
        match self {
            Self::Ascribe(t, ty) => Ok(Term::ascribe(t.to_named(ctx)?, ty.to_named(ctx)?)),
            Self::String(s) => Ok(Term::string(s.clone())),
            Self::Pack(ty1, t2, ty3) => Ok(Term::pack(
                ty1.to_named(ctx)?,
                t2.to_named(ctx)?,
                ty3.to_named(ctx)?,
            )),
            Self::Unpack(x1, x2, t1, t2) => {
                let name1 = ctx.pick_fresh_name(x1);
                let name2 = ctx.pick_fresh_name(x2);
                Ok(Term::unpack(
                    name1.clone(),
                    name2.clone(),
                    t1.to_named(ctx)?,
                    ctx.with_name(name1, |ctx| ctx.with_name(name2, |ctx| t2.to_named(ctx)))?,
                ))
            }
            Self::Var(x) => Ok(Term::var(ctx.index_to_name(*x)?)),
            Self::Abs(x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::abs(
                    name.clone(),
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::App(t1, t2) => Ok(Term::app(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::TAbs(x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::t_abs(
                    name.clone(),
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::TApp(t, ty) => Ok(Term::t_app(t.to_named(ctx)?, ty.to_named(ctx)?)),
            Self::Record(fields) => Ok(Term::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(Term::proj(t.to_named(ctx)?, l.clone())),
            Self::True => Ok(Term::true_()),
            Self::False => Ok(Term::false_()),
            Self::If(t1, t2, t3) => Ok(Term::if_(
                t1.to_named(ctx)?,
                t2.to_named(ctx)?,
                t3.to_named(ctx)?,
            )),
            Self::Zero => Ok(Term::zero()),
            Self::Succ(t) => Ok(Term::succ(t.to_named(ctx)?)),
            Self::Pred(t) => Ok(Term::pred(t.to_named(ctx)?)),
            Self::IsZero(t) => Ok(Term::is_zero(t.to_named(ctx)?)),
            Self::Unit => Ok(Term::unit()),
            Self::Float(x) => Ok(Term::float(*x)),
            Self::TimesFloat(t1, t2) => Ok(Term::times_float(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::Let(x, t1, t2) => Ok(Term::let_(
                x.clone(),
                t1.to_named(ctx)?,
                ctx.with_name(x.clone(), |ctx| t2.to_named(ctx))?,
            )),
            Self::Inert(ty) => Ok(Term::inert(ty.to_named(ctx)?)),
            Self::Fix(t) => Ok(Term::fix(t.to_named(ctx)?)),
        }
    }
}

impl Term {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTerm>> {
        match self {
            Self::Ascribe(t, ty) => Ok(DeBruijnTerm::ascribe(
                t.to_de_bruijn(ctx)?,
                ty.to_de_bruijn(ctx)?,
            )),
            Self::String(s) => Ok(DeBruijnTerm::string(s.clone())),
            Self::Pack(ty1, t2, ty3) => Ok(DeBruijnTerm::pack(
                ty1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                ty3.to_de_bruijn(ctx)?,
            )),
            Self::Unpack(x1, x2, t1, t2) => Ok(DeBruijnTerm::unpack(
                x1.clone(),
                x2.clone(),
                t1.to_de_bruijn(ctx)?,
                ctx.with_name(x1.clone(), |ctx| {
                    ctx.with_name(x2.clone(), |ctx| t2.to_de_bruijn(ctx))
                })?,
            )),
            Self::Var(x) => Ok(DeBruijnTerm::var(ctx.name_to_index(x)?)),
            Self::Abs(x, ty, t) => Ok(DeBruijnTerm::abs(
                x.clone(),
                ty.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::App(t1, t2) => Ok(DeBruijnTerm::app(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::TAbs(x, ty, t) => Ok(DeBruijnTerm::t_abs(
                x.clone(),
                ty.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::TApp(t, ty) => Ok(DeBruijnTerm::t_app(
                t.to_de_bruijn(ctx)?,
                ty.to_de_bruijn(ctx)?,
            )),
            Self::Record(fields) => Ok(DeBruijnTerm::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(DeBruijnTerm::proj(t.to_de_bruijn(ctx)?, l.clone())),
            Self::True => Ok(DeBruijnTerm::true_()),
            Self::False => Ok(DeBruijnTerm::false_()),
            Self::If(t1, t2, t3) => Ok(DeBruijnTerm::if_(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                t3.to_de_bruijn(ctx)?,
            )),
            Self::Zero => Ok(DeBruijnTerm::zero()),
            Self::Succ(t) => Ok(DeBruijnTerm::succ(t.to_de_bruijn(ctx)?)),
            Self::Pred(t) => Ok(DeBruijnTerm::pred(t.to_de_bruijn(ctx)?)),
            Self::IsZero(t) => Ok(DeBruijnTerm::is_zero(t.to_de_bruijn(ctx)?)),
            Self::Unit => Ok(DeBruijnTerm::unit()),
            Self::Float(x) => Ok(DeBruijnTerm::float(*x)),
            Self::TimesFloat(t1, t2) => Ok(DeBruijnTerm::times_float(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::Let(x, t1, t2) => Ok(DeBruijnTerm::let_(
                x.clone(),
                t1.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| t2.to_de_bruijn(ctx))?,
            )),
            Self::Inert(ty) => Ok(DeBruijnTerm::inert(ty.to_de_bruijn(ctx)?)),
            Self::Fix(t) => Ok(DeBruijnTerm::fix(t.to_de_bruijn(ctx)?)),
        }
    }
}

impl Binding {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<DeBruijnBinding> {
        match self {
            Self::Name => Ok(DeBruijnBinding::Name),
            Self::Var(ty) => Ok(DeBruijnBinding::Var(ty.to_de_bruijn(ctx)?)),
            Self::TermAbb(t, ty) => Ok(DeBruijnBinding::TermAbb(
                t.to_de_bruijn(ctx)?,
                ty.as_ref().map(|ty| ty.to_de_bruijn(ctx)).transpose()?,
            )),
            Self::TyVar(ty) => Ok(DeBruijnBinding::TyVar(ty.to_de_bruijn(ctx)?)),
            Self::TyAbb(ty) => Ok(DeBruijnBinding::TyAbb(ty.to_de_bruijn(ctx)?)),
        }
    }
}
