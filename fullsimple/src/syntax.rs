use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    error::{Error, Result},
    BindingShift, RcTerm,
};

pub const KEYWORDS: &[&str] = &[
    "true",
    "false",
    "if",
    "then",
    "else",
    "unit",
    "timesfloat",
    "as",
    "case",
    "of",
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
    "Unit",
    "Float",
    "String",
    "Bool",
    "Nat",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty<V = String> {
    Id(#[rc_term(into)] String),
    Var(#[rc_term(into)] V),
    Unit,
    Float,
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Variant(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    String,
    Bool,
    Arr(Rc<Self>, Rc<Self>),
    Nat,
}

pub type DeBruijnTy = Ty<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Ascribe(Rc<Self>, Rc<Ty<V>>),
    String(#[rc_term(into)] String),
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Case(Rc<Self>, #[rc_term(into)] Vec<(String, String, Rc<Self>)>),
    Tag(#[rc_term(into)] String, Rc<Self>, Rc<Ty<V>>),
    Unit,
    Float(f64),
    TimesFloat(Rc<Self>, Rc<Self>),
    Let(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Proj(Rc<Self>, #[rc_term(into)] String),
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    Fix(Rc<Self>),
    Zero,
    Succ(Rc<Self>),
    Pred(Rc<Self>),
    IsZero(Rc<Self>),
    Inert(Rc<Ty<V>>),
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
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "Unit"),
            Self::Id(b) => write!(f, "{b}"),
            Self::Float => write!(f, "Float"),
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
            Self::Variant(fields) => {
                write!(f, "<")?;
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
                write!(f, ">")
            }
            Self::Var(x) => write!(f, "{x}"),
            Self::Nat => write!(f, "Nat"),
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
        self.fmt_arrow(f)
    }
}

impl<V: Display> Term<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s:?}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Tag(l, t, ty) => {
                write!(f, "<{l}={t}> as {ty}")
            }
            Self::Unit => write!(f, "unit"),
            Self::Float(x) => write!(f, "{x}"),
            Self::Zero => write!(f, "0"),
            Self::Succ(t) if t.is_int() => {
                write!(f, "{}", t.to_int().unwrap() + 1)
            }
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
            Self::Inert(ty) => write!(f, "inert[{ty}]"),
            Self::Var(x) => write!(f, "{x}"),
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
            Self::If(t1, t2, t3) => {
                write!(f, "if {t1} then {t2} else {t3}")
            }
            Self::Case(t, cases) => {
                write!(f, "case {t} of ")?;
                for (i, (l, x, t)) in cases.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "<{l}={x}> ==> {t}")?;
                }
                Ok(())
            }
            Self::Let(x, t1, t2) => {
                write!(f, "let {x} = {t1} in {t2}")
            }
            Self::Abs(x, ty, t) => {
                write!(f, "lambda {x}: {ty}. {t}")
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
    TyVar,
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
        on_var: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        Ok(match self {
            Self::Id(s) => Self::id(s.clone()),
            Self::Var(x) => on_var(cutoff, *x)?,
            Self::Unit => Self::unit(),
            Self::Float => Self::float(),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.map_vars_walk(cutoff, on_var)?)))
                    .collect::<Result<Vec<_>>>()?,
            ),
            Self::Variant(fields) => Self::variant(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.map_vars_walk(cutoff, on_var)?)))
                    .collect::<Result<Vec<_>>>()?,
            ),
            Self::String => Self::string(),
            Self::Bool => Self::bool(),
            Self::Arr(t1, t2) => Self::arr(
                t1.map_vars_walk(cutoff, on_var)?,
                t2.map_vars_walk(cutoff, on_var)?,
            ),
            Self::Nat => Self::nat(),
        })
    }

    fn map_vars(
        &self,
        cutoff: usize,
        on_var: impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        let mut on_var = on_var;
        self.map_vars_walk(cutoff, &mut on_var)
    }

    fn shift_above(&self, d: isize, cutoff: usize) -> Result<Rc<Self>> {
        self.map_vars(cutoff, |c, x| {
            if x >= c {
                if x as isize + d < 0 {
                    return Err(Error::ScopingError);
                }
                Ok(Self::var((x as isize + d) as usize))
            } else {
                Ok(Self::var(x))
            }
        })
    }

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.shift_above(d, 0)
    }
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
        on_type: &mut impl FnMut(usize, &Rc<DeBruijnTy>) -> Result<Rc<DeBruijnTy>>,
    ) -> Result<Rc<Self>> {
        Ok(match self {
            Self::Ascribe(t, ty) => Self::ascribe(
                t.map_vars_walk(cutoff, on_var, on_type)?,
                on_type(cutoff, ty)?,
            ),
            Self::String(s) => Self::string(s.clone()),
            Self::True => Self::true_(),
            Self::False => Self::false_(),
            Self::If(t1, t2, t3) => Self::if_(
                t1.map_vars_walk(cutoff, on_var, on_type)?,
                t2.map_vars_walk(cutoff, on_var, on_type)?,
                t3.map_vars_walk(cutoff, on_var, on_type)?,
            ),
            Self::Case(t, cases) => Self::case(
                t.map_vars_walk(cutoff, on_var, on_type)?,
                cases
                    .iter()
                    .map(|(l, x, t)| {
                        Ok((
                            l.clone(),
                            x.clone(),
                            t.map_vars_walk(cutoff + 1, on_var, on_type)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            ),
            Self::Tag(l, t, ty) => Self::tag(
                l.clone(),
                t.map_vars_walk(cutoff, on_var, on_type)?,
                on_type(cutoff, ty)?,
            ),
            Self::Unit => Self::unit(),
            Self::Float(x) => Self::float(*x),
            Self::TimesFloat(t1, t2) => Self::times_float(
                t1.map_vars_walk(cutoff, on_var, on_type)?,
                t2.map_vars_walk(cutoff, on_var, on_type)?,
            ),
            Self::Let(x, t1, t2) => Self::let_(
                x.clone(),
                t1.map_vars_walk(cutoff, on_var, on_type)?,
                t2.map_vars_walk(cutoff + 1, on_var, on_type)?,
            ),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, term)| {
                        Ok((label.clone(), term.map_vars_walk(cutoff, on_var, on_type)?))
                    })
                    .collect::<Result<Vec<_>>>()?,
            ),
            Self::Proj(t, l) => Self::proj(t.map_vars_walk(cutoff, on_var, on_type)?, l.clone()),
            Self::Var(x) => on_var(cutoff, *x)?,
            Self::Abs(x, ty, t) => Self::abs(
                x.clone(),
                on_type(cutoff, ty)?,
                t.map_vars_walk(cutoff + 1, on_var, on_type)?,
            ),
            Self::App(t1, t2) => Self::app(
                t1.map_vars_walk(cutoff, on_var, on_type)?,
                t2.map_vars_walk(cutoff, on_var, on_type)?,
            ),
            Self::Fix(t) => Self::fix(t.map_vars_walk(cutoff, on_var, on_type)?),
            Self::Zero => Self::zero(),
            Self::Succ(t) => Self::succ(t.map_vars_walk(cutoff, on_var, on_type)?),
            Self::Pred(t) => Self::pred(t.map_vars_walk(cutoff, on_var, on_type)?),
            Self::IsZero(t) => Self::is_zero(t.map_vars_walk(cutoff, on_var, on_type)?),
            Self::Inert(ty) => Self::inert(on_type(cutoff, ty)?),
        })
    }

    fn map_vars(
        &self,
        cutoff: usize,
        on_var: impl FnMut(usize, usize) -> Result<Rc<Self>>,
        on_type: impl FnMut(usize, &Rc<DeBruijnTy>) -> Result<Rc<DeBruijnTy>>,
    ) -> Result<Rc<Self>> {
        let mut on_var = on_var;
        let mut on_type = on_type;
        self.map_vars_walk(cutoff, &mut on_var, &mut on_type)
    }

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.map_vars(
            0,
            |c, x| {
                if x >= c {
                    if x as isize + d < 0 {
                        return Err(Error::ScopingError);
                    }
                    Ok(Self::var((x as isize + d) as usize))
                } else {
                    Ok(Self::var(x))
                }
            },
            |c, ty| ty.shift_above(d, c),
        )
    }

    fn subst(&self, j: usize, s: &Self) -> Result<Rc<Self>> {
        self.map_vars(
            0,
            |c, x| {
                if x == j + c {
                    s.shift(c as isize)
                } else {
                    Ok(Self::var(x))
                }
            },
            |_, ty| Ok(ty.clone()),
        )
    }

    pub fn subst_top(&self, s: &Self) -> Result<Rc<Self>> {
        self.subst(0, s.shift(1)?.as_ref())?.shift(-1)
    }
}

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Result<Self> {
        match self {
            Self::Name => Ok(Self::Name),
            Self::TermAbb(t, ty) => Ok(Self::TermAbb(
                t.shift(d)?,
                ty.as_ref().map(|ty| ty.shift(d)).transpose()?,
            )),
            Self::Var(ty) => Ok(Self::Var(ty.shift(d)?)),
            Self::TyVar => Ok(Self::TyVar),
            Self::TyAbb(ty) => Ok(Self::TyAbb(ty.shift(d)?)),
        }
    }
}

impl DeBruijnTy {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Id(s) => Ok(Ty::id(s.clone())),
            Self::Var(x) => Ok(Ty::var(ctx.index_to_name(*x)?)),
            Self::Unit => Ok(Ty::unit()),
            Self::Float => Ok(Ty::float()),
            Self::Record(fields) => Ok(Ty::record(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Variant(fields) => Ok(Ty::variant(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::String => Ok(Ty::string()),
            Self::Bool => Ok(Ty::bool()),
            Self::Arr(t1, t2) => Ok(Ty::arr(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::Nat => Ok(Ty::nat()),
        }
    }
}

impl Ty {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Id(s) => Ok(DeBruijnTy::id(s.clone())),
            Self::Var(x) => Ok(ctx
                .name_to_index(x)
                .map_or_else(|_| DeBruijnTy::id(x.clone()), DeBruijnTy::var)),
            Self::Unit => Ok(DeBruijnTy::unit()),
            Self::Float => Ok(DeBruijnTy::float()),
            Self::Record(fields) => Ok(DeBruijnTy::record(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Variant(fields) => Ok(DeBruijnTy::variant(
                fields
                    .iter()
                    .map(|(label, ty)| Ok((label.clone(), ty.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::String => Ok(DeBruijnTy::string()),
            Self::Bool => Ok(DeBruijnTy::bool()),
            Self::Arr(t1, t2) => Ok(DeBruijnTy::arr(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::Nat => Ok(DeBruijnTy::nat()),
        }
    }
}

impl DeBruijnTerm {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Term>> {
        match self {
            Self::Ascribe(t, ty) => Ok(Term::ascribe(t.to_named(ctx)?, ty.to_named(ctx)?)),
            Self::String(s) => Ok(Term::string(s.clone())),
            Self::True => Ok(Term::true_()),
            Self::False => Ok(Term::false_()),
            Self::If(t1, t2, t3) => Ok(Term::if_(
                t1.to_named(ctx)?,
                t2.to_named(ctx)?,
                t3.to_named(ctx)?,
            )),
            Self::Case(t, cases) => Ok(Term::case(
                t.to_named(ctx)?,
                cases
                    .iter()
                    .map(|(l, x, t)| {
                        let name = ctx.pick_fresh_name(x);
                        Ok((
                            l.clone(),
                            name,
                            ctx.with_name(x.clone(), |ctx| t.to_named(ctx))?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Tag(l, t, ty) => Ok(Term::tag(l.clone(), t.to_named(ctx)?, ty.to_named(ctx)?)),
            Self::Unit => Ok(Term::unit()),
            Self::Float(x) => Ok(Term::float(*x)),
            Self::TimesFloat(t1, t2) => Ok(Term::times_float(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::Let(x, t1, t2) => Ok(Term::let_(
                x.clone(),
                t1.to_named(ctx)?,
                ctx.with_name(x.clone(), |ctx| t2.to_named(ctx))?,
            )),
            Self::Record(fields) => Ok(Term::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(Term::proj(t.to_named(ctx)?, l.clone())),
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
            Self::Fix(t) => Ok(Term::fix(t.to_named(ctx)?)),
            Self::Zero => Ok(Term::zero()),
            Self::Succ(t) => Ok(Term::succ(t.to_named(ctx)?)),
            Self::Pred(t) => Ok(Term::pred(t.to_named(ctx)?)),
            Self::IsZero(t) => Ok(Term::is_zero(t.to_named(ctx)?)),
            Self::Inert(ty) => Ok(Term::inert(ty.to_named(ctx)?)),
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
            Self::True => Ok(DeBruijnTerm::true_()),
            Self::False => Ok(DeBruijnTerm::false_()),
            Self::If(t1, t2, t3) => Ok(DeBruijnTerm::if_(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                t3.to_de_bruijn(ctx)?,
            )),
            Self::Case(t, cases) => Ok(DeBruijnTerm::case(
                t.to_de_bruijn(ctx)?,
                cases
                    .iter()
                    .map(|(l, x, t)| {
                        Ok((
                            l.clone(),
                            x.clone(),
                            ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Tag(l, t, ty) => Ok(DeBruijnTerm::tag(
                l.clone(),
                t.to_de_bruijn(ctx)?,
                ty.to_de_bruijn(ctx)?,
            )),
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
            Self::Record(fields) => Ok(DeBruijnTerm::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(DeBruijnTerm::proj(t.to_de_bruijn(ctx)?, l.clone())),
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
            Self::Fix(t) => Ok(DeBruijnTerm::fix(t.to_de_bruijn(ctx)?)),
            Self::Zero => Ok(DeBruijnTerm::zero()),
            Self::Succ(t) => Ok(DeBruijnTerm::succ(t.to_de_bruijn(ctx)?)),
            Self::Pred(t) => Ok(DeBruijnTerm::pred(t.to_de_bruijn(ctx)?)),
            Self::IsZero(t) => Ok(DeBruijnTerm::is_zero(t.to_de_bruijn(ctx)?)),
            Self::Inert(ty) => Ok(DeBruijnTerm::inert(ty.to_de_bruijn(ctx)?)),
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
            Self::TyVar => Ok(DeBruijnBinding::TyVar),
            Self::TyAbb(ty) => Ok(DeBruijnBinding::TyAbb(ty.to_de_bruijn(ctx)?)),
        }
    }
}
