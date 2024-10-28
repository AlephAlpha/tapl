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
    "let",
    "in",
    "lambda",
    "succ",
    "pred",
    "iszero",
    "timesfloat",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    String(#[rc_term(into)] String),
    Var(#[rc_term(into)] V),
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Let(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Proj(Rc<Self>, #[rc_term(into)] String),
    Abs(#[rc_term(into)] String, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    Zero,
    Succ(Rc<Self>),
    Pred(Rc<Self>),
    IsZero(Rc<Self>),
    Float(f64),
    TimesFloat(Rc<Self>, Rc<Self>),
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

impl<V: Display> Term<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s:?}"),
            Self::Var(x) => write!(f, "{x}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Zero => write!(f, "0"),
            Self::Succ(t) if t.is_int() => write!(f, "{}", t.to_int().unwrap() + 1),
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
            Self::Float(x) => write!(f, "{x}"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_path(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proj(t, l) => {
                t.fmt_path(f)?;
                write!(f, ".{l}")
            }
            _ => self.fmt_atom(f),
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
            Self::Let(x, t1, t2) => {
                write!(f, "let {x} = {t1} in {t2}")
            }
            Self::Abs(x, t) => {
                write!(f, "lambda {x}. {t}")
            }
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding<V = String> {
    #[default]
    Name,
    TermAbb(Rc<Term<V>>),
}

pub type DeBruijnBinding = Binding<usize>;
pub type Context = util::Context<DeBruijnBinding>;

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Bind(String, Binding),
    Noop,
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        f: &mut impl FnMut(usize, usize) -> Rc<Self>,
    ) -> Rc<Self> {
        match self {
            Self::String(s) => Self::string(s.clone()),
            Self::Var(x) => f(cutoff, *x),
            Self::True => Self::true_(),
            Self::False => Self::false_(),
            Self::If(t1, t2, t3) => Self::if_(
                t1.map_vars_walk(cutoff, f),
                t2.map_vars_walk(cutoff, f),
                t3.map_vars_walk(cutoff, f),
            ),
            Self::Let(x, t1, t2) => Self::let_(
                x.clone(),
                t1.map_vars_walk(cutoff, f),
                t2.map_vars_walk(cutoff + 1, f),
            ),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, term)| (label.clone(), term.map_vars_walk(cutoff, f)))
                    .collect::<Vec<_>>(),
            ),
            Self::Proj(t, l) => Self::proj(t.map_vars_walk(cutoff, f), l.clone()),
            Self::Abs(x, t) => Self::abs(x.clone(), t.map_vars_walk(cutoff + 1, f)),
            Self::App(t1, t2) => {
                Self::app(t1.map_vars_walk(cutoff, f), t2.map_vars_walk(cutoff, f))
            }
            Self::Zero => Self::zero(),
            Self::Succ(t) => Self::succ(t.map_vars_walk(cutoff, f)),
            Self::Pred(t) => Self::pred(t.map_vars_walk(cutoff, f)),
            Self::IsZero(t) => Self::is_zero(t.map_vars_walk(cutoff, f)),
            Self::Float(x) => Self::float(*x),
            Self::TimesFloat(t1, t2) => {
                Self::times_float(t1.map_vars_walk(cutoff, f), t2.map_vars_walk(cutoff, f))
            }
        }
    }

    fn map_vars(&self, cutoff: usize, f: impl FnMut(usize, usize) -> Rc<Self>) -> Rc<Self> {
        let mut f = f;
        self.map_vars_walk(cutoff, &mut f)
    }

    pub fn shift(&self, d: isize) -> Rc<Self> {
        self.map_vars(0, |c, x| {
            if x >= c {
                assert!(x as isize + d >= 0);
                Self::var((x as isize + d) as usize)
            } else {
                Self::var(x)
            }
        })
    }

    fn subst(&self, j: usize, s: &Self) -> Rc<Self> {
        self.map_vars(0, |c, x| {
            if x == j + c {
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

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Self {
        match self {
            Self::Name => Self::Name,
            Self::TermAbb(t) => Self::TermAbb(t.shift(d)),
        }
    }
}

impl DeBruijnTerm {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Term>> {
        match self {
            Self::String(s) => Ok(Term::string(s.clone())),
            Self::Var(x) => Ok(Term::var(ctx.index_to_name(*x)?)),
            Self::True => Ok(Term::true_()),
            Self::False => Ok(Term::false_()),
            Self::If(t1, t2, t3) => Ok(Term::if_(
                t1.to_named(ctx)?,
                t2.to_named(ctx)?,
                t3.to_named(ctx)?,
            )),
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
            Self::Abs(x, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::abs(
                    name.clone(),
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::App(t1, t2) => Ok(Term::app(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::Zero => Ok(Term::zero()),
            Self::Succ(t) => Ok(Term::succ(t.to_named(ctx)?)),
            Self::Pred(t) => Ok(Term::pred(t.to_named(ctx)?)),
            Self::IsZero(t) => Ok(Term::is_zero(t.to_named(ctx)?)),
            Self::Float(x) => Ok(Term::float(*x)),
            Self::TimesFloat(t1, t2) => Ok(Term::times_float(t1.to_named(ctx)?, t2.to_named(ctx)?)),
        }
    }
}

impl Term {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTerm>> {
        match self {
            Self::String(s) => Ok(DeBruijnTerm::string(s.clone())),
            Self::Var(x) => Ok(DeBruijnTerm::var(ctx.name_to_index(x)?)),
            Self::True => Ok(DeBruijnTerm::true_()),
            Self::False => Ok(DeBruijnTerm::false_()),
            Self::If(t1, t2, t3) => Ok(DeBruijnTerm::if_(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                t3.to_de_bruijn(ctx)?,
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
            Self::Abs(x, t) => Ok(DeBruijnTerm::abs(
                x.clone(),
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::App(t1, t2) => Ok(DeBruijnTerm::app(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::Zero => Ok(DeBruijnTerm::zero()),
            Self::Succ(t) => Ok(DeBruijnTerm::succ(t.to_de_bruijn(ctx)?)),
            Self::Pred(t) => Ok(DeBruijnTerm::pred(t.to_de_bruijn(ctx)?)),
            Self::IsZero(t) => Ok(DeBruijnTerm::is_zero(t.to_de_bruijn(ctx)?)),
            Self::Float(x) => Ok(DeBruijnTerm::float(*x)),
            Self::TimesFloat(t1, t2) => Ok(DeBruijnTerm::times_float(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
        }
    }
}

impl Binding {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<DeBruijnBinding> {
        match self {
            Self::Name => Ok(DeBruijnBinding::Name),
            Self::TermAbb(t) => Ok(DeBruijnBinding::TermAbb(t.to_de_bruijn(ctx)?)),
        }
    }
}
