use crate::error::{Error, Result};
use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

pub const KEYWORDS: &[&str] = &["lambda"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind"];

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(String),
    Abs(String, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

impl Term {
    pub fn var(x: impl Into<String>) -> Rc<Self> {
        Rc::new(Self::Var(x.into()))
    }

    pub fn abs(x: impl Into<String>, t: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Abs(x.into(), t))
    }

    pub fn app(t1: Rc<Self>, t2: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::App(t1, t2))
    }
}

impl Term {
    fn fmt_atoms(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{x}"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::App(t1, t2) => {
                t1.fmt_app(f)?;
                write!(f, " ")?;
                t2.fmt_atoms(f)
            }
            _ => self.fmt_atoms(f),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abs(x, t) => {
                write!(f, "lambda {x}. {t}")
            }
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Bind(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeBruijnTerm {
    Var(usize),
    Abs(String, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

impl DeBruijnTerm {
    pub fn var(x: usize) -> Rc<Self> {
        Rc::new(Self::Var(x))
    }

    pub fn abs(x: impl Into<String>, t: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Abs(x.into(), t))
    }

    pub fn app(t1: Rc<Self>, t2: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::App(t1, t2))
    }
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        f: &mut impl FnMut(usize, usize) -> Rc<Self>,
    ) -> Rc<Self> {
        match self {
            Self::Var(x) => f(cutoff, *x),
            Self::Abs(x, t) => Self::abs(x.clone(), t.map_vars_walk(cutoff + 1, f)),
            Self::App(t1, t2) => {
                Self::app(t1.map_vars_walk(cutoff, f), t2.map_vars_walk(cutoff, f))
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

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Context {
    names: Vec<String>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn add_name(&mut self, name: impl Into<String>) {
        self.names.push(name.into());
    }

    fn drop_name(&mut self) {
        self.names.pop();
    }

    fn with_name<T>(&mut self, name: impl Into<String>, f: impl FnOnce(&mut Self) -> T) -> T {
        self.add_name(name);
        let result = f(self);
        self.drop_name();
        result
    }

    fn is_name_bound(&self, name: &str) -> bool {
        self.names.iter().any(|n| n == name)
    }

    fn pick_fresh_name(&self, name: &str) -> String {
        let mut name = name.to_string();
        while self.is_name_bound(&name) {
            name.push('\'');
        }
        name
    }

    fn index_to_name(&self, i: usize) -> Result<&str> {
        self.names
            .get(self.len() - i - 1)
            .map(String::as_str)
            .ok_or_else(|| Error::VariableLookupFailure(i, self.len()))
    }

    fn name_to_index(&self, name: &str) -> Result<usize> {
        self.names
            .iter()
            .rposition(|n| n == name)
            .map(|i| self.len() - i - 1)
            .ok_or_else(|| Error::IdentifierUnbound(name.to_string()))
    }
}

impl DeBruijnTerm {
    pub fn to_term(&self, ctx: &mut Context) -> Result<Rc<Term>> {
        match self {
            Self::Var(x) => Ok(Term::var(ctx.index_to_name(*x)?.to_string())),
            Self::Abs(x, t) => {
                let name = ctx.pick_fresh_name(x);
                ctx.with_name(name.clone(), |ctx| Ok(Term::abs(name, t.to_term(ctx)?)))
            }
            Self::App(t1, t2) => Ok(Term::app(t1.to_term(ctx)?, t2.to_term(ctx)?)),
        }
    }
}

impl Term {
    pub fn to_debruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTerm>> {
        match self {
            Self::Var(x) => Ok(DeBruijnTerm::var(ctx.name_to_index(x)?)),
            Self::Abs(x, t) => ctx.with_name(x.clone(), |ctx| {
                Ok(DeBruijnTerm::abs(x.clone(), t.to_debruijn(ctx)?))
            }),
            Self::App(t1, t2) => Ok(DeBruijnTerm::app(
                t1.to_debruijn(ctx)?,
                t2.to_debruijn(ctx)?,
            )),
        }
    }
}
