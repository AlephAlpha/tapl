use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    RcTerm,
    error::{Error, Result},
};

pub const KEYWORDS: &[&str] = &["lambda", "_"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

pub type DeBruijnTerm = Term<usize>;

impl<V: Display> Term<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
                t2.fmt_atom(f)
            }
            _ => self.fmt_atom(f),
        }
    }
}

impl<V: Display> Display for Term<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abs(x, t) => {
                write!(f, "lambda {x}. {t}")
            }
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Binding {
    #[default]
    Name,
}

pub type Context = util::Context<Binding>;

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
        f: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        match self {
            Self::Var(x) => f(cutoff, *x),
            Self::Abs(x, t) => Ok(Self::abs(x.clone(), t.map_vars_walk(cutoff + 1, f)?)),
            Self::App(t1, t2) => Ok(Self::app(
                t1.map_vars_walk(cutoff, f)?,
                t2.map_vars_walk(cutoff, f)?,
            )),
        }
    }

    fn map_vars(
        &self,
        cutoff: usize,
        f: impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        let mut f = f;
        self.map_vars_walk(cutoff, &mut f)
    }

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.map_vars(0, |c, x| {
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

    fn subst(&self, j: usize, s: &Self) -> Result<Rc<Self>> {
        self.map_vars(0, |c, x| {
            if x == j + c {
                s.shift(c as isize)
            } else {
                Ok(Self::var(x))
            }
        })
    }

    pub fn subst_top(&self, s: &Self) -> Result<Rc<Self>> {
        self.subst(0, s.shift(1)?.as_ref())?.shift(-1)
    }
}

impl DeBruijnTerm {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Term>> {
        match self {
            Self::Var(x) => Ok(Term::var(ctx.index_to_name(*x)?)),
            Self::Abs(x, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::abs(
                    name.clone(),
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::App(t1, t2) => Ok(Term::app(t1.to_named(ctx)?, t2.to_named(ctx)?)),
        }
    }
}

impl Term {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTerm>> {
        match self {
            Self::Var(x) => Ok(DeBruijnTerm::var(ctx.name_to_index(x)?)),
            Self::Abs(x, t) => Ok(DeBruijnTerm::abs(
                x.clone(),
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::App(t1, t2) => Ok(DeBruijnTerm::app(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
        }
    }
}
