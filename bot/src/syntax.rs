use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    RcTerm,
    error::{Error, Result},
};

pub const KEYWORDS: &[&str] = &["lambda", "_", "Bot", "Top"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty {
    Bot,
    Top,
    Arr(Rc<Self>, Rc<Self>),
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

pub type DeBruijnTerm = Term<usize>;

impl Ty {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bot => write!(f, "Bot"),
            Self::Top => write!(f, "Top"),
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

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_arrow(f)
    }
}

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
            Self::Abs(x, ty, t) => write!(f, "lambda {x}: {ty}. {t}"),
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding {
    #[default]
    Name,
    Var(Rc<Ty>),
}

pub type Context = util::Context<Binding>;

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Bind(String, Binding),
    Type(Rc<Term>),
    Noop,
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        match self {
            Self::Var(x) => on_var(cutoff, *x),
            Self::Abs(x, ty, t) => Ok(Self::abs(
                x.clone(),
                ty.clone(),
                t.map_vars_walk(cutoff + 1, on_var)?,
            )),
            Self::App(t1, t2) => Ok(Self::app(
                t1.map_vars_walk(cutoff, on_var)?,
                t2.map_vars_walk(cutoff, on_var)?,
            )),
        }
    }

    fn map_vars(
        &self,
        cutoff: usize,
        on_var: impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        let mut on_var = on_var;
        self.map_vars_walk(cutoff, &mut on_var)
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
            Self::Abs(x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::abs(
                    name.clone(),
                    ty.clone(),
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
            Self::Abs(x, ty, t) => Ok(DeBruijnTerm::abs(
                x.clone(),
                ty.clone(),
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::App(t1, t2) => Ok(DeBruijnTerm::app(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
        }
    }
}
