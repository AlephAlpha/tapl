use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{error::Result, RcTerm};

pub const KEYWORDS: &[&str] = &["lambda", "_", "Bot", "Top"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty {
    Bot,
    Top,
    Arr(Rc<Self>, Rc<Self>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    Record(#[rc_term(into)] Vec<(String, Rc<Self>)>),
    Proj(Rc<Self>, #[rc_term(into)] String),
}

pub type DeBruijnTerm = Term<usize>;

impl Ty {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bot => write!(f, "Bot"),
            Self::Top => write!(f, "Top"),
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
            _ => self.fmt_path(f),
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
        on_var: &mut impl FnMut(usize, usize) -> Rc<Self>,
    ) -> Rc<Self> {
        match self {
            Self::Var(x) => on_var(cutoff, *x),
            Self::Abs(x, ty, t) => {
                Self::abs(x.clone(), ty.clone(), t.map_vars_walk(cutoff + 1, on_var))
            }
            Self::App(t1, t2) => Self::app(
                t1.map_vars_walk(cutoff, on_var),
                t2.map_vars_walk(cutoff, on_var),
            ),
            Self::Proj(t, l) => Self::proj(t.map_vars_walk(cutoff, on_var), l.clone()),
            Self::Record(fields) => Self::record(
                fields
                    .iter()
                    .map(|(label, term)| (label.clone(), term.map_vars_walk(cutoff, on_var)))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    fn map_vars(&self, cutoff: usize, on_var: impl FnMut(usize, usize) -> Rc<Self>) -> Rc<Self> {
        let mut on_var = on_var;
        self.map_vars_walk(cutoff, &mut on_var)
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
            Self::Record(fields) => Ok(Term::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_named(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(Term::proj(t.to_named(ctx)?, l.clone())),
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
            Self::Record(fields) => Ok(DeBruijnTerm::record(
                fields
                    .iter()
                    .map(|(label, term)| Ok((label.clone(), term.to_de_bruijn(ctx)?)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            Self::Proj(t, l) => Ok(DeBruijnTerm::proj(t.to_de_bruijn(ctx)?, l.clone())),
        }
    }
}
