use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    BindingLinear, RcTerm,
    error::{Error, Result},
};

pub const KEYWORDS: &[&str] = &[
    "true", "false", "if", "then", "else", "lambda", "split", "as", "in", "lin", "un", "Bool",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Default)]
pub enum Qualifier {
    Lin,
    #[default]
    Un,
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum PreTy {
    Bool,
    Pair(Rc<Ty>, Rc<Ty>),
    Arr(Rc<Ty>, Rc<Ty>),
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub struct Ty {
    pub qualifier: Qualifier,
    pub pre_ty: Rc<PreTy>,
}

impl Ty {
    fn is_un(&self) -> bool {
        self.qualifier == Qualifier::Un
    }

    pub fn un(pre_ty: Rc<PreTy>) -> Rc<Self> {
        Self::new(Qualifier::Un, pre_ty)
    }
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Bool(Qualifier, bool),
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Pair(Qualifier, Rc<Self>, Rc<Self>),
    Split(
        Rc<Self>,
        #[rc_term(into)] String,
        #[rc_term(into)] String,
        Rc<Self>,
    ),
    Abs(Qualifier, #[rc_term(into)] String, Rc<Ty>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

impl<V> Term<V> {
    pub fn with_qualifier(self: Rc<Self>, qualifier: Qualifier) -> Rc<Self> {
        match self.as_ref() {
            Self::Bool(_, b) => Self::bool(qualifier, *b),
            Self::Pair(_, t1, t2) => Self::pair(qualifier, t1.clone(), t2.clone()),
            Self::Abs(_, x, ty, t) => Self::abs(qualifier, x.clone(), ty.clone(), t.clone()),
            _ => self,
        }
    }
}

pub type DeBruijnTerm = Term<usize>;

impl Display for Qualifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lin => write!(f, "lin"),
            Self::Un => write!(f, "un"),
        }
    }
}

impl PreTy {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_pair(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pair(t1, t2) => {
                t1.fmt_atom(f)?;
                write!(f, " * ")?;
                t2.fmt_atom(f)
            }
            t => t.fmt_atom(f),
        }
    }

    fn fmt_arrow(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arr(t1, t2) => {
                t1.fmt_pair(f)?;
                write!(f, " -> ")?;
                t2.fmt_arrow(f)
            }
            t => t.fmt_pair(f),
        }
    }
}

impl Display for PreTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_arrow(f)
    }
}

impl Ty {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.is_un() {
            write!(f, "{} ", self.qualifier)?;
        }
        self.pre_ty.fmt_atom(f)
    }

    fn fmt_pair(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_un() {
            self.pre_ty.fmt_pair(f)
        } else {
            self.fmt_atom(f)
        }
    }

    fn fmt_arrow(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_un() {
            self.pre_ty.fmt_arrow(f)
        } else {
            self.fmt_atom(f)
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
            Self::Bool(Qualifier::Un, b) => write!(f, "{b}"),
            Self::Pair(Qualifier::Un, t1, t2) => write!(f, "<{t1}, {t2}>"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(q, b) if *q != Qualifier::Un => write!(f, "{q} {b}"),
            Self::Pair(q, t1, t2) if *q != Qualifier::Un => write!(f, "{q} <{t1}, {t2}>"),
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
            Self::If(t1, t2, t3) => {
                write!(f, "if {t1} then {t2} else {t3}")
            }
            Self::Split(t1, x, y, t2) => {
                write!(f, "split {t1} as {x}, {y} in {t2}")
            }
            Self::Abs(Qualifier::Un, x, ty, t) => {
                write!(f, "lambda {x}: {ty}. {t}")
            }
            Self::Abs(q, x, ty, t) => {
                write!(f, "{q} lambda {x}: {ty}. {t}")
            }
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding {
    #[default]
    Name,
    Var(Rc<Ty>),
    Used,
}

pub type Context = util::Context<Binding>;

impl Binding {
    pub fn print_type(&self, x: &str) {
        match self {
            Self::Var(ty) => println!("{x} : {ty}"),
            _ => {}
        }
    }
}

impl BindingLinear for Binding {
    fn used(&self) -> Self {
        if self.is_unused_lin() {
            Self::Used
        } else {
            self.clone()
        }
    }

    fn is_used(&self) -> bool {
        matches!(self, Self::Used)
    }

    fn is_unused_lin(&self) -> bool {
        matches!(self, Self::Var(ty) if !ty.is_un())
    }
}

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
        f: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
    ) -> Result<Rc<Self>> {
        match self {
            Self::Var(x) => f(*x, cutoff),
            Self::Bool(q, b) => Ok(Self::bool(*q, *b)),
            Self::If(t1, t2, t3) => Ok(Self::if_(
                t1.map_vars_walk(cutoff, f)?,
                t2.map_vars_walk(cutoff, f)?,
                t3.map_vars_walk(cutoff, f)?,
            )),
            Self::Pair(q, t1, t2) => Ok(Self::pair(
                *q,
                t1.map_vars_walk(cutoff, f)?,
                t2.map_vars_walk(cutoff, f)?,
            )),
            Self::Split(t1, x, y, t2) => Ok(Self::split(
                t1.map_vars_walk(cutoff, f)?,
                x.clone(),
                y.clone(),
                t2.map_vars_walk(cutoff + 2, f)?,
            )),
            Self::Abs(q, x, ty, t) => Ok(Self::abs(
                *q,
                x.clone(),
                ty.clone(),
                t.map_vars_walk(cutoff + 1, f)?,
            )),
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
            Self::Bool(q, b) => Ok(Term::bool(*q, *b)),
            Self::If(t1, t2, t3) => Ok(Term::if_(
                t1.to_named(ctx)?,
                t2.to_named(ctx)?,
                t3.to_named(ctx)?,
            )),
            Self::Pair(q, t1, t2) => Ok(Term::pair(*q, t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::Split(t1, x, y, t2) => {
                let t1 = t1.to_named(ctx)?;
                let x_name = ctx.pick_fresh_name(x);
                let y_name = ctx.pick_fresh_name(y);
                Ok(Term::split(
                    t1,
                    x_name.clone(),
                    y_name.clone(),
                    ctx.with_name(x_name, |ctx| ctx.with_name(y_name, |ctx| t2.to_named(ctx)))?,
                ))
            }
            Self::Abs(q, x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::abs(
                    *q,
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
            Self::Bool(q, b) => Ok(DeBruijnTerm::bool(*q, *b)),
            Self::If(t1, t2, t3) => Ok(DeBruijnTerm::if_(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                t3.to_de_bruijn(ctx)?,
            )),
            Self::Pair(q, t1, t2) => Ok(DeBruijnTerm::pair(
                *q,
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::Split(t1, x, y, t2) => Ok(DeBruijnTerm::split(
                t1.to_de_bruijn(ctx)?,
                x.clone(),
                y.clone(),
                ctx.with_name(x.clone(), |ctx| {
                    ctx.with_name(y.clone(), |ctx| t2.to_de_bruijn(ctx))
                })?,
            )),
            Self::Abs(q, x, ty, t) => Ok(DeBruijnTerm::abs(
                *q,
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
