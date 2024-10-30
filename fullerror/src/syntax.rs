use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{error::Result, BindingShift, RcTerm};

pub const KEYWORDS: &[&str] = &[
    "true", "false", "error", "lambda", "if", "then", "else", "try", "with", "_", "Bot", "Top",
    "Bool",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "bindtype", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty<V = String> {
    Bot,
    Top,
    Var(#[rc_term(into)] V),
    Arr(Rc<Self>, Rc<Self>),
    Bool,
}

pub type DeBruijnTy = Ty<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Error,
    Try(Rc<Self>, Rc<Self>),
}

pub type DeBruijnTerm = Term<usize>;

impl<V: Display> Ty<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bot => write!(f, "Bot"),
            Self::Top => write!(f, "Top"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(x) => write!(f, "{x}"),
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
            Self::Var(x) => write!(f, "{x}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Error => write!(f, "error"),
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
            Self::If(t1, t2, t3) => write!(f, "if {t1} then {t2} else {t3}"),
            Self::Try(t1, t2) => write!(f, "try {t1} with {t2}"),
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
        on_var: &mut impl FnMut(usize, usize) -> Rc<Self>,
    ) -> Rc<Self> {
        match self {
            Self::Bot => Self::bot(),
            Self::Arr(ty1, ty2) => Self::arr(
                ty1.map_vars_walk(cutoff, on_var),
                ty2.map_vars_walk(cutoff, on_var),
            ),
            Self::Top => Self::top(),
            Self::Bool => Self::bool(),
            Self::Var(x) => on_var(cutoff, *x),
        }
    }

    fn map_vars(&self, cutoff: usize, on_var: impl FnMut(usize, usize) -> Rc<Self>) -> Rc<Self> {
        let mut on_var = on_var;
        self.map_vars_walk(cutoff, &mut on_var)
    }

    fn shift_above(&self, d: isize, cutoff: usize) -> Rc<Self> {
        self.map_vars(cutoff, |c, x| {
            if x >= c {
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
}

impl DeBruijnTerm {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Rc<Self>,
        on_type: &mut impl FnMut(usize, &Rc<DeBruijnTy>) -> Rc<DeBruijnTy>,
    ) -> Rc<Self> {
        match self {
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
            Self::Error => Self::error(),
            Self::Try(t1, t2) => Self::try_(
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff, on_var, on_type),
            ),
            Self::True => Self::true_(),
            Self::False => Self::false_(),
            Self::If(t1, t2, t3) => Self::if_(
                t1.map_vars_walk(cutoff, on_var, on_type),
                t2.map_vars_walk(cutoff, on_var, on_type),
                t3.map_vars_walk(cutoff, on_var, on_type),
            ),
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
            0,
            |c, x| {
                if x == j + c {
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
}

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Self {
        match self {
            Self::Name => Self::Name,
            Self::TermAbb(t, ty) => Self::TermAbb(t.shift(d), ty.as_ref().map(|ty| ty.shift(d))),
            Self::Var(ty) => Self::Var(ty.shift(d)),
            Self::TyVar => Self::TyVar,
            Self::TyAbb(ty) => Self::TyAbb(ty.shift(d)),
        }
    }
}

impl DeBruijnTy {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Bot => Ok(Ty::bot()),
            Self::Top => Ok(Ty::top()),
            Self::Var(x) => Ok(Ty::var(ctx.index_to_name(*x)?)),
            Self::Arr(ty1, ty2) => Ok(Ty::arr(ty1.to_named(ctx)?, ty2.to_named(ctx)?)),
            Self::Bool => Ok(Ty::bool()),
        }
    }
}

impl Ty {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Bot => Ok(DeBruijnTy::bot()),
            Self::Top => Ok(DeBruijnTy::top()),
            Self::Var(x) => Ok(DeBruijnTy::var(ctx.name_to_index(x)?)),
            Self::Arr(ty1, ty2) => Ok(DeBruijnTy::arr(
                ty1.to_de_bruijn(ctx)?,
                ty2.to_de_bruijn(ctx)?,
            )),
            Self::Bool => Ok(DeBruijnTy::bool()),
        }
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
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::App(t1, t2) => Ok(Term::app(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::True => Ok(Term::true_()),
            Self::False => Ok(Term::false_()),
            Self::If(t1, t2, t3) => Ok(Term::if_(
                t1.to_named(ctx)?,
                t2.to_named(ctx)?,
                t3.to_named(ctx)?,
            )),
            Self::Error => Ok(Term::error()),
            Self::Try(t1, t2) => Ok(Term::try_(t1.to_named(ctx)?, t2.to_named(ctx)?)),
        }
    }
}

impl Term {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTerm>> {
        match self {
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
            Self::True => Ok(DeBruijnTerm::true_()),
            Self::False => Ok(DeBruijnTerm::false_()),
            Self::If(t1, t2, t3) => Ok(DeBruijnTerm::if_(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
                t3.to_de_bruijn(ctx)?,
            )),
            Self::Error => Ok(DeBruijnTerm::error()),
            Self::Try(t1, t2) => Ok(DeBruijnTerm::try_(
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
            Self::Var(ty) => Ok(DeBruijnBinding::Var(ty.to_de_bruijn(ctx)?)),
            Self::TermAbb(t, ty) => {
                let ty_ = ty.clone().map_or_else(|| t.type_of(ctx), Ok)?;
                Ok(DeBruijnBinding::TermAbb(
                    t.to_de_bruijn(ctx)?,
                    Some(ty_.to_de_bruijn(ctx)?),
                ))
            }
            Self::TyVar => Ok(DeBruijnBinding::TyVar),
            Self::TyAbb(ty) => Ok(DeBruijnBinding::TyAbb(ty.to_de_bruijn(ctx)?)),
        }
    }
}
