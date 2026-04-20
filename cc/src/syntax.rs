use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    BindingShift, RcTerm,
    error::{Error, Result},
};

pub const KEYWORDS: &[&str] = &["lambda", "all", "_", "Pi", "Prop", "Prf"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type", "kind"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Kind<V = String> {
    Star,
    Pi(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
}

pub type DeBruijnKind = Kind<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty<V = String> {
    Var(#[rc_term(into)] V),
    Pi(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
    App(Rc<Self>, Rc<Term<V>>),
    Prop,
    Prf,
}

pub type DeBruijnTy = Ty<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    All(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
}

pub type DeBruijnTerm = Term<usize>;

impl<V: Display> Display for Kind<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Pi(x, ty, kn) => {
                if x == "_" {
                    ty.fmt_app(f)?;
                    write!(f, " -> {kn}")
                } else {
                    write!(f, "Pi {x}: {ty}. {kn}")
                }
            }
        }
    }
}

impl<V: Display> Ty<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{x}"),
            Self::Prop => write!(f, "Prop"),
            Self::Prf => write!(f, "Prf"),
            ty => write!(f, "({ty})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::App(ty, t) => {
                ty.fmt_app(f)?;
                write!(f, " ")?;
                t.fmt_atom(f)
            }
            ty => ty.fmt_atom(f),
        }
    }

    fn fmt_arrow(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pi(x, ty1, ty2) if x == "_" => {
                ty1.fmt_arrow(f)?;
                write!(f, " -> ")?;
                ty2.fmt_arrow(f)
            }
            ty => ty.fmt_app(f),
        }
    }
}

impl<V: Display> Display for Ty<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pi(x, ty1, ty2) if x != "_" => write!(f, "Pi {x}: {ty1}. {ty2}"),
            ty => ty.fmt_arrow(f),
        }
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
            t => t.fmt_atom(f),
        }
    }
}

impl<V: Display> Display for Term<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abs(x, ty, t) => write!(f, "lambda {x}: {ty}. {t}"),
            Self::All(x, ty, t) => write!(f, "all {x}: {ty}. {t}"),
            t => t.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding<V = String> {
    #[default]
    Name,
    Var(Rc<Ty<V>>),
    TermAbb(Rc<Term<V>>, Option<Rc<Ty<V>>>),
    TyVar(Rc<Kind<V>>),
}

pub type DeBruijnBinding = Binding<usize>;
pub type Context = util::Context<DeBruijnBinding>;

impl<V: Display> Binding<V> {
    pub fn print_type(&self, x: &str) {
        match self {
            Self::Var(ty) => println!("{x} : {ty}"),
            Self::TermAbb(_, Some(ty)) => println!("{x} : {ty}"),
            Self::TyVar(kn) => println!("{x} :: {kn}"),
            _ => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Bind(String, Binding),
    Type(Rc<Term>),
    Kind(Rc<Ty>),
    Noop,
}

impl DeBruijnKind {
    fn map_types_walk(
        &self,
        cutoff: usize,
        on_type: &mut impl FnMut(usize, &Rc<DeBruijnTy>) -> Result<Rc<DeBruijnTy>>,
    ) -> Result<Rc<Self>> {
        Ok(match self {
            Self::Star => Self::star(),
            Self::Pi(x, ty, kn) => Self::pi(
                x.clone(),
                on_type(cutoff, ty)?,
                kn.map_types_walk(cutoff + 1, on_type)?,
            ),
        })
    }

    fn map_types(
        &self,
        cutoff: usize,
        on_type: impl FnMut(usize, &Rc<DeBruijnTy>) -> Result<Rc<DeBruijnTy>>,
    ) -> Result<Rc<Self>> {
        let mut on_type = on_type;
        self.map_types_walk(cutoff, &mut on_type)
    }

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.map_types(0, |c, ty| ty.shift_above(d, c))
    }

    fn subst_term(&self, j: usize, s: &DeBruijnTerm) -> Result<Rc<Self>> {
        self.map_types(j, |c, t| t.subst_term(c, s))
    }

    pub fn subst_top_term(&self, s: &DeBruijnTerm) -> Result<Rc<Self>> {
        self.subst_term(0, s.shift(1)?.as_ref())?.shift(-1)
    }
}

impl DeBruijnTy {
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
        on_term: &mut impl FnMut(usize, &Rc<DeBruijnTerm>) -> Result<Rc<DeBruijnTerm>>,
    ) -> Result<Rc<Self>> {
        Ok(match self {
            Self::Var(x) => on_var(cutoff, *x)?,
            Self::Pi(x, ty1, ty2) => Self::pi(
                x.clone(),
                ty1.map_vars_walk(cutoff, on_var, on_term)?,
                ty2.map_vars_walk(cutoff + 1, on_var, on_term)?,
            ),
            Self::App(ty, t) => Self::app(
                ty.map_vars_walk(cutoff, on_var, on_term)?,
                on_term(cutoff, t)?,
            ),
            Self::Prop => Self::prop(),
            Self::Prf => Self::prf(),
        })
    }

    fn map_vars(
        &self,
        cutoff: usize,
        on_var: impl FnMut(usize, usize) -> Result<Rc<Self>>,
        on_term: impl FnMut(usize, &Rc<DeBruijnTerm>) -> Result<Rc<DeBruijnTerm>>,
    ) -> Result<Rc<Self>> {
        let mut on_var = on_var;
        let mut on_term = on_term;
        self.map_vars_walk(cutoff, &mut on_var, &mut on_term)
    }

    fn shift_above(&self, d: isize, cutoff: usize) -> Result<Rc<Self>> {
        self.map_vars(
            cutoff,
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
            |c, t| t.shift_above(d, c),
        )
    }

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.shift_above(d, 0)
    }

    fn subst_term(&self, j: usize, s: &DeBruijnTerm) -> Result<Rc<Self>> {
        self.map_vars(j, |_, x| Ok(Self::var(x)), |c, t| t.subst(c, s))
    }

    pub fn subst_top_term(&self, s: &DeBruijnTerm) -> Result<Rc<Self>> {
        self.subst_term(0, s.shift(1)?.as_ref())?.shift(-1)
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
            Self::All(x, ty, t) => Self::all(
                x.clone(),
                on_type(cutoff, ty)?,
                t.map_vars_walk(cutoff + 1, on_var, on_type)?,
            ),
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

    fn shift_above(&self, d: isize, cutoff: usize) -> Result<Rc<Self>> {
        self.map_vars(
            cutoff,
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

    pub fn shift(&self, d: isize) -> Result<Rc<Self>> {
        self.shift_above(d, 0)
    }

    fn subst(&self, j: usize, s: &Self) -> Result<Rc<Self>> {
        self.map_vars(
            j,
            |c, x| {
                if x == c {
                    s.shift(c as isize)
                } else {
                    Ok(Self::var(x))
                }
            },
            |c, ty| ty.subst_term(c, s),
        )
    }

    pub fn subst_top(&self, s: &Self) -> Result<Rc<Self>> {
        self.subst(0, s.shift(1)?.as_ref())?.shift(-1)
    }
}

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Result<Self> {
        Ok(match self {
            Self::Name => Self::Name,
            Self::TermAbb(t, ty) => {
                Self::TermAbb(t.shift(d)?, ty.as_ref().map(|ty| ty.shift(d)).transpose()?)
            }
            Self::Var(ty) => Self::Var(ty.shift(d)?),
            Self::TyVar(kn) => Self::TyVar(kn.shift(d)?),
        })
    }
}

impl DeBruijnKind {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Kind>> {
        match self {
            Self::Star => Ok(Kind::star()),
            Self::Pi(x, ty, kn) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Kind::pi(
                    name.clone(),
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| kn.to_named(ctx))?,
                ))
            }
        }
    }
}

impl Kind {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnKind>> {
        match self {
            Self::Star => Ok(DeBruijnKind::star()),
            Self::Pi(x, ty, kn) => Ok(DeBruijnKind::pi(
                x.clone(),
                ty.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| kn.to_de_bruijn(ctx))?,
            )),
        }
    }
}

impl DeBruijnTy {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Var(x) => Ok(Ty::var(ctx.index_to_name(*x)?)),
            Self::Pi(x, ty1, ty2) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Ty::pi(
                    name.clone(),
                    ty1.to_named(ctx)?,
                    ctx.with_name(name, |ctx| ty2.to_named(ctx))?,
                ))
            }
            Self::App(ty, t) => Ok(Ty::app(ty.to_named(ctx)?, t.to_named(ctx)?)),
            Self::Prop => Ok(Ty::prop()),
            Self::Prf => Ok(Ty::prf()),
        }
    }
}

impl Ty {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Var(x) => Ok(DeBruijnTy::var(ctx.name_to_index(x)?)),
            Self::Pi(x, ty1, ty2) => Ok(DeBruijnTy::pi(
                x.clone(),
                ty1.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| ty2.to_de_bruijn(ctx))?,
            )),
            Self::App(ty, t) => Ok(DeBruijnTy::app(ty.to_de_bruijn(ctx)?, t.to_de_bruijn(ctx)?)),
            Self::Prop => Ok(DeBruijnTy::prop()),
            Self::Prf => Ok(DeBruijnTy::prf()),
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
            Self::All(x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::all(
                    name.clone(),
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
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
            Self::All(x, ty, t) => Ok(DeBruijnTerm::all(
                x.clone(),
                ty.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
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
            Self::TyVar(kn) => Ok(DeBruijnBinding::TyVar(kn.to_de_bruijn(ctx)?)),
        }
    }
}

impl DeBruijnBinding {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Binding> {
        match self {
            Self::Name => Ok(Binding::Name),
            Self::Var(ty) => Ok(Binding::Var(ty.to_named(ctx)?)),
            Self::TermAbb(t, ty) => Ok(Binding::TermAbb(
                t.to_named(ctx)?,
                ty.as_ref().map(|ty| ty.to_named(ctx)).transpose()?,
            )),
            Self::TyVar(kn) => Ok(Binding::TyVar(kn.to_named(ctx)?)),
        }
    }
}
