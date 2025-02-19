use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::{
    error::{Error, Result},
    BindingShift, RcTerm,
};

pub const KEYWORDS: &[&str] = &["lambda", "_", "Top", "Nat", "All"];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Ty<V = String> {
    Top,
    Var(#[rc_term(into)] V),
    Arr(Rc<Self>, Rc<Self>),
    All(#[rc_term(into)] String, Rc<Self>, Rc<Self>),
}

pub type DeBruijnTy = Ty<usize>;

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term<V = String> {
    Var(#[rc_term(into)] V),
    Abs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    App(Rc<Self>, Rc<Self>),
    TAbs(#[rc_term(into)] String, Rc<Ty<V>>, Rc<Self>),
    TApp(Rc<Self>, Rc<Ty<V>>),
}

pub type DeBruijnTerm = Term<usize>;

impl<V: Display> Ty<V> {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Top => write!(f, "Top"),
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
            Self::TApp(t, ty) => {
                t.fmt_app(f)?;
                write!(f, " [{ty}]")
            }
            _ => self.fmt_atom(f),
        }
    }
}

impl<V: Display> Display for Term<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
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
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Binding<V = String> {
    #[default]
    Name,
    Var(Rc<Ty<V>>),
    TyVar(Rc<Ty<V>>),
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
        match self {
            Self::Top => Ok(Self::top()),
            Self::Var(x) => on_var(cutoff, *x),
            Self::All(x, ty1, ty2) => Ok(Self::all(
                x.clone(),
                ty1.map_vars_walk(cutoff, on_var)?,
                ty2.map_vars_walk(cutoff + 1, on_var)?,
            )),
            Self::Arr(t1, t2) => Ok(Self::arr(
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

    fn subst(&self, j: usize, s: &Self) -> Result<Rc<Self>> {
        self.map_vars(j, |c, x| {
            if x == c {
                s.shift(c as isize)?.as_ref().shift(0)
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
    fn map_vars_walk(
        &self,
        cutoff: usize,
        on_var: &mut impl FnMut(usize, usize) -> Result<Rc<Self>>,
        on_type: &mut impl FnMut(usize, &Rc<DeBruijnTy>) -> Result<Rc<DeBruijnTy>>,
    ) -> Result<Rc<Self>> {
        match self {
            Self::Var(x) => on_var(cutoff, *x),
            Self::Abs(x, ty, t) => Ok(Self::abs(
                x.clone(),
                on_type(cutoff, ty)?,
                t.map_vars_walk(cutoff + 1, on_var, on_type)?,
            )),
            Self::App(t1, t2) => Ok(Self::app(
                t1.map_vars_walk(cutoff, on_var, on_type)?,
                t2.map_vars_walk(cutoff, on_var, on_type)?,
            )),
            Self::TAbs(x, ty, t) => Ok(Self::t_abs(
                x.clone(),
                on_type(cutoff, ty)?,
                t.map_vars_walk(cutoff + 1, on_var, on_type)?,
            )),
            Self::TApp(t, ty) => Ok(Self::t_app(
                t.map_vars_walk(cutoff, on_var, on_type)?,
                on_type(cutoff, ty)?,
            )),
        }
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
            j,
            |c, x| {
                if x == c {
                    s.shift(c as isize)?.as_ref().shift(0)
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

    fn subst_ty(&self, j: usize, ty: &DeBruijnTy) -> Result<Rc<Self>> {
        self.map_vars(j, |_, x| Ok(Self::var(x)), |c, ty_| ty_.subst(c, ty))
    }

    pub fn subst_top_ty(&self, ty: &DeBruijnTy) -> Result<Rc<Self>> {
        self.subst_ty(0, ty.shift(1)?.as_ref())?.shift(-1)
    }
}

impl BindingShift for DeBruijnBinding {
    fn shift(&self, d: isize) -> Result<Self> {
        match self {
            Self::Name => Ok(Self::Name),
            Self::Var(ty) => Ok(Self::Var(ty.shift(d)?)),
            Self::TyVar(ty) => Ok(Self::TyVar(ty.shift(d)?)),
        }
    }
}

impl DeBruijnTy {
    pub fn to_named(&self, ctx: &mut Context) -> Result<Rc<Ty>> {
        match self {
            Self::Top => Ok(Ty::top()),
            Self::Var(x) => Ok(Ty::var(ctx.index_to_name(*x)?)),
            Self::Arr(t1, t2) => Ok(Ty::arr(t1.to_named(ctx)?, t2.to_named(ctx)?)),
            Self::All(x, ty1, ty2) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Ty::all(
                    name.clone(),
                    ty1.to_named(ctx)?,
                    ctx.with_name(name, |ctx| ty2.to_named(ctx))?,
                ))
            }
        }
    }
}

impl Ty {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<Rc<DeBruijnTy>> {
        match self {
            Self::Top => Ok(DeBruijnTy::top()),
            Self::Var(x) => Ok(DeBruijnTy::var(ctx.name_to_index(x)?)),
            Self::Arr(t1, t2) => Ok(DeBruijnTy::arr(
                t1.to_de_bruijn(ctx)?,
                t2.to_de_bruijn(ctx)?,
            )),
            Self::All(x, ty1, ty2) => Ok(DeBruijnTy::all(
                x.clone(),
                ty1.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| ty2.to_de_bruijn(ctx))?,
            )),
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
            Self::TAbs(x, ty, t) => {
                let name = ctx.pick_fresh_name(x);
                Ok(Term::t_abs(
                    name.clone(),
                    ty.to_named(ctx)?,
                    ctx.with_name(name, |ctx| t.to_named(ctx))?,
                ))
            }
            Self::TApp(t, ty) => Ok(Term::t_app(t.to_named(ctx)?, ty.to_named(ctx)?)),
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
            Self::TAbs(x, ty, t) => Ok(DeBruijnTerm::t_abs(
                x.clone(),
                ty.to_de_bruijn(ctx)?,
                ctx.with_name(x.clone(), |ctx| t.to_de_bruijn(ctx))?,
            )),
            Self::TApp(t, ty) => Ok(DeBruijnTerm::t_app(
                t.to_de_bruijn(ctx)?,
                ty.to_de_bruijn(ctx)?,
            )),
        }
    }
}

impl Binding {
    pub fn to_de_bruijn(&self, ctx: &mut Context) -> Result<DeBruijnBinding> {
        match self {
            Self::Name => Ok(DeBruijnBinding::Name),
            Self::Var(ty) => Ok(DeBruijnBinding::Var(ty.to_de_bruijn(ctx)?)),
            Self::TyVar(ty) => Ok(DeBruijnBinding::TyVar(ty.to_de_bruijn(ctx)?)),
        }
    }
}
