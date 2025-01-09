use crate::syntax::{Binding, Context, DeBruijnTerm, Term, Ty};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::True | Self::False | Self::Abs(_, _, _) => true,
            t => t.is_numeric_val(),
        }
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, _, t) if t2.is_val(ctx) => Ok(t.subst_top(t2)),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
            },
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Succ(t) => Ok(Self::succ(t.eval1(ctx)?)),
            Self::Pred(t) => match t.as_ref() {
                Self::Zero => Ok(Self::zero()),
                Self::Succ(t) if t.is_numeric_val() => Ok(t.clone()),
                _ => Ok(Self::pred(t.eval1(ctx)?)),
            },
            Self::IsZero(t) => match t.as_ref() {
                Self::Zero => Ok(Self::true_()),
                Self::Succ(t) if t.is_numeric_val() => Ok(Self::false_()),
                _ => Ok(Self::is_zero(t.eval1(ctx)?)),
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    Ok(t2.subst_top(t1))
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx)?, t2.clone()))
                }
            }
            _ => Err(Error::NoRuleApplies),
        }
    }

    pub fn eval(self: &Rc<Self>, ctx: &Context) -> Rc<Self> {
        let mut t = Rc::clone(self);
        while let Ok(t_) = t.eval1(ctx) {
            t = t_;
        }
        t
    }
}

impl Ty {
    fn subst_in(&self, x: &str, s: &Rc<Self>) -> Rc<Self> {
        match self {
            Self::Arr(ty1, ty2) => Self::arr(ty1.subst_in(x, s), ty2.subst_in(x, s)),
            Self::Nat => Self::nat(),
            Self::Bool => Self::bool(),
            Self::Id(y) => {
                if x == y {
                    s.clone()
                } else {
                    Self::id(y.clone())
                }
            }
        }
    }

    fn occurs_in(&self, x: &str) -> bool {
        match self {
            Self::Arr(ty1, ty2) => ty1.occurs_in(x) || ty2.occurs_in(x),
            Self::Nat | Self::Bool => false,
            Self::Id(y) => x == y,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Substitution {
    subst: Vec<(String, Rc<Ty>)>,
}

impl Substitution {
    pub fn apply(&self, ty: &Rc<Ty>) -> Rc<Ty> {
        let mut ty = ty.clone();
        for (x, s) in &self.subst {
            ty = ty.subst_in(x, s);
        }
        ty
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Constr {
    constr: Vec<(Rc<Ty>, Rc<Ty>)>,
}

impl Constr {
    pub const fn new() -> Self {
        Self { constr: vec![] }
    }

    fn push(&mut self, c: (Rc<Ty>, Rc<Ty>)) {
        self.constr.push(c);
    }

    fn pop(&mut self) -> Option<(Rc<Ty>, Rc<Ty>)> {
        self.constr.pop()
    }

    fn subst_in(&mut self, x: &str, s: &Rc<Ty>) {
        for (ty1, ty2) in &mut self.constr {
            *ty1 = ty1.subst_in(x, s);
            *ty2 = ty2.subst_in(x, s);
        }
    }

    fn unify(&self) -> Result<Substitution> {
        let mut subst = Substitution::default();
        let mut constr = self.clone();

        while let Some((ty1, ty2)) = constr.pop() {
            if ty1 == ty2 {
                continue;
            }

            match (ty1.as_ref(), ty2.as_ref()) {
                (Ty::Arr(ty11, ty12), Ty::Arr(ty21, ty22)) => {
                    constr.push((ty11.clone(), ty21.clone()));
                    constr.push((ty12.clone(), ty22.clone()));
                }
                (Ty::Id(x), _) => {
                    if ty2.occurs_in(x) {
                        return Err(Error::UnificationError("Circular constraints".to_string()));
                    } else {
                        subst.subst.push((x.clone(), ty2.clone()));
                        constr.subst_in(x, &ty2);
                    }
                }
                (_, Ty::Id(x)) => {
                    if ty1.occurs_in(x) {
                        return Err(Error::UnificationError("Circular constraints".to_string()));
                    } else {
                        subst.subst.push((x.clone(), ty1.clone()));
                        constr.subst_in(x, &ty1);
                    }
                }
                _ => {
                    return Err(Error::UnificationError(
                        "Unsolvable constraints".to_string(),
                    ))
                }
            }
        }

        Ok(subst)
    }
}

impl From<Substitution> for Constr {
    fn from(subst: Substitution) -> Self {
        let mut constr = Self::default();
        for (x, s) in subst.subst {
            constr.push((Ty::id(x), s));
        }
        constr
    }
}

#[derive(Debug, Clone, Default)]
pub struct UVarGen {
    next: u32,
}

impl UVarGen {
    pub const fn new() -> Self {
        Self { next: 0 }
    }

    fn fresh(&mut self) -> Rc<Ty> {
        let ty = Ty::id(format!("?X{}", self.next));
        self.next += 1;
        ty
    }
}

impl DeBruijnTerm {
    fn recon(
        &self,
        ctx: &mut Context,
        uvar_gen: &mut UVarGen,
        constr: &mut Constr,
    ) -> Result<Rc<Ty>> {
        match self {
            Self::Var(i) => match ctx.get_binding(*i)? {
                Binding::Var(ty) => Ok(ty.clone()),
                _ => Err(Error::TypeError(format!(
                    "wrong kind of binding for variable {}",
                    ctx.index_to_name(*i).unwrap()
                ))),
            },
            Self::Abs(x, ty, t2) => {
                let ty1 = ty.clone().unwrap_or_else(|| uvar_gen.fresh());
                ctx.with_binding(x.clone(), Binding::Var(ty1.clone()), |ctx| {
                    let ty2 = t2.recon(ctx, uvar_gen, constr)?;
                    Ok(Ty::arr(ty1.clone(), ty2))
                })
            }
            Self::App(t1, t2) => {
                let ty1 = t1.recon(ctx, uvar_gen, constr)?;
                let ty2 = t2.recon(ctx, uvar_gen, constr)?;
                let ty3 = uvar_gen.fresh();
                constr.push((ty1, Ty::arr(ty2, ty3.clone())));
                Ok(ty3)
            }
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    let _ = t1.recon(ctx, uvar_gen, constr)?;
                    t2.subst_top(t1).recon(ctx, uvar_gen, constr)
                } else {
                    let ty1 = t1.recon(ctx, uvar_gen, constr)?;
                    ctx.with_binding(x.clone(), Binding::Var(ty1), |ctx| {
                        t2.recon(ctx, uvar_gen, constr)
                    })
                }
            }
            Self::Zero => Ok(Ty::nat()),
            Self::Succ(t) => {
                let ty = t.recon(ctx, uvar_gen, constr)?;
                constr.push((ty, Ty::nat()));
                Ok(Ty::nat())
            }
            Self::Pred(t) => {
                let ty = t.recon(ctx, uvar_gen, constr)?;
                constr.push((ty, Ty::nat()));
                Ok(Ty::nat())
            }
            Self::IsZero(t) => {
                let ty = t.recon(ctx, uvar_gen, constr)?;
                constr.push((ty, Ty::nat()));
                Ok(Ty::bool())
            }
            Self::True | Self::False => Ok(Ty::bool()),
            Self::If(t1, t2, t3) => {
                let ty1 = t1.recon(ctx, uvar_gen, constr)?;
                let ty2 = t2.recon(ctx, uvar_gen, constr)?;
                let ty3 = t3.recon(ctx, uvar_gen, constr)?;
                constr.push((ty1, Ty::bool()));
                constr.push((ty2, ty3.clone()));
                Ok(ty3)
            }
        }
    }

    pub fn type_of(
        &self,
        ctx: &mut Context,
        uvar_gen: &mut UVarGen,
        constr: &mut Constr,
    ) -> Result<Rc<Ty>> {
        let mut constr_ = constr.clone();
        let ty = self.recon(ctx, uvar_gen, &mut constr_)?;
        let subst = constr_.unify()?;
        *constr = subst.clone().into();
        Ok(subst.apply(&ty))
    }
}

impl Term {
    pub fn eval1(&self, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval1(ctx)?.to_named(ctx)
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval(ctx).to_named(ctx)
    }

    pub fn type_of(
        &self,
        ctx: &mut Context,
        uvar_gen: &mut UVarGen,
        constr: &mut Constr,
    ) -> Result<Rc<Ty>> {
        self.to_de_bruijn(ctx)?.type_of(ctx, uvar_gen, constr)
    }
}
