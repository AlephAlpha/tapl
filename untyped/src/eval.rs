use crate::{
    error::{Error, Result},
    syntax::{Context, DeBruijnTerm, Term},
};
use std::rc::Rc;

impl DeBruijnTerm {
    const fn is_val(&self) -> bool {
        matches!(self, Self::Abs(_, _))
    }

    pub fn eval1(&self, _ctx: &mut Context) -> Result<Rc<Self>> {
        match self {
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, t) if t2.is_val() => Ok(t.subst_top(t2)),
                _ if t1.is_val() => Ok(Self::app(t1.clone(), t2.eval1(_ctx)?)),
                _ => Ok(Self::app(t1.eval1(_ctx)?, t2.clone())),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context) -> Rc<Self> {
        let mut t = Rc::clone(self);
        while let Ok(t_) = t.eval1(ctx) {
            t = t_;
        }
        t
    }
}

impl Term {
    pub fn eval1(&self, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_debruijn(ctx)?.eval1(ctx)?.to_term(ctx)
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_debruijn(ctx)?.eval(ctx).to_term(ctx)
    }
}
