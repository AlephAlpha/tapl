use crate::syntax::{Binding, Context, DeBruijnTerm, Term};
use std::rc::Rc;
use util::error::{Error, Result};

impl DeBruijnTerm {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    fn is_val(&self, _ctx: &Context) -> bool {
        match self {
            Self::String(_) | Self::Float(_) | Self::True | Self::False | Self::Abs(_, _) => true,
            Self::Record(fields) => fields.iter().all(|(_, t)| t.is_val(_ctx)),
            t => t.is_numeric_val(),
        }
    }

    pub fn eval1(&self, ctx: &Context) -> Result<Rc<Self>> {
        match self {
            Self::Var(i) => match ctx.get_binding_shifting(*i) {
                Ok(Binding::TermAbb(t)) => Ok(t),
                _ => Err(Error::NoRuleApplies),
            },
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1(ctx)?, t2.clone(), t3.clone())),
            },
            Self::Let(x, t1, t2) => {
                if t1.is_val(ctx) {
                    Ok(t2.subst_top(t1))
                } else {
                    Ok(Self::let_(x.clone(), t1.eval1(ctx)?, t2.clone()))
                }
            }
            Self::Record(fields) => {
                if let Some(i) = fields.iter().position(|(_, t)| !t.is_val(ctx)) {
                    let mut fields_ = fields.clone();
                    fields_[i].1 = fields[i].1.eval1(ctx)?;
                    Ok(Self::record(fields_))
                } else {
                    Err(Error::NoRuleApplies)
                }
            }
            Self::Proj(t, l) => match t.as_ref() {
                Self::Record(fields) => {
                    if let Some((_, t)) = fields.iter().find(|(l_, _)| l_ == l) {
                        Ok(t.clone())
                    } else {
                        Err(Error::NoRuleApplies)
                    }
                }
                _ => Ok(Self::proj(t.eval1(ctx)?, l.clone())),
            },
            Self::App(t1, t2) => match t1.as_ref() {
                Self::Abs(_, t) if t2.is_val(ctx) => Ok(t.subst_top(t2)),
                _ => {
                    if t1.is_val(ctx) {
                        Ok(Self::app(t1.clone(), t2.eval1(ctx)?))
                    } else {
                        Ok(Self::app(t1.eval1(ctx)?, t2.clone()))
                    }
                }
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
            Self::TimesFloat(t1, t2) => match t1.as_ref() {
                Self::Float(f1) => match t2.as_ref() {
                    Self::Float(f2) => Ok(Self::float(f1 * f2)),
                    _ => Ok(Self::times_float(t1.clone(), t2.eval1(ctx)?)),
                },
                _ => Ok(Self::times_float(t1.eval1(ctx)?, t2.clone())),
            },
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

impl Term {
    pub fn eval1(&self, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval1(ctx)?.to_named(ctx)
    }

    pub fn eval(self: &Rc<Self>, ctx: &mut Context) -> Result<Rc<Self>> {
        self.to_de_bruijn(ctx)?.eval(ctx).to_named(ctx)
    }
}
