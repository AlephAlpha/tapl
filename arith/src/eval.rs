use crate::{
    error::{Error, Result},
    syntax::Term,
};
use std::rc::Rc;

impl Term {
    fn is_numeric_val(&self) -> bool {
        match self {
            Self::Zero => true,
            Self::Succ(t) => t.is_numeric_val(),
            _ => false,
        }
    }

    pub fn eval1(&self) -> Result<Rc<Self>> {
        match self {
            Self::If(cond, then, else_) => match cond.as_ref() {
                Self::True => Ok(then.clone()),
                Self::False => Ok(else_.clone()),
                _ => Ok(Self::if_(cond.eval1()?, then.clone(), else_.clone())),
            },
            Self::Succ(t) => Ok(Self::succ(t.eval1()?)),
            Self::Pred(t) => match t.as_ref() {
                Self::Zero => Ok(Self::zero()),
                Self::Succ(t) if t.is_numeric_val() => Ok(t.clone()),
                _ => Ok(Self::pred(t.eval1()?)),
            },
            Self::IsZero(t) => match t.as_ref() {
                Self::Zero => Ok(Self::true_()),
                Self::Succ(t) if t.is_numeric_val() => Ok(Self::false_()),
                _ => Ok(Self::is_zero(t.eval1()?)),
            },
            _ => Err(Error::NoRuleApplies),
        }
    }

    pub fn eval(self: &Rc<Self>) -> Rc<Self> {
        let mut t = Rc::clone(self);
        while let Ok(t_) = t.eval1() {
            t = t_;
        }
        t
    }
}
