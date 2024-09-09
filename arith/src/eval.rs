use crate::syntax::Term;
use std::rc::Rc;
use util::error::{Error, Result};

impl Term {
    fn is_numeric_val(&self) -> bool {
        self.is_int()
    }

    pub fn eval1(&self) -> Result<Rc<Self>> {
        match self {
            Self::If(t1, t2, t3) => match t1.as_ref() {
                Self::True => Ok(t2.clone()),
                Self::False => Ok(t3.clone()),
                _ => Ok(Self::if_(t1.eval1()?, t2.clone(), t3.clone())),
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
