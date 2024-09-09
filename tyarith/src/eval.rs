use crate::syntax::{Term, Ty};
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

    pub fn type_of(&self) -> Result<Ty> {
        match self {
            Self::True | Self::False => Ok(Ty::Bool),
            Self::Zero => Ok(Ty::Nat),
            Self::If(t1, t2, t3) => {
                if t1.type_of()? == Ty::Bool {
                    let ty2 = t2.type_of()?;
                    if ty2 == t3.type_of()? {
                        Ok(ty2)
                    } else {
                        Err(Error::TypeError(
                            "arms of conditional have different types".to_string(),
                        ))
                    }
                } else {
                    Err(Error::TypeError(
                        "guard of conditional not a boolean".to_string(),
                    ))
                }
            }
            Self::Succ(t) => {
                if t.type_of()? == Ty::Nat {
                    Ok(Ty::Nat)
                } else {
                    Err(Error::TypeError(
                        "argument of succ is not a number".to_string(),
                    ))
                }
            }
            Self::Pred(t) => {
                if t.type_of()? == Ty::Nat {
                    Ok(Ty::Nat)
                } else {
                    Err(Error::TypeError(
                        "argument of pred is not a number".to_string(),
                    ))
                }
            }
            Self::IsZero(t) => {
                if t.type_of()? == Ty::Nat {
                    Ok(Ty::Bool)
                } else {
                    Err(Error::TypeError(
                        "argument of iszero is not a number".to_string(),
                    ))
                }
            }
        }
    }
}
