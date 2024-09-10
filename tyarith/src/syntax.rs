use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use util::RcTerm;

pub const KEYWORDS: &[&str] = &[
    "true", "false", "if", "then", "else", "succ", "pred", "iszero", "Bool", "Nat",
];
pub const COMMANDS: &[&str] = &["eval", "eval1", "bind", "type"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Bool,
    Nat,
}

#[derive(Clone, Debug, PartialEq, RcTerm)]
pub enum Term {
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Zero,
    Succ(Rc<Self>),
    Pred(Rc<Self>),
    IsZero(Rc<Self>),
}

impl Term {
    pub fn is_int(&self) -> bool {
        match self {
            Self::Zero => true,
            Self::Succ(t) => t.is_int(),
            _ => false,
        }
    }

    pub fn to_int(&self) -> Option<u64> {
        let mut t = self;
        let mut n = 0;
        while let Self::Succ(t_) = t {
            t = t_;
            n += 1;
        }
        if matches!(t, Self::Zero) {
            Some(n)
        } else {
            None
        }
    }

    pub fn from_int(n: u64) -> Rc<Self> {
        let mut t = Self::zero();
        for _ in 0..n {
            t = Self::succ(t);
        }
        t
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Nat => write!(f, "Nat"),
        }
    }
}

impl Term {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Zero => write!(f, "0"),
            Self::Succ(t) if t.is_int() => write!(f, "{}", t.to_int().unwrap() + 1),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Succ(t) if !t.is_int() => {
                write!(f, "succ ")?;
                t.fmt_atom(f)
            }
            Self::Pred(t) => {
                write!(f, "pred ")?;
                t.fmt_atom(f)
            }
            Self::IsZero(t) => {
                write!(f, "iszero ")?;
                t.fmt_atom(f)
            }
            t => t.fmt_atom(f),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::If(t1, t2, t3) => write!(f, "if {t1} then {t2} else {t3}"),
            _ => self.fmt_app(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
    Type(Rc<Term>),
    Noop,
}
