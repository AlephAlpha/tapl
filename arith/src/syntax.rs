use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

pub const KEYWORDS: &[&str] = &[
    "true", "false", "if", "then", "else", "succ", "pred", "iszero",
];
pub const COMMANDS: &[&str] = &["eval", "eval1"];

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    True,
    False,
    If(Rc<Self>, Rc<Self>, Rc<Self>),
    Zero,
    Succ(Rc<Self>),
    Pred(Rc<Self>),
    IsZero(Rc<Self>),
}

// TODO: make a macro for generating these functions
impl Term {
    pub fn true_() -> Rc<Self> {
        Rc::new(Self::True)
    }

    pub fn false_() -> Rc<Self> {
        Rc::new(Self::False)
    }

    pub fn if_(cond: Rc<Self>, then: Rc<Self>, else_: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::If(cond, then, else_))
    }

    pub fn zero() -> Rc<Self> {
        Rc::new(Self::Zero)
    }

    pub fn succ(t: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Succ(t))
    }

    pub fn pred(t: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Pred(t))
    }

    pub fn is_zero(t: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::IsZero(t))
    }
}

impl Term {
    pub fn to_num(&self) -> Option<u64> {
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

    pub fn from_num(n: u64) -> Rc<Self> {
        let mut t = Self::zero();
        for _ in 0..n {
            t = Self::succ(t);
        }
        t
    }
}

impl Term {
    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(n) = self.to_num() {
            return write!(f, "{n}");
        }

        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(n) = self.to_num() {
            return write!(f, "{n}");
        }

        match self {
            Self::Succ(t) => {
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
        if let Self::If(cond, then, else_) = self {
            write!(f, "if {cond} then {then} else {else_}")
        } else {
            self.fmt_app(f)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Rc<Term>),
    Eval(Rc<Term>),
}
