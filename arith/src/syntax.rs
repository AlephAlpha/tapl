use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
}

impl Term {
    fn to_num(&self) -> Option<u64> {
        let mut t = self;
        let mut n = 0;
        while let Term::Succ(t_) = t {
            t = t_;
            n += 1;
        }
        if let Term::Zero = t {
            Some(n)
        } else {
            None
        }
    }

    fn fmt_atom(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(n) = self.to_num() {
            return write!(f, "{n}");
        }

        match self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            t => write!(f, "({t})"),
        }
    }

    fn fmt_app(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(n) = self.to_num() {
            return write!(f, "{n}");
        }

        match self {
            Term::Succ(t) => {
                write!(f, "succ ")?;
                t.fmt_atom(f)
            }
            Term::Pred(t) => {
                write!(f, "pred ")?;
                t.fmt_atom(f)
            }
            Term::IsZero(t) => {
                write!(f, "iszero ")?;
                t.fmt_atom(f)
            }
            t => t.fmt_atom(f),
        }
    }

    fn fmt_if(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Term::If(cond, then, else_) = self {
            write!(f, "if ")?;
            cond.fmt_app(f)?;
            write!(f, " then ")?;
            then.fmt_app(f)?;
            write!(f, " else ")?;
            else_.fmt_app(f)
        } else {
            self.fmt_app(f)
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_if(f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Eval1(Term),
    Eval(Term),
}
