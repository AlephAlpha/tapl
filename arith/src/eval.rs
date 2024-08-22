use crate::syntax::Term;
use either::{Either, Left, Right};

impl Term {
    fn is_numeric_val(&self) -> bool {
        match self {
            Term::Zero => true,
            Term::Succ(t) => t.is_numeric_val(),
            _ => false,
        }
    }

    // Returns either the next term or the same term if no reduction is possible.
    pub fn eval1(self) -> Either<Term, Term> {
        match self {
            Term::If(cond, then, else_) => match *cond {
                Term::True => Left(*then),
                Term::False => Left(*else_),
                _ => cond
                    .eval1()
                    .map(|cond_| Term::If(Box::new(cond_), then, else_)),
            },
            Term::Succ(t) => t.eval1().map(|t_| Term::Succ(Box::new(t_))),
            Term::Pred(t) => match *t {
                Term::Zero => Left(Term::Zero),
                Term::Succ(t) if t.is_numeric_val() => Left(*t),
                _ => t.eval1().map(|t_| Term::Pred(Box::new(t_))),
            },
            Term::IsZero(t) => match *t {
                Term::Zero => Left(Term::True),
                Term::Succ(t) if t.is_numeric_val() => Left(Term::False),
                _ => t.eval1().map(|t_| Term::IsZero(Box::new(t_))),
            },
            _ => Right(self),
        }
    }

    pub fn eval(self) -> Term {
        let mut t = self;
        loop {
            match t.eval1() {
                Left(t_) => t = t_,
                Right(t_) => return t_,
            }
        }
    }
}
