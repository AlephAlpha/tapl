use crate::syntax::{Command, Term, Ty, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Ty {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|ty| {
            let bool_ = text::keyword("Bool").to(Self::bool());

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = bool_.or(parens);

            let arrow = atom
                .clone()
                .padded()
                .then_ignore(just("->"))
                .repeated()
                .then(atom.padded())
                .foldr(Self::arr);

            arrow.padded()
        })
    }
}

impl Term {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|term| {
            let var = util::parser::ident(KEYWORDS.iter().copied()).map(Self::var);

            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, parens, var));

            let app = atom
                .clone()
                .then(atom.clone().padded().repeated())
                .foldl(Self::app);

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let abs = text::keyword("lambda")
                .ignore_then(text::ident().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(x, ty, t));

            choice((if_, abs, app)).padded()
        })
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Simple<char>>> {
        Self::parser().parse(input)
    }

    fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let term = Term::parser().map(Self::Eval);
        let eval1 = just(':')
            .then(text::keyword("eval1"))
            .ignore_then(Term::parser())
            .map(Self::Eval1);
        let eval = just(':')
            .then(text::keyword("eval"))
            .ignore_then(Term::parser())
            .map(Self::Eval);
        let bind = just(':')
            .then(text::keyword("bind"))
            .ignore_then(text::ident().padded())
            .then(just(':').ignore_then(Ty::parser()).or_not())
            .map(|(x, ty)| {
                if let Some(ty) = ty {
                    Self::BindVar(x, ty)
                } else {
                    Self::BindName(x)
                }
            });
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .map(Self::Type);
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, type_, term, noop)).then_ignore(end())
    }
}
