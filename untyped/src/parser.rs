use crate::syntax::{Binding, Command, Term, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Term {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ident(KEYWORDS.iter().copied())
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|term| {
            let var = Self::ident().map(Self::var);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = var.or(parens).padded();

            let app = atom.clone().then(atom.repeated()).foldl(Self::app);

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident().padded())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(x, t)| Self::abs(x, t));

            abs.or(app).padded()
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
            .ignore_then(Term::ident().padded())
            .map(|x| Self::Bind(x, Binding::Name));
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, term, noop)).then_ignore(end())
    }
}
