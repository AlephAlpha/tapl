use crate::syntax::{Command, Term};
use chumsky::prelude::*;
use std::rc::Rc;

impl Term {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|term| {
            let var = text::ident().map(Self::var);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = var.or(parens);

            let app = atom.clone().then(atom.padded().repeated()).foldl(Self::app);

            let abs = text::keyword("lambda")
                .ignore_then(text::ident().padded())
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
            .ignore_then(text::ident().padded())
            .map(Self::Bind);

        let command = eval1.or(eval).or(bind).or(term);
        command.then_ignore(end())
    }
}
