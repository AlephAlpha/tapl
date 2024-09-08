use crate::syntax::{Command, Term, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Term {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|term| {
            let var = text::ident().try_map(|s: String, span| {
                if KEYWORDS.contains(&s.as_str()) {
                    Err(Simple::custom(span, format!("unexpected keyword: {s}")))
                } else {
                    Ok(Self::var(s))
                }
            });

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
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, term, noop)).then_ignore(end())
    }
}
