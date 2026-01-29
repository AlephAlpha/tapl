use crate::syntax::{Command, Term};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Term {
    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        recursive(|term| {
            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());

            let int = util::parser::int().map(Self::from_int);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, int, parens));

            let succ = text::keyword("succ")
                .ignore_then(term.clone())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(term.clone())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(term.clone())
                .map(Self::is_zero);

            let app = choice((atom, succ, pred, is_zero));

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            app.or(if_).padded().labelled("term").boxed()
        })
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Rich<'_, char>>> {
        Self::parser().parse(input).into_result()
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let eval1 = just(':')
            .then(text::keyword("eval1"))
            .ignore_then(Term::parser())
            .then_ignore(end())
            .map(Self::Eval1);
        let eval = just(':')
            .then(text::keyword("eval"))
            .or_not()
            .ignore_then(Term::parser())
            .then_ignore(end())
            .map(Self::Eval);
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, noop)).then_ignore(end())
    }
}
