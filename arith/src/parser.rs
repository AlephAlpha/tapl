use crate::syntax::{Command, Term};
use chumsky::prelude::*;
use std::rc::Rc;

impl Term {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|term| {
            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());

            let number = text::int(10).try_map(|s: String, span| {
                s.parse()
                    .map(Self::from_num)
                    .map_err(|e| Simple::custom(span, e.to_string()))
            });

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = true_.or(false_).or(number).or(parens);

            let succ = text::keyword("succ")
                .ignore_then(term.clone().padded())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(term.clone().padded())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(term.clone().padded())
                .map(Self::is_zero);

            let app = atom.or(succ).or(pred).or(is_zero);

            let if_ = text::keyword("if")
                .ignore_then(term.clone().padded())
                .then_ignore(text::keyword("then"))
                .then(term.clone().padded())
                .then_ignore(text::keyword("else"))
                .then(term.clone().padded())
                .map(|((cond, then), else_)| Self::if_(cond, then, else_));

            app.or(if_).padded()
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

        let command = eval1.or(eval).or(term);
        command.then_ignore(end())
    }
}
