use crate::syntax::{Command, Term, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Term {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> {
        recursive(|term| {
            let var = util::parser::ident(KEYWORDS.iter().copied()).map(Self::var);

            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());

            let string = util::parser::string().map(Self::string);
            let float = util::parser::float().map(Self::float);
            let int = util::parser::int().map(Self::from_int);

            let field = text::ident()
                .padded()
                .then_ignore(just('='))
                .or_not()
                .then(term.clone());

            let fields = field.separated_by(just(',')).map(|fields| {
                fields
                    .into_iter()
                    .enumerate()
                    .map(|(i, (k, v))| (k.unwrap_or_else(|| i.to_string()), v))
                    .collect::<Vec<_>>()
            });

            let record = just('{')
                .ignore_then(fields)
                .then_ignore(just('}'))
                .map(Self::record);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, string, float, int, record, parens, var));

            let path = atom
                .clone()
                .then(
                    just('.')
                        .ignore_then(text::ident().or(text::int(10)))
                        .repeated(),
                )
                .foldl(Self::proj);

            let app_ = path
                .clone()
                .then(path.clone().padded().repeated())
                .foldl(Self::app);

            let succ = text::keyword("succ")
                .ignore_then(path.clone().padded())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(path.clone().padded())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(path.clone().padded())
                .map(Self::is_zero);

            let times_float = text::keyword("timesfloat")
                .ignore_then(path.clone().padded())
                .then(path.clone().padded())
                .map(|(t1, t2)| Self::times_float(t1, t2));

            let app = choice((succ, pred, is_zero, times_float, app_));

            let if_ = text::keyword("if")
                .ignore_then(term.clone().padded())
                .then_ignore(text::keyword("then"))
                .then(term.clone().padded())
                .then_ignore(text::keyword("else"))
                .then(term.clone().padded())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let abs = text::keyword("lambda")
                .ignore_then(text::ident().padded())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(x, t)| Self::abs(x, t));

            let let_ = text::keyword("let")
                .ignore_then(text::ident().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|((x, t1), t2)| Self::let_(x, t1, t2));

            choice((if_, abs, let_, app)).padded()
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
            .then(just('=').ignore_then(Term::parser()).or_not())
            .map(|(x, t)| {
                if let Some(t) = t {
                    Self::BindTerm(x, t)
                } else {
                    Self::BindName(x)
                }
            });
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, term, noop)).then_ignore(end())
    }
}
