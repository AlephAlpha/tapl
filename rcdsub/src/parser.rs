use crate::syntax::{Binding, Command, KEYWORDS, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;

impl Ty {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|ty| {
            let bot = text::keyword("Bot").to(Self::bot());
            let top = text::keyword("Top").to(Self::top());

            let field = text::ident()
                .padded()
                .then_ignore(just(':'))
                .or_not()
                .then(ty.clone());

            let fields = field.separated_by(just(',')).map(|fields| {
                fields
                    .into_iter()
                    .enumerate()
                    .map(|(i, (k, v))| (k.unwrap_or_else(|| i.to_string()), v))
                    .collect::<Vec<_>>()
            });

            let record = just('{')
                .ignore_then(fields.clone())
                .then_ignore(just('}'))
                .map(Self::record);

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((bot, top, record, parens)).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .then(atom.clone())
                .foldr(Self::arr);

            arrow.padded()
        })
    }
}

impl Term {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::var_ident(KEYWORDS.iter().copied())
    }

    fn ident_or_underscore() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        Self::ident().or(text::keyword("_").to("_".to_string()))
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|term| {
            let var = Self::ident().map(Self::var);

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

            let atom = choice((record, parens, var)).padded();

            let path = atom
                .then(
                    just('.')
                        .ignore_then(text::ident().or(text::int(10)).padded())
                        .repeated(),
                )
                .foldl(Self::proj)
                .padded();

            let app = path.clone().then(path.repeated()).foldl(Self::app);

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(x, ty, t));

            choice((abs, app)).padded()
        })
    }
}

impl Binding {
    fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let name = empty().to(Self::Name);
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        choice((var, name)).padded()
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
            .then(Binding::parser())
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .map(Self::Type);
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, type_, term, noop)).then_ignore(end())
    }
}
