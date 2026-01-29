use crate::syntax::{Binding, Command, KEYWORDS, Term};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Term {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::var_ident(KEYWORDS.iter().copied())
    }

    fn ident_or_underscore<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone
    {
        Self::ident().or(text::keyword("_").to("_".to_string()))
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        recursive(|term| {
            let var = Self::ident().map(Self::var);

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

            let fields = field
                .separated_by(just(','))
                .enumerate()
                .collect::<Vec<_>>()
                .map(|fields| {
                    fields
                        .into_iter()
                        .map(|(i, (k, v))| {
                            (
                                k.map(str::to_owned).unwrap_or_else(|| (i + 1).to_string()),
                                v,
                            )
                        })
                        .collect::<Vec<_>>()
                });

            let record = fields.delimited_by(just('{'), just('}')).map(Self::record);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, string, float, int, record, parens, var)).padded();

            let path = atom
                .clone()
                .foldl(
                    just('.')
                        .ignore_then(text::ident().or(text::int(10)).padded())
                        .repeated(),
                    Self::proj,
                )
                .padded();

            let succ = text::keyword("succ")
                .ignore_then(path.clone())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(path.clone())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(path.clone())
                .map(Self::is_zero);

            let times_float = text::keyword("timesfloat")
                .ignore_then(path.clone())
                .then(path.clone())
                .map(|(t1, t2)| Self::times_float(t1, t2));

            let app = choice((succ, pred, is_zero, times_float, path.clone()))
                .foldl(path.repeated(), Self::app);

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(x, t)| Self::abs(x, t));

            let let_ = text::keyword("let")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|((x, t1), t2)| Self::let_(x, t1, t2));

            choice((if_, abs, let_, app))
                .padded()
                .labelled("term")
                .boxed()
        })
    }
}

impl Binding {
    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let term = just('=').ignore_then(Term::parser()).map(Self::TermAbb);

        term.or(name).padded()
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
        let bind = just(':')
            .then(text::keyword("bind"))
            .or_not()
            .ignore_then(Term::ident().padded())
            .then(Binding::parser())
            .then_ignore(end())
            .map(|(x, b)| Self::Bind(x, b));
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, bind, noop)).then_ignore(end())
    }
}
