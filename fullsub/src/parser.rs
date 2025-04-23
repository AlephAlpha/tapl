use crate::syntax::{Binding, Command, KEYWORDS, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Ty {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let top = text::keyword("Top").to(Self::top());
            let string = text::keyword("String").to(Self::string());
            let unit = text::keyword("Unit").to(Self::unit());
            let bool = text::keyword("Bool").to(Self::bool());
            let float = text::keyword("Float").to(Self::float());
            let nat = text::keyword("Nat").to(Self::nat());

            let field = text::ident()
                .padded()
                .then_ignore(just(':'))
                .or_not()
                .then(ty.clone());

            let fields = field
                .separated_by(just(','))
                .enumerate()
                .collect::<Vec<_>>()
                .map(|fields| {
                    fields
                        .into_iter()
                        .map(|(i, (k, v))| {
                            (k.map(str::to_owned).unwrap_or_else(|| i.to_string()), v)
                        })
                        .collect::<Vec<_>>()
                });

            let record = fields.delimited_by(just('{'), just('}')).map(Self::record);

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((top, string, bool, unit, float, nat, record, parens, var)).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .foldr(atom.clone(), Self::arr);

            arrow.padded().labelled("type").boxed()
        })
    }
}

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

            let unit = text::keyword("unit").to(Self::unit());
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
                            (k.map(str::to_owned).unwrap_or_else(|| i.to_string()), v)
                        })
                        .collect::<Vec<_>>()
                });

            let record = fields.delimited_by(just('{'), just('}')).map(Self::record);

            let inert = text::keyword("inert")
                .ignore_then(Ty::parser().delimited_by(just('['), just(']')).padded())
                .map(Self::inert);

            let seq = term
                .clone()
                .foldl(just(';').ignore_then(term.clone()).repeated(), |t1, t2| {
                    Self::app(Self::abs("_", Ty::unit(), t2), t1)
                });

            let parens = seq.delimited_by(just('('), just(')'));

            let atom = choice((
                true_, false_, unit, string, float, int, record, inert, parens, var,
            ))
            .padded();

            let ascribe_ = atom
                .clone()
                .then_ignore(text::keyword("as"))
                .then(Ty::parser())
                .map(|(t, ty)| Self::ascribe(t, ty));

            let ascribe = ascribe_.or(atom.clone());

            let path = ascribe
                .foldl(
                    just('.')
                        .ignore_then(text::ident().or(text::int(10)).padded())
                        .repeated(),
                    Self::proj,
                )
                .padded();

            let fix = text::keyword("fix")
                .ignore_then(path.clone())
                .map(Self::fix);

            let times_float = text::keyword("timesfloat")
                .ignore_then(path.clone())
                .then(path.clone())
                .map(|(t1, t2)| Self::times_float(t1, t2));

            let succ = text::keyword("succ")
                .ignore_then(path.clone())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(path.clone())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(path.clone())
                .map(Self::is_zero);

            let app = choice((fix, times_float, succ, pred, is_zero, path.clone()))
                .foldl(path.repeated(), Self::app);

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(x, ty, t));

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let let_ = text::keyword("let")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|((x, t1), t2)| Self::let_(x, t1, t2));

            let let_rec = text::keyword("letrec")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|(((x, ty), t1), t2)| {
                    Self::let_(x.clone(), Self::fix(Self::abs(x, ty, t1)), t2)
                });

            choice((if_, abs, let_, let_rec, app))
                .padded()
                .labelled("term").boxed()
        })
    }
}

impl Binding {
    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let term = just('=')
            .ignore_then(Term::parser())
            .map(|t| Self::TermAbb(t, None));
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        choice((term, var, name)).padded()
    }

    fn ty_parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let ty_var = empty().to(Self::TyVar);
        let ty_abb = just('=').ignore_then(Ty::parser()).map(Self::TyAbb);

        choice((ty_abb, ty_var)).padded()
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Rich<char>>> {
        Self::parser().parse(input).into_result()
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
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
            .ignore_then(
                Term::ident()
                    .padded()
                    .then(Binding::parser())
                    .or(Ty::ident().padded().then(Binding::ty_parser())),
            )
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .map(Self::Type);
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, type_, term, noop)).then_ignore(end())
    }
}
