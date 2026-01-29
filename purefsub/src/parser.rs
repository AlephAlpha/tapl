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

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((top, parens, var)).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .foldr(atom, Self::arr);

            let all = text::keyword("All")
                .ignore_then(Self::ident().padded())
                .then(
                    just("<:")
                        .ignore_then(ty.clone())
                        .or_not()
                        .map(|t| t.unwrap_or_else(Self::top)),
                )
                .then_ignore(just('.'))
                .then(ty.clone())
                .map(|((x, t1), t2)| Self::all(x, t1, t2));

            all.or(arrow).padded().labelled("type").boxed()
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

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = parens.or(var).padded();

            enum Arg {
                Atom(Rc<Term>),
                Ty(Rc<Ty>),
            }

            let arg = atom.clone().map(Arg::Atom).or(Ty::parser()
                .delimited_by(just('['), just(']'))
                .padded()
                .map(Arg::Ty));

            let app = atom.foldl(arg.repeated(), |t, arg| match arg {
                Arg::Atom(p) => Self::app(t, p),
                Arg::Ty(ty) => Self::t_app(t, ty),
            });

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(x, ty, t));

            let t_abs = text::keyword("lambda")
                .ignore_then(Ty::ident().padded())
                .then(
                    just("<:")
                        .ignore_then(Ty::parser())
                        .or_not()
                        .map(|t| t.unwrap_or_else(Ty::top)),
                )
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::t_abs(x, ty, t));

            choice((abs, t_abs, app)).padded().labelled("term").boxed()
        })
    }
}

impl Binding {
    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        var.or(name).padded()
    }

    fn ty_parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let ty_var = just("<:")
            .ignore_then(Ty::parser())
            .or_not()
            .map(|ty| ty.unwrap_or_else(Ty::top))
            .map(Self::TyVar);

        ty_var.padded()
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
            .ignore_then(
                Term::ident()
                    .padded()
                    .then(Binding::parser())
                    .or(Ty::ident().padded().then(Binding::ty_parser())),
            )
            .then_ignore(end())
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .then_ignore(end())
            .map(Self::Type);
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, bind, type_, noop)).then_ignore(end())
    }
}
