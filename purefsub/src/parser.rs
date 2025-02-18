use crate::syntax::{Binding, Command, Term, Ty, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Ty {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let top = text::keyword("Top").to(Self::top());

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((top, parens, var)).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .then(atom)
                .foldr(Self::arr);

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

            all.or(arrow).padded()
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

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = parens.or(var).padded();

            enum Arg {
                Atom(Rc<Term>),
                Ty(Rc<Ty>),
            }

            let arg = atom.clone().map(Arg::Atom).or(just('[')
                .ignore_then(Ty::parser())
                .then_ignore(just(']'))
                .padded()
                .map(Arg::Ty));

            let app = atom.then(arg.repeated()).foldl(|t, arg| match arg {
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

            choice((abs, t_abs, app)).padded()
        })
    }
}

impl Binding {
    fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let name = empty().to(Self::Name);
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        var.or(name).padded()
    }

    fn ty_parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let ty_var = just("<:")
            .ignore_then(Ty::parser())
            .or_not()
            .map(|ty| ty.unwrap_or_else(Ty::top))
            .map(Self::TyVar);

        ty_var.padded()
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
