use crate::syntax::{Binding, Command, KEYWORDS, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;

impl Ty {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let bot = text::keyword("Bot").to(Self::bot());
            let top = text::keyword("Top").to(Self::top());
            let bool = text::keyword("Bool").to(Self::bool());

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((bot, top, bool, parens, var)).padded();

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

            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());
            let error = text::keyword("error").to(Self::error());

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, error, parens, var)).padded();

            let app = atom.clone().then(atom.repeated()).foldl(Self::app);

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

            let try_ = text::keyword("try")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("with"))
                .then(term.clone())
                .map(|(t1, t2)| Self::try_(t1, t2));

            choice((abs, if_, try_, app)).padded()
        })
    }
}

impl Binding {
    fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let name = empty().to(Self::Name);
        let term = just('=')
            .ignore_then(Term::parser())
            .map(|t| Self::TermAbb(t, None));
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        choice((term, var, name)).padded()
    }

    fn ty_parser() -> impl Parser<char, Self, Error = Simple<char>> {
        let ty_var = empty().to(Self::TyVar);
        let ty_abb = just('=').ignore_then(Ty::parser()).map(Self::TyAbb);

        choice((ty_abb, ty_var)).padded()
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
