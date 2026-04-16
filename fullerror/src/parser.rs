use crate::syntax::{Binding, Command, KEYWORDS, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Ty {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);

        let bot = text::keyword("Bot").to(Self::bot());
        let top = text::keyword("Top").to(Self::top());
        let bool = text::keyword("Bool").to(Self::bool());

        let parens = ty.delimited_by(just('('), just(')'));

        let atom = choice((bot, top, bool, parens, var)).padded();

        let arrow = atom
            .clone()
            .then_ignore(just("->"))
            .repeated()
            .foldr(atom.clone(), Self::arr);

        arrow.padded().labelled("type").boxed()
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

    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);

        let true_ = text::keyword("true").to(Self::true_());
        let false_ = text::keyword("false").to(Self::false_());
        let error = text::keyword("error").to(Self::error());

        let parens = term.clone().delimited_by(just('('), just(')'));

        let atom = choice((true_, false_, error, parens, var)).padded();

        let app = atom.clone().foldl(atom.repeated(), Self::app);

        let abs = text::keyword("lambda")
            .ignore_then(Self::ident_or_underscore().padded())
            .then_ignore(just(':'))
            .then(ty)
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
            .then(term)
            .map(|(t1, t2)| Self::try_(t1, t2));

        choice((abs, if_, try_, app))
            .padded()
            .labelled("term")
            .boxed()
    }
}

impl Binding {
    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + 'src,
        term: impl Parser<'src, &'src str, Rc<Term>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let term = just('=').ignore_then(term).map(|t| Self::TermAbb(t, None));
        let var = just(':').ignore_then(ty).map(Self::Var);

        choice((term, var, name)).padded()
    }

    fn ty_parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let ty_var = empty().to(Self::TyVar);
        let ty_abb = just('=').ignore_then(ty).map(Self::TyAbb);

        choice((ty_abb, ty_var)).padded()
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Rich<'_, char>>> {
        Self::parser().parse(input).into_result()
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let mut ty = Recursive::declare();
        let mut term = Recursive::declare();

        ty.define(Ty::parser(ty.clone()));
        term.define(Term::parser(ty.clone(), term.clone()));

        let eval1 = just(':')
            .then(text::keyword("eval1"))
            .ignore_then(term.clone())
            .then_ignore(end())
            .map(Self::Eval1);
        let eval = just(':')
            .then(text::keyword("eval"))
            .or_not()
            .ignore_then(term.clone())
            .then_ignore(end())
            .map(Self::Eval);
        let bind = just(':')
            .then(text::keyword("bind"))
            .or_not()
            .ignore_then(
                Term::ident()
                    .padded()
                    .then(Binding::parser(ty.clone(), term.clone()))
                    .or(Ty::ident().padded().then(Binding::ty_parser(ty.clone()))),
            )
            .then_ignore(end())
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(term.clone())
            .then_ignore(end())
            .map(Self::Type);
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, bind, type_, noop)).then_ignore(end())
    }
}
