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
            let id = Self::ident().map(Self::id);
            let bool = text::keyword("Bool").to(Self::bool());
            let nat = text::keyword("Nat").to(Self::nat());

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((bool, nat, parens, id)).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .foldr(atom, Self::arr);

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

            let true_ = text::keyword("true").to(Self::true_());
            let false_ = text::keyword("false").to(Self::false_());
            let int = util::parser::int().map(Self::from_int);

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_, false_, int, parens, var)).padded();

            let succ = text::keyword("succ")
                .ignore_then(atom.clone())
                .map(Self::succ);

            let pred = text::keyword("pred")
                .ignore_then(atom.clone())
                .map(Self::pred);

            let is_zero = text::keyword("iszero")
                .ignore_then(atom.clone())
                .map(Self::is_zero);

            let app = choice((succ, pred, is_zero, atom.clone())).foldl(atom.repeated(), Self::app);

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(x, ty, t));

            choice((if_, abs, app)).padded().labelled("term").boxed()
        })
    }
}

impl Binding {
    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let var = just(':').ignore_then(Ty::parser()).map(Self::Var);

        var.or(name).padded()
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
