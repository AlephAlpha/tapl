use crate::syntax::{Binding, Command, KEYWORDS, PreTy, Qualifier, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Ty {
    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let mut pre_ty = Recursive::declare();
        let mut ty = Recursive::declare();

        let qualifier = choice((
            text::keyword("ord").to(Qualifier::Ord),
            text::keyword("lin").to(Qualifier::Lin),
            text::keyword("un").to(Qualifier::Un),
        ))
        .padded();

        let bool = text::keyword("Bool").to(PreTy::bool());

        let parens = pre_ty.clone().delimited_by(just('('), just(')'));

        let pre_atom = bool.or(parens).padded();

        let atom = qualifier
            .clone()
            .or_not()
            .map(|q| q.unwrap_or(Qualifier::Un))
            .then(pre_atom.clone())
            .map(|(q, pre_ty)| Self::new(q, pre_ty));

        let pre_pair_ = atom
            .clone()
            .then_ignore(just('*'))
            .then(atom.clone())
            .map(|(t1, t2)| PreTy::pair(t1, t2))
            .padded();

        let pre_pair = pre_pair_.or(pre_atom);

        let pair = pre_pair.clone().map(Self::un).or(atom.clone());

        let arrow = pair
            .clone()
            .then_ignore(just("->"))
            .repeated()
            .foldr(pair.clone(), |t1, t2| Self::un(PreTy::arr(t1, t2)));

        let pre_arrow = arrow.clone().map(|t| t.pre_ty.clone());

        pre_ty.define(pre_arrow.padded().labelled("pre_ty").boxed());
        ty.define(arrow.padded().labelled("ty").boxed());

        ty
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
            let qualifier = choice((
                text::keyword("ord").to(Qualifier::Ord),
                text::keyword("lin").to(Qualifier::Lin),
                text::keyword("un").to(Qualifier::Un),
            ))
            .padded();

            let var = Self::ident().map(Self::var);

            let true_ = text::keyword("true").to(Self::bool(Qualifier::Un, true));
            let false_ = text::keyword("false").to(Self::bool(Qualifier::Un, false));

            let pair = term
                .clone()
                .then_ignore(just(','))
                .then(term.clone())
                .delimited_by(just('<'), just('>'))
                .map(|(t1, t2)| Self::pair(Qualifier::Un, t1, t2));

            let parens = term.clone().delimited_by(just('('), just(')'));

            let atom = choice((true_.clone(), false_.clone(), pair.clone(), parens, var)).padded();

            let qualified = qualifier
                .clone()
                .then(choice((true_, false_, pair)).padded())
                .map(|(q, t)| t.with_qualifier(q));

            let app = qualified
                .or(atom.clone())
                .foldl(atom.clone().repeated(), Self::app);

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let split = text::keyword("split")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("as"))
                .then(Self::ident_or_underscore().padded())
                .then_ignore(just(','))
                .then(Self::ident_or_underscore().padded())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|(((t1, x1), x2), t2)| Self::split(t1, x1, x2, t2));

            let let_ = text::keyword("let")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|((x, t1), t2)| Self::let_(x, t1, t2));

            let abs = text::keyword("lambda")
                .ignore_then(Self::ident_or_underscore().padded())
                .then_ignore(just(':'))
                .then(Ty::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::abs(Qualifier::Un, x, ty, t));

            let qualified_abs = qualifier
                .clone()
                .then(abs.clone())
                .map(|(q, t)| t.with_qualifier(q));

            choice((if_, split, let_, qualified_abs, abs, app))
                .padded()
                .labelled("term")
                .boxed()
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
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .then_ignore(end())
            .map(Self::Type);
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, bind, type_, noop)).then_ignore(end())
    }
}
