use crate::syntax::{Binding, Command, KEYWORDS, Kind, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Kind {
    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Term>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let star = just('*').to(Self::star());

        let ty_app = Ty::app_parser(ty.clone(), term);

        let pi = text::keyword("Pi")
            .ignore_then(Term::ident_or_underscore().padded())
            .then_ignore(just(":"))
            .then(ty)
            .then_ignore(just('.'))
            .or(ty_app
                .then_ignore(just("->"))
                .map(|ty| ("_".to_string(), ty)))
            .padded()
            .repeated()
            .foldr(star, |(x, ty), kn| Self::pi(x, ty, kn));

        pi.padded().labelled("kind").boxed()
    }
}

impl Ty {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn app_parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Term>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);
        let prop = text::keyword("Prop").to(Self::prop());
        let prf = text::keyword("Prf").to(Self::prf());

        let parens = ty.delimited_by(just('('), just(')'));

        let atom = choice((prop, prf, parens, var)).padded();

        let atom_term = Term::atom_parser(term);

        let app = atom.clone().foldl(atom_term.repeated(), Self::app);

        app.labelled("type application").boxed()
    }

    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Term>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let app = Self::app_parser(ty.clone(), term);

        let arrow = app
            .clone()
            .then_ignore(just("->"))
            .repeated()
            .foldr(app, |ty1, ty2| Self::pi("_".to_string(), ty1, ty2));

        let pi = text::keyword("Pi")
            .ignore_then(Term::ident_or_underscore().padded())
            .then_ignore(just(":"))
            .then(ty.clone())
            .then_ignore(just('.'))
            .then(ty)
            .map(|((x, ty1), ty2)| Self::pi(x, ty1, ty2));

        arrow.or(pi).padded().labelled("type").boxed()
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

    fn atom_parser<'src>(
        term: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);

        let parens = term.delimited_by(just('('), just(')'));

        var.or(parens).padded().labelled("term atom").boxed()
    }

    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let atom = Self::atom_parser(term.clone());

        let app = atom.clone().foldl(atom.clone().repeated(), Self::app);

        let abs = text::keyword("lambda")
            .ignore_then(Self::ident_or_underscore().padded())
            .then_ignore(just(":"))
            .then(ty.clone())
            .then_ignore(just('.'))
            .then(term.clone())
            .map(|((x, ty), t)| Self::abs(x, ty, t));

        let all = text::keyword("all")
            .ignore_then(Self::ident_or_underscore().padded())
            .then_ignore(just(":"))
            .then(ty)
            .then_ignore(just('.'))
            .then(term)
            .map(|((x, ty), t)| Self::all(x, ty, t));

        choice((abs, all, app)).padded().labelled("term").boxed()
    }
}

impl Binding {
    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Term>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let abb = just('=')
            .ignore_then(term)
            .then(just(':').ignore_then(ty.clone()).or_not())
            .map(|(t, ty)| Self::TermAbb(t, ty));
        let var = just(':').ignore_then(ty).map(Self::Var);

        choice((abb, var, name)).padded()
    }

    fn ty_parser<'src>(
        kind: impl Parser<'src, &'src str, Rc<Kind>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let ty_var = just("::").ignore_then(kind).map(Self::TyVar);

        ty_var.padded()
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Rich<'_, char>>> {
        Self::parser().parse(input).into_result()
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let mut kind = Recursive::declare();
        let mut ty = Recursive::declare();
        let mut term = Recursive::declare();

        kind.define(Kind::parser(ty.clone(), term.clone()));
        ty.define(Ty::parser(ty.clone(), term.clone()));
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
                    .or(Ty::ident().padded().then(Binding::ty_parser(kind.clone()))),
            )
            .then_ignore(end())
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(term.clone())
            .then_ignore(end())
            .map(Self::Type);
        let kind = just(':')
            .then(text::keyword("kind"))
            .ignore_then(ty.clone())
            .then_ignore(end())
            .map(Self::Kind);
        let noop = text::whitespace().then_ignore(end()).to(Self::Noop);

        choice((eval1, eval, bind, type_, kind, noop)).then_ignore(end())
    }
}
