use crate::syntax::{Binding, Command, KEYWORDS, Kind, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Kind {
    fn parser<'src>(
        kind: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let star = just('*').to(Self::star());

        let parens = kind.delimited_by(just('('), just(')'));

        let atom = star.or(parens).padded();

        let arrow = atom
            .clone()
            .then_ignore(just("=>"))
            .repeated()
            .foldr(atom, Self::arr);

        arrow.padded().labelled("kind").boxed()
    }
}

impl Ty {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser<'src>(
        kind: impl Parser<'src, &'src str, Rc<Kind>, ParserError<'src>> + Clone + 'src,
        ty: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);

        let top = text::keyword("Top")
            .padded()
            .ignore_then(
                kind.clone()
                    .delimited_by(just('['), just(']'))
                    .padded()
                    .or_not(),
            )
            .map(|kn| kn.unwrap_or_else(Kind::star).make_top());

        let parens = ty.clone().delimited_by(just('('), just(')'));

        let atom = choice((top, parens, var)).padded();

        let app = atom.clone().foldl(atom.clone().repeated(), Self::app);

        let arrow = app
            .clone()
            .then_ignore(just("->"))
            .repeated()
            .foldr(app, Self::arr);

        let abs = text::keyword("lambda")
            .ignore_then(Self::ident().padded())
            .then(
                just("::")
                    .ignore_then(kind.clone())
                    .or_not()
                    .map(|k| k.unwrap_or_else(Kind::star)),
            )
            .then_ignore(just('.'))
            .then(ty.clone())
            .map(|((x, kn), t)| Self::abs(x, kn, t));

        let all = text::keyword("All")
            .ignore_then(Self::ident().padded())
            .then(
                just("<:").ignore_then(ty.clone()).or(just("::")
                    .ignore_then(kind)
                    .or_not()
                    .map(|k| k.unwrap_or_else(Kind::star).make_top())),
            )
            .then_ignore(just('.'))
            .then(ty)
            .map(|((x, t1), t2)| Self::all(x, t1, t2));

        choice((arrow, abs, all)).padded().labelled("type").boxed()
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
        kind: impl Parser<'src, &'src str, Rc<Kind>, ParserError<'src>> + Clone + 'src,
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + Clone + 'src,
        term: impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone + 'src,
    ) -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        let var = Self::ident().map(Self::var);

        let parens = term.clone().delimited_by(just('('), just(')'));

        let atom = parens.or(var).padded();

        enum Arg {
            Path(Rc<Term>),
            Ty(Rc<Ty>),
        }

        let arg = atom.clone().map(Arg::Path).or(ty
            .clone()
            .delimited_by(just('['), just(']'))
            .padded()
            .map(Arg::Ty));

        let app = atom.foldl(arg.repeated(), |t, arg| match arg {
            Arg::Path(p) => Self::app(t, p),
            Arg::Ty(ty) => Self::t_app(t, ty),
        });

        let abs = text::keyword("lambda")
            .ignore_then(Self::ident_or_underscore().padded())
            .then_ignore(just(':'))
            .then(ty.clone())
            .then_ignore(just('.'))
            .then(term.clone())
            .map(|((x, ty), t)| Self::abs(x, ty, t));

        let t_abs = text::keyword("lambda")
            .ignore_then(Ty::ident().padded())
            .then(
                just("<:").ignore_then(ty).or(just("::")
                    .ignore_then(kind)
                    .or_not()
                    .map(|k| k.unwrap_or_else(Kind::star).make_top())),
            )
            .then_ignore(just('.'))
            .then(term)
            .map(|((x, ty), t)| Self::t_abs(x, ty, t));

        choice((abs, t_abs, app)).padded().labelled("term").boxed()
    }
}

impl Binding {
    fn parser<'src>(
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let name = empty().to(Self::Name);
        let var = just(':').ignore_then(ty).map(Self::Var);

        var.or(name).padded()
    }

    fn ty_parser<'src>(
        kind: impl Parser<'src, &'src str, Rc<Kind>, ParserError<'src>> + 'src,
        ty: impl Parser<'src, &'src str, Rc<Ty>, ParserError<'src>> + 'src,
    ) -> impl Parser<'src, &'src str, Self, ParserError<'src>> {
        let ty_var = just("<:")
            .ignore_then(ty)
            .or(just("::")
                .ignore_then(kind)
                .or_not()
                .map(|k| k.unwrap_or_else(Kind::star).make_top()))
            .map(Self::TyVar);

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

        kind.define(Kind::parser(kind.clone()));
        ty.define(Ty::parser(kind.clone(), ty.clone()));
        term.define(Term::parser(kind.clone(), ty.clone(), term.clone()));

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
                    .then(Binding::parser(ty.clone()))
                    .or(Ty::ident()
                        .padded()
                        .then(Binding::ty_parser(kind.clone(), ty.clone()))),
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
