use crate::syntax::{Binding, Command, KEYWORDS, Kind, Term, Ty, Variance};
use chumsky::prelude::*;
use std::rc::Rc;
use util::parser::ParserError;

impl Kind {
    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        recursive(|kind| {
            let star = just('*').to(Self::star());

            let parens = kind.clone().delimited_by(just('('), just(')'));

            let atom = star.or(parens).padded();

            let arrow = atom
                .clone()
                .then_ignore(just("=>"))
                .repeated()
                .foldr(atom, Self::arr);

            arrow.padded().labelled("kind").boxed()
        })
    }
}

impl Ty {
    fn ident<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser<'src>() -> impl Parser<'src, &'src str, Rc<Self>, ParserError<'src>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let string = text::keyword("String").to(Self::string());
            let unit = text::keyword("Unit").to(Self::unit());
            let bool = text::keyword("Bool").to(Self::bool());
            let nat = text::keyword("Nat").to(Self::nat());
            let float = text::keyword("Float").to(Self::float());

            let top = text::keyword("Top")
                .padded()
                .ignore_then(
                    Kind::parser()
                        .delimited_by(just('['), just(']'))
                        .padded()
                        .or_not(),
                )
                .map(|kn| kn.unwrap_or_else(Kind::star).make_top());

            let o_type = just("<:").ignore_then(ty.clone()).or(just("::")
                .ignore_then(Kind::parser())
                .or_not()
                .map(|k| k.unwrap_or_else(Kind::star).make_top()));

            let some = text::keyword("Some")
                .padded()
                .ignore_then(Self::ident().padded())
                .then(o_type.clone())
                .then_ignore(just(','))
                .then(ty.clone())
                .delimited_by(just('{'), just('}'))
                .map(|((x, t1), t2)| Self::some(x, t1, t2));

            let field = just('#')
                .or_not()
                .map(|v| v.map_or(Variance::Covariant, |_| Variance::Invariant))
                .then(text::ident().padded().then_ignore(just(':')).or_not())
                .then(ty.clone());

            let fields = field
                .separated_by(just(','))
                .enumerate()
                .collect::<Vec<_>>()
                .map(|fields| {
                    fields
                        .into_iter()
                        .map(|(i, ((var, k), v))| {
                            (
                                k.map(str::to_owned).unwrap_or_else(|| i.to_string()),
                                var,
                                v,
                            )
                        })
                        .collect::<Vec<_>>()
                });

            let record = fields.delimited_by(just('{'), just('}')).map(Self::record);

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((
                top, string, unit, bool, nat, float, some, record, parens, var,
            ))
            .padded();

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
                        .ignore_then(Kind::parser())
                        .or_not()
                        .map(|k| k.unwrap_or_else(Kind::star)),
                )
                .then_ignore(just('.'))
                .then(ty.clone())
                .map(|((x, kn), t)| Self::abs(x, kn, t));

            let all = text::keyword("All")
                .ignore_then(Self::ident().padded())
                .then(o_type.clone())
                .then_ignore(just('.'))
                .then(ty.clone())
                .map(|((x, t1), t2)| Self::all(x, t1, t2));

            choice((arrow, abs, all)).padded().labelled("type").boxed()
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

            let field = just('#')
                .or_not()
                .map(|v| v.map_or(Variance::Covariant, |_| Variance::Invariant))
                .then(text::ident().padded().then_ignore(just('=')).or_not())
                .then(term.clone());

            let fields = field
                .separated_by(just(','))
                .enumerate()
                .collect::<Vec<_>>()
                .map(|fields| {
                    fields
                        .into_iter()
                        .map(|(i, ((var, k), v))| {
                            (
                                k.map(str::to_owned).unwrap_or_else(|| i.to_string()),
                                var,
                                v,
                            )
                        })
                        .collect::<Vec<_>>()
                });

            let record = fields.delimited_by(just('{'), just('}')).map(Self::record);

            let pack = just('*')
                .padded()
                .ignore_then(Ty::parser())
                .then_ignore(just(','))
                .then(term.clone())
                .delimited_by(just('{'), just('}'))
                .then_ignore(text::keyword("as").padded())
                .then(Ty::parser())
                .map(|((ty1, t2), ty3)| Self::pack(ty1, t2, ty3));

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
                unit, true_, false_, string, float, int, record, pack, inert, parens, var,
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

            enum Arg {
                Path(Rc<Term>),
                Ty(Rc<Ty>),
            }

            let arg = path.clone().map(Arg::Path).or(Ty::parser()
                .delimited_by(just('['), just(']'))
                .padded()
                .map(Arg::Ty));

            let app = choice((fix, times_float, succ, pred, is_zero, path.clone())).foldl(
                arg.repeated(),
                |t, arg| match arg {
                    Arg::Path(p) => Self::app(t, p),
                    Arg::Ty(ty) => Self::t_app(t, ty),
                },
            );

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
                    just("<:").ignore_then(Ty::parser()).or(just("::")
                        .ignore_then(Kind::parser())
                        .or_not()
                        .map(|k| k.unwrap_or_else(Kind::star).make_top())),
                )
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((x, ty), t)| Self::t_abs(x, ty, t));

            let unpack = text::keyword("let")
                .padded()
                .ignore_then(
                    Ty::ident()
                        .padded()
                        .then_ignore(just(','))
                        .then(Self::ident().padded())
                        .delimited_by(just('{'), just('}')),
                )
                .then_ignore(just('=').padded())
                .then(term.clone())
                .then_ignore(text::keyword("in"))
                .then(term.clone())
                .map(|(((ty, x), t1), t2)| Self::unpack(ty, x, t1, t2));

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

            let if_ = text::keyword("if")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("then"))
                .then(term.clone())
                .then_ignore(text::keyword("else"))
                .then(term.clone())
                .map(|((t1, t2), t3)| Self::if_(t1, t2, t3));

            let update = app
                .clone()
                .then_ignore(just("<-"))
                .then(text::ident().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .map(|((t1, l), t2)| Self::update(t1, l, t2));

            choice((abs, t_abs, unpack, let_, let_rec, if_, update, app))
                .padded()
                .labelled("term")
                .boxed()
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
        let ty_var = just("<:")
            .ignore_then(Ty::parser())
            .or(just("::")
                .ignore_then(Kind::parser())
                .or_not()
                .map(|k| k.unwrap_or_else(Kind::star).make_top()))
            .map(Self::TyVar);
        let ty_abb = Ty::ident()
            .padded()
            .then(
                just("::")
                    .ignore_then(Kind::parser())
                    .or_not()
                    .map(|k| k.unwrap_or_else(Kind::star)),
            )
            .repeated()
            .foldr(just('=').ignore_then(Ty::parser()), |(x, kn), ty| {
                Ty::abs(x, kn, ty)
            })
            .map(|ty| Self::TyAbb(ty, None));

        ty_abb.or(ty_var).padded()
    }
}

impl Command {
    pub fn parse(input: &str) -> Result<Self, Vec<Rich<'_, char>>> {
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
        let kind = just(':')
            .then(text::keyword("kind"))
            .ignore_then(Ty::parser())
            .map(Self::Kind);
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, type_, kind, term, noop)).then_ignore(end())
    }
}
