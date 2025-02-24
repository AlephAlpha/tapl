use crate::syntax::{Binding, Command, KEYWORDS, Kind, Term, Ty};
use chumsky::prelude::*;
use std::rc::Rc;

impl Kind {
    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|kind| {
            let star = text::keyword("*").to(Self::star());

            let parens = kind.clone().delimited_by(just('('), just(')'));

            let atom = star.or(parens);

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .then(atom)
                .foldr(Self::arr);

            arrow.padded()
        })
    }
}

impl Ty {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ty_ident(KEYWORDS.iter().copied())
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let string = text::keyword("String").to(Self::string());
            let unit = text::keyword("Unit").to(Self::unit());
            let bool = text::keyword("Bool").to(Self::bool());
            let nat = text::keyword("Nat").to(Self::nat());
            let float = text::keyword("Float").to(Self::float());

            let some = just('{')
                .then(text::keyword("Some").padded())
                .ignore_then(Self::ident().padded())
                .then(
                    just("::")
                        .ignore_then(Kind::parser())
                        .or_not()
                        .map(|k| k.unwrap_or_else(Kind::star)),
                )
                .then_ignore(just(','))
                .then(ty.clone())
                .then_ignore(just('}'))
                .map(|((x, kn), t)| Self::some(x, kn, t));

            let field = text::ident()
                .padded()
                .then_ignore(just(':'))
                .or_not()
                .then(ty.clone());

            let fields = field.separated_by(just(',')).map(|fields| {
                fields
                    .into_iter()
                    .enumerate()
                    .map(|(i, (k, v))| (k.unwrap_or_else(|| i.to_string()), v))
                    .collect::<Vec<_>>()
            });

            let record = just('{')
                .ignore_then(fields.clone())
                .then_ignore(just('}'))
                .map(Self::record);

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((string, unit, bool, nat, float, some, record, parens, var)).padded();

            let app = atom.clone().then(atom.clone().repeated()).foldl(Self::app);

            let arrow = app
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .then(app)
                .foldr(Self::arr);

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
                .then(
                    just("::")
                        .ignore_then(Kind::parser())
                        .or_not()
                        .map(|k| k.unwrap_or_else(Kind::star)),
                )
                .then_ignore(just('.'))
                .then(ty.clone())
                .map(|((x, kn), t)| Self::all(x, kn, t));

            let ref_ = text::keyword("Ref")
                .ignore_then(atom.clone())
                .map(Self::ref_);

            choice((arrow, abs, all, ref_)).padded()
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

            let fields = field.separated_by(just(',')).map(|fields| {
                fields
                    .into_iter()
                    .enumerate()
                    .map(|(i, (k, v))| (k.unwrap_or_else(|| i.to_string()), v))
                    .collect::<Vec<_>>()
            });

            let record = just('{')
                .ignore_then(fields)
                .then_ignore(just('}'))
                .map(Self::record);

            let pack = just('{')
                .then(just('*').padded())
                .ignore_then(Ty::parser())
                .then_ignore(just(','))
                .then(term.clone())
                .then_ignore(just('}'))
                .then_ignore(text::keyword("as").padded())
                .then(Ty::parser())
                .map(|((ty1, t2), ty3)| Self::pack(ty1, t2, ty3));

            let inert = text::keyword("inert")
                .ignore_then(Ty::parser().delimited_by(just('['), just(']')).padded())
                .map(Self::inert);

            let seq = term
                .clone()
                .then(just(';').ignore_then(term.clone()).repeated())
                .foldl(|t1, t2| Self::app(Self::abs("_", Ty::unit(), t2), t1));

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
                .then(
                    just('.')
                        .ignore_then(text::ident().or(text::int(10)).padded())
                        .repeated(),
                )
                .foldl(Self::proj)
                .padded();

            let ref_ = text::keyword("ref")
                .ignore_then(path.clone())
                .map(Self::ref_);

            let deref = just('!').ignore_then(path.clone()).map(Self::deref);

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

            let arg = path.clone().map(Arg::Path).or(just('[')
                .ignore_then(Ty::parser())
                .then_ignore(just(']'))
                .padded()
                .map(Arg::Ty));

            let app = choice((
                ref_,
                deref,
                fix,
                times_float,
                succ,
                pred,
                is_zero,
                path.clone(),
            ))
            .then(arg.repeated())
            .foldl(|t, arg| match arg {
                Arg::Path(p) => Self::app(t, p),
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
                    just("::")
                        .ignore_then(Kind::parser())
                        .or_not()
                        .map(|k| k.unwrap_or_else(Kind::star)),
                )
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((ty, kn), t)| Self::t_abs(ty, kn, t));

            let assign = app
                .clone()
                .padded()
                .then_ignore(just(":="))
                .then(app.clone().padded())
                .map(|(t1, t2)| Self::assign(t1, t2));

            let unpack = text::keyword("let")
                .padded()
                .then(just('{'))
                .ignore_then(Ty::ident().padded())
                .then_ignore(just(','))
                .then(Self::ident().padded())
                .then_ignore(just('}'))
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

            choice((abs, t_abs, assign, unpack, let_, let_rec, if_, app)).padded()
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
        let ty_var = just("::")
            .ignore_then(Kind::parser())
            .or_not()
            .map(|k| k.unwrap_or_else(Kind::star))
            .map(Self::TyVar);
        let ty_abb = just('=')
            .ignore_then(Ty::parser())
            .map(|ty| Self::TyAbb(ty, None));

        ty_abb.or(ty_var).padded()
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
