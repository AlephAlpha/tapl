use crate::syntax::{Binding, Command, Term, Ty, KEYWORDS};
use chumsky::prelude::*;
use std::rc::Rc;

impl Ty {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ident(KEYWORDS.iter().copied())
    }

    fn parser() -> impl Parser<char, Rc<Self>, Error = Simple<char>> + Clone {
        recursive(|ty| {
            let var = Self::ident().map(Self::var);

            let bot = text::keyword("Bot").to(Self::bot());
            let top = text::keyword("Top").to(Self::top());
            let string = text::keyword("String").to(Self::string());
            let unit = text::keyword("Unit").to(Self::unit());
            let bool = text::keyword("Bool").to(Self::bool());
            let float = text::keyword("Float").to(Self::float());
            let nat = text::keyword("Nat").to(Self::nat());

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

            let variant = just('<')
                .ignore_then(fields.clone())
                .then_ignore(just('>'))
                .map(Self::variant);

            let parens = ty.clone().delimited_by(just('('), just(')'));

            let atom = choice((
                bot, top, string, bool, unit, float, nat, record, variant, parens, var,
            ))
            .padded();

            let arrow = atom
                .clone()
                .then_ignore(just("->"))
                .repeated()
                .then(atom.clone())
                .foldr(Self::arr);

            let ref_ = text::keyword("Ref")
                .ignore_then(atom.clone())
                .map(Self::ref_);
            let source = text::keyword("Source")
                .ignore_then(atom.clone())
                .map(Self::source);
            let sink = text::keyword("Sink")
                .ignore_then(atom.clone())
                .map(Self::sink);

            choice((arrow, ref_, source, sink)).padded()
        })
    }
}

impl Term {
    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone {
        util::parser::ident(KEYWORDS.iter().copied())
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

            let tag = just('<')
                .ignore_then(text::ident().padded())
                .then_ignore(just('='))
                .then(term.clone())
                .then_ignore(just('>'))
                .then_ignore(text::keyword("as").padded())
                .then(Ty::parser())
                .map(|((l, t1), t2)| Self::tag(l, t1, t2));

            let inert = text::keyword("inert")
                .ignore_then(Ty::parser().delimited_by(just('['), just(']')).padded())
                .map(Self::inert);

            let seq = term
                .clone()
                .then(just(';').ignore_then(term.clone()).repeated())
                .foldl(|t1, t2| Self::app(Self::abs("_", Ty::unit(), t2), t1));

            let parens = seq.delimited_by(just('('), just(')'));

            let atom = choice((
                true_, false_, unit, string, float, int, record, tag, inert, parens, var,
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

            let app_ = path.clone().then(path.clone().repeated()).foldl(Self::app);

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

            let app = choice((ref_, deref, fix, times_float, succ, pred, is_zero, app_)).padded();

            let assign = app
                .clone()
                .then_ignore(just(":="))
                .then(app.clone())
                .map(|(t1, t2)| Self::assign(t1, t2));

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

            let cases = just('<')
                .ignore_then(text::ident().padded())
                .then_ignore(just('='))
                .then(Self::ident().padded())
                .then_ignore(just('>'))
                .then_ignore(just("==>").padded())
                .then(term.clone())
                .padded()
                .map(|((l, t1), t2)| (l, t1, t2))
                .separated_by(just('|'))
                .at_least(1);

            let case = text::keyword("case")
                .ignore_then(term.clone())
                .then_ignore(text::keyword("of"))
                .then(cases)
                .map(|(t, cases)| Self::case(t, cases));

            choice((if_, abs, let_, let_rec, case, assign, app)).padded()
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
            .ignore_then(Term::ident().padded())
            .then(Binding::parser())
            .map(|(x, b)| Self::Bind(x, b));
        let bind_type = just(':')
            .then(text::keyword("bindtype"))
            .ignore_then(Term::ident().padded())
            .then(Binding::ty_parser())
            .map(|(x, b)| Self::Bind(x, b));
        let type_ = just(':')
            .then(text::keyword("type"))
            .ignore_then(Term::parser())
            .map(Self::Type);
        let noop = text::whitespace().to(Self::Noop);

        choice((eval1, eval, bind, bind_type, type_, term, noop)).then_ignore(end())
    }
}
