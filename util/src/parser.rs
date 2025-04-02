use chumsky::prelude::*;

pub type ParserError<'src> = extra::Err<Rich<'src, char>>;

pub fn string<'src>() -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
    let escape = just('\\').ignore_then(one_of("\\\"nrt").map(|c| match c {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        c => c,
    }));

    just('"')
        .ignore_then(escape.or(none_of("\\\"")).repeated().collect())
        .then_ignore(just('"'))
        .labelled("string")
}

pub fn float<'src>() -> impl Parser<'src, &'src str, f64, ParserError<'src>> + Clone {
    text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str::<f64>()
        .try_map(|s, span| s.map_err(|e| Rich::custom(span, e)))
        .labelled("float")
}

pub fn int<'src>() -> impl Parser<'src, &'src str, u64, ParserError<'src>> + Clone {
    text::int(10)
        .to_slice()
        .from_str::<u64>()
        .try_map(|s, span| s.map_err(|e| Rich::custom(span, e)))
        .labelled("int")
}

fn ident<'src>(
    kerwords: impl IntoIterator<Item = &'src str>,
) -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
    let kerwords = kerwords.into_iter().collect::<Vec<_>>();
    text::ident().try_map(move |s, span| {
        if kerwords.contains(&s) {
            Err(Rich::custom(span, format!("unexpected keyword: {s}")))
        } else {
            Ok(s.to_owned())
        }
    })
}

pub fn var_ident<'src>(
    kerwords: impl IntoIterator<Item = &'src str>,
) -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
    ident(kerwords)
        .try_map(|s, span| {
            if s.chars().next().is_some_and(char::is_lowercase) {
                Ok(s)
            } else {
                Err(Rich::custom(
                    span,
                    format!("expected a variable identifier, got {s}"),
                ))
            }
        })
        .labelled("variable identifier")
}

pub fn ty_ident<'src>(
    kerwords: impl IntoIterator<Item = &'src str>,
) -> impl Parser<'src, &'src str, String, ParserError<'src>> + Clone {
    ident(kerwords)
        .try_map(|s, span| {
            if s.chars().next().is_some_and(char::is_uppercase) {
                Ok(s)
            } else {
                Err(Rich::custom(
                    span,
                    format!("expected a type identifier, got {s}"),
                ))
            }
        })
        .labelled("type identifier")
}
