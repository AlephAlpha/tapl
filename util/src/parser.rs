use chumsky::prelude::*;

pub fn string() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    let escape = just('\\').ignore_then(one_of("\\\"nrt").map(|c| match c {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        c => c,
    }));

    just('"')
        .ignore_then(escape.or(none_of("\\\"")).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
}

pub fn float() -> impl Parser<char, f64, Error = Simple<char>> + Clone {
    text::int(10)
        .chain::<char, _, _>(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .try_map(|s: String, span| {
            s.parse::<f64>()
                .map_err(|e| Simple::custom(span, e.to_string()))
        })
}

pub fn int() -> impl Parser<char, u64, Error = Simple<char>> + Clone {
    text::int(10).try_map(|s: String, span| {
        s.parse::<u64>()
            .map_err(|e| Simple::custom(span, e.to_string()))
    })
}

fn ident(
    kerwords: impl IntoIterator<Item = impl Into<String>>,
) -> impl Parser<char, String, Error = Simple<char>> + Clone {
    let kerwords = kerwords.into_iter().map(Into::into).collect::<Vec<_>>();
    text::ident().try_map(move |s: String, span| {
        if kerwords.contains(&s) {
            Err(Simple::custom(span, format!("unexpected keyword: {s}")))
        } else {
            Ok(s)
        }
    })
}

pub fn var_ident(
    kerwords: impl IntoIterator<Item = impl Into<String>>,
) -> impl Parser<char, String, Error = Simple<char>> + Clone {
    ident(kerwords).try_map(|s: String, span| {
        if s.chars().next().is_some_and(char::is_lowercase) {
            Ok(s)
        } else {
            Err(Simple::custom(
                span,
                format!("expected a variable identifier, got {s}"),
            ))
        }
    })
}

pub fn ty_ident(
    kerwords: impl IntoIterator<Item = impl Into<String>>,
) -> impl Parser<char, String, Error = Simple<char>> + Clone {
    ident(kerwords).try_map(|s: String, span| {
        if s.chars().next().is_some_and(char::is_uppercase) {
            Ok(s)
        } else {
            Err(Simple::custom(
                span,
                format!("expected a type identifier, got {s}"),
            ))
        }
    })
}
