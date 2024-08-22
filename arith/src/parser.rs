use crate::syntax::{Command, Term};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{digit1, space0},
        streaming::space1,
    },
    combinator::{map, map_res, value},
    sequence::{delimited, preceded, terminated},
    IResult,
};

impl Term {
    fn from_num(n: u64) -> Term {
        let mut t = Term::Zero;
        for _ in 0..n {
            t = Term::Succ(Box::new(t));
        }
        t
    }
}

fn parse_term(input: &str) -> IResult<&str, Term> {
    delimited(space0, parse_inner, space0)(input)
}

fn parse_inner(input: &str) -> IResult<&str, Term> {
    alt((
        parse_true,
        parse_false,
        parse_number,
        parse_succ,
        parse_pred,
        parse_iszero,
        parse_if,
        parse_parens,
    ))(input)
}

fn parse_true(input: &str) -> IResult<&str, Term> {
    value(Term::True, tag("true"))(input)
}

fn parse_false(input: &str) -> IResult<&str, Term> {
    value(Term::False, tag("false"))(input)
}

fn parse_number(input: &str) -> IResult<&str, Term> {
    map_res(digit1, |n: &str| n.parse().map(Term::from_num))(input)
}

fn parse_parens(input: &str) -> IResult<&str, Term> {
    delimited(tag("("), parse_term, tag(")"))(input)
}

fn parse_pre_space(input: &str) -> IResult<&str, Term> {
    alt((parse_parens, preceded(space1, parse_inner)))(input)
}

fn parse_delim_spaces(input: &str) -> IResult<&str, Term> {
    alt((parse_parens, delimited(space1, parse_inner, space1)))(input)
}

fn parse_succ(input: &str) -> IResult<&str, Term> {
    let (input, _) = tag("succ")(input)?;
    let (input, t) = parse_pre_space(input)?;
    Ok((input, Term::Succ(Box::new(t))))
}

fn parse_pred(input: &str) -> IResult<&str, Term> {
    let (input, _) = tag("pred")(input)?;
    let (input, t) = parse_pre_space(input)?;
    Ok((input, Term::Pred(Box::new(t))))
}

fn parse_iszero(input: &str) -> IResult<&str, Term> {
    let (input, _) = tag("iszero")(input)?;
    let (input, t) = parse_pre_space(input)?;
    Ok((input, Term::IsZero(Box::new(t))))
}

fn parse_if(input: &str) -> IResult<&str, Term> {
    let (input, _) = tag("if")(input)?;
    let (input, cond) = parse_delim_spaces(input)?;
    let (input, _) = preceded(space0, tag("then"))(input)?;
    let (input, then) = parse_delim_spaces(input)?;
    let (input, _) = preceded(space0, tag("else"))(input)?;
    let (input, else_) = parse_pre_space(input)?;
    Ok((
        input,
        Term::If(Box::new(cond), Box::new(then), Box::new(else_)),
    ))
}

impl Command {
    pub fn parse(input: &str) -> Result<Command, String> {
        match parse_command(input) {
            Ok(("", c)) => Ok(c),
            Ok((rest, _)) => Err(format!("Unexpected input: {rest}")),
            Err(e) => Err(format!("Parse error: {e}")),
        }
    }
}

fn parse_command(input: &str) -> IResult<&str, Command> {
    alt((
        parse_command_eval1,
        parse_command_eval,
        map(parse_term, Command::Eval),
    ))(input)
}

fn parse_command_eval1(input: &str) -> IResult<&str, Command> {
    let (input, _) = terminated(tag(":eval1"), space1)(input)?;
    let (input, t) = parse_term(input)?;
    Ok((input, Command::Eval1(t)))
}

fn parse_command_eval(input: &str) -> IResult<&str, Command> {
    let (input, _) = terminated(tag(":eval"), space1)(input)?;
    let (input, t) = parse_term(input)?;
    Ok((input, Command::Eval(t)))
}
