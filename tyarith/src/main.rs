#![warn(clippy::nursery)]
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::uninlined_format_args)]

mod eval;
mod parser;
mod syntax;

use rustyline::{Editor, Result, history::DefaultHistory};
use syntax::{COMMANDS, Command, KEYWORDS};
use util::KeywordsCompleter;

fn main() -> Result<()> {
    let mut rl = Editor::<KeywordsCompleter, DefaultHistory>::new()?;
    let completer = KeywordsCompleter::new(KEYWORDS.iter().copied(), COMMANDS.iter().copied());
    rl.set_helper(Some(completer));

    loop {
        let input = rl.readline("tyarith> ");
        match input {
            Ok(line) => {
                rl.add_history_entry(&line).ok();
                match Command::parse(line.as_str()) {
                    Ok(cmd) => {
                        match cmd {
                            Command::Eval(t) => match t.type_of() {
                                Ok(ty) => println!("{} : {ty}", t.eval()),
                                Err(err) => eprintln!("Type error: {err}"),
                            },
                            Command::Eval1(t) => match t.type_of() {
                                Ok(ty) => match t.eval1() {
                                    Ok(t_) => println!("{t_} : {ty}"),
                                    Err(err) => eprintln!("Evaluation error: {err}"),
                                },
                                Err(err) => eprintln!("Type error: {err}"),
                            },
                            Command::Type(t) => match t.type_of() {
                                Ok(ty) => println!("{t} : {ty}"),
                                Err(err) => eprintln!("Type error: {err}"),
                            },
                            Command::Noop => {}
                        };
                    }
                    Err(errs) => {
                        for err in errs {
                            eprintln!("Parse error: {err}");
                        }
                    }
                }
            }
            Err(_) => break,
        }
    }

    Ok(())
}
