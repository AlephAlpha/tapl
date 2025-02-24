#![warn(clippy::nursery)]
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::uninlined_format_args)]

mod eval;
mod parser;
mod syntax;

use eval::{Constr, UVarGen};
use rustyline::{Editor, Result, history::DefaultHistory};
use syntax::{COMMANDS, Command, Context, KEYWORDS};
use util::KeywordsCompleter;

fn main() -> Result<()> {
    let mut rl = Editor::<KeywordsCompleter, DefaultHistory>::new()?;
    let completer = KeywordsCompleter::new(KEYWORDS.iter().copied(), COMMANDS.iter().copied());
    rl.set_helper(Some(completer));

    let mut ctx = Context::new();
    let mut constr = Constr::new();
    let mut uvar_gen = UVarGen::new();

    loop {
        let input = rl.readline("recon> ");
        match input {
            Ok(line) => {
                rl.add_history_entry(&line).ok();
                match Command::parse(line.as_str()) {
                    Ok(cmd) => {
                        match cmd {
                            Command::Eval(t) => {
                                match t.type_of(&mut ctx, &mut uvar_gen, &mut constr) {
                                    Ok(ty) => match t.eval(&mut ctx) {
                                        Ok(t_) => println!("{t_}: {ty}"),
                                        Err(err) => eprintln!("Evaluation error: {err}"),
                                    },
                                    Err(err) => eprintln!("Type error: {err}"),
                                }
                            }
                            Command::Eval1(t) => {
                                match t.type_of(&mut ctx, &mut uvar_gen, &mut constr) {
                                    Ok(ty) => match t.eval1(&mut ctx) {
                                        Ok(t_) => println!("{t_}: {ty}"),
                                        Err(err) => eprintln!("Evaluation error: {err}"),
                                    },
                                    Err(err) => eprintln!("Type error: {err}"),
                                }
                            }
                            Command::Bind(x, b) => {
                                ctx.add_binding(&x, b);
                                rl.helper_mut().unwrap().add_keyword(x);
                            }
                            Command::Type(t) => {
                                match t.type_of(&mut ctx, &mut uvar_gen, &mut constr) {
                                    Ok(ty) => println!("{t}: {ty}"),
                                    Err(err) => eprintln!("Type error: {err}"),
                                }
                            }
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
