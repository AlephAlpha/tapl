#![warn(clippy::nursery)]
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::uninlined_format_args)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::large_stack_frames)]

mod eval;
mod parser;
mod syntax;

use eval::Store;
use rustyline::{Editor, Result, history::DefaultHistory};
use syntax::{COMMANDS, Command, Context, KEYWORDS};
use util::KeywordsCompleter;

fn main() -> Result<()> {
    let mut rl = Editor::<KeywordsCompleter, DefaultHistory>::new()?;
    let completer = KeywordsCompleter::new(KEYWORDS.iter().copied(), COMMANDS.iter().copied());
    rl.set_helper(Some(completer));

    let mut ctx = Context::new();
    let mut store = Store::new();

    loop {
        let input = rl.readline("fullref> ");
        match input {
            Ok(line) => {
                rl.add_history_entry(&line).ok();
                match Command::parse(line.as_str()) {
                    Ok(cmd) => {
                        match cmd {
                            Command::Eval(t) => match t.type_of(&mut ctx) {
                                Ok(ty) => match t.eval(&mut ctx, &mut store) {
                                    Ok(t_) => println!("{t_}: {ty}"),
                                    Err(err) => eprintln!("Evaluation error: {err}"),
                                },
                                Err(err) => eprintln!("Type error: {err}"),
                            },
                            Command::Eval1(t) => match t.type_of(&mut ctx) {
                                Ok(ty) => match t.eval1(&mut ctx, &mut store) {
                                    Ok(t_) => println!("{t_}: {ty}"),
                                    Err(err) => eprintln!("Evaluation error: {err}"),
                                },
                                Err(err) => eprintln!("Type error: {err}"),
                            },
                            Command::Bind(x, b) => {
                                match b.to_de_bruijn(&mut ctx).and_then(|b_| b_.check(&mut ctx)) {
                                    Ok(b_) => {
                                        ctx.add_binding(&x, b_.eval(&ctx, &mut store));
                                        store.shift(1).unwrap();
                                        rl.helper_mut().unwrap().add_keyword(x);
                                    }
                                    Err(err) => eprintln!("Binding error: {err}"),
                                }
                            }
                            Command::Type(t) => match t.type_of(&mut ctx) {
                                Ok(ty) => println!("{t}: {ty}"),
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
