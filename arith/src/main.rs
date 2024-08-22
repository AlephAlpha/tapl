mod eval;
mod parser;
mod syntax;

use either::{Left, Right};
use rustyline::{DefaultEditor, Result};
use syntax::Command;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let input = rl.readline("arith> ");
        match input {
            Ok(line) => {
                rl.add_history_entry(&line).ok();
                match Command::parse(&line) {
                    Ok(cmd) => {
                        match cmd {
                            Command::Eval(t) => println!("{}", t.eval()),
                            Command::Eval1(t) => match t.eval1() {
                                Left(t_) => println!("{t_}"),
                                Right(t_) => eprintln!("No rule applies: {t_}"),
                            },
                        };
                    }
                    Err(e) => eprintln!("{e}"),
                }
            }
            Err(_) => break,
        }
    }

    Ok(())
}
