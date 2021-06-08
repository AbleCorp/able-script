use logos::Source;
use rustyline::Editor;

use crate::{interpret::ExecEnv, parser::Parser};

pub fn repl() {
    let mut rl = Editor::<()>::new();
    let mut env = ExecEnv::new();
    loop {
        let readline = rl.readline(":: ");
        match readline {
            Ok(line) => {
                if &line == "exit" {
                    println!("bye");
                    break;
                }
                let mut parser = Parser::new(&line);
                let value = parser.init().and_then(|ast| {
                    println!("{:?}", &ast);
                    env.eval_stmts(&ast)
                });

                if let Err(e) = value {
                    println!(
                        "Error `{:?}` occurred at span: {:?} = `{:?}`",
                        e.kind,
                        e.span.clone(),
                        line.slice(e.span.clone())
                    );

                    println!(" | {}", line);
                    println!(
                        "   {}{}-- Here",
                        " ".repeat(e.span.start),
                        "^".repeat((e.span.end - e.span.start).max(1))
                    );
                }
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("bye");
                break;
            }
            Err(rustyline::error::ReadlineError::Interrupted) => (),
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        }
    }
}
