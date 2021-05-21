use logos::Source;
use rustyline::Editor;

use crate::{interpret::Scope, parser::Parser};

pub fn repl() {
    let mut rl = Editor::<()>::new();
    let mut ctx = Scope::new();
    loop {
        let readline = rl.readline(":: ");
        match readline {
            Ok(line) => {
                if &line == "exit" {
                    println!("bye");
                    break;
                }
                let mut parser = Parser::new(&line);
                let ast = parser.init();
                match ast {
                    Ok(ast) => {
                        println!("{:?}", ast);
                        println!("{:?}", ctx.eval_items(&ast));
                    },
                    Err(e) => {
                        println!(
                            "Error `{:?}` occured at span: {:?} = `{:?}`",
                            e.kind,
                            e.position.clone(),
                            line.slice(e.position)
                        );
                    }
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
