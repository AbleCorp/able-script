use rustyline::Editor;

use crate::parser::Parser;

pub fn repl() {
    let mut rl = Editor::<()>::new();
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
                println!("{:#?}", ast);
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
