use rustyline::Editor;

use ablescript::interpret::ExecEnv;
use ablescript::parser::Parser;

pub fn repl(ast_print: bool) {
    let mut rl = Editor::<()>::new();
    let mut env = ExecEnv::new();
    loop {
        let readline = rl.readline(":: ");
        match readline {
            Ok(line) => {
                // NOTE(Alex): `readline()` leaves a newline at the
                // end of the string if stdin is connected to a file
                // or unsupported terminal; this can interfere with
                // error printing.
                rl.add_history_entry(&line);
                let line = line.trim_end();

                if line == "exit" {
                    println!("bye");
                    break;
                }

                let mut parser = Parser::new(line);
                let value = parser.init().and_then(|ast| {
                    if ast_print {
                        println!("{:#?}", &ast);
                    }
                    env.eval_stmts(&ast)
                });

                if let Err(e) = value {
                    println!("{}", e);
                    println!(" | {}", line);
                    println!(
                        "   {}{}",
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
