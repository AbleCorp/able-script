#![forbid(unsafe_code, clippy::unwrap_used)]
mod repl;

use std::process::exit;

use ablescript::interpret::ExecEnv;
use ablescript::parser::Parser;
use clap::{App, Arg};

fn main() {
    // variables::test(); // NOTE(Able): Add this as a test case
    let matches = App::new("AbleScript")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Able <abl3theabove@gmail.com>")
        .about("AbleScript interpreter")
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .value_name("FILE")
                .help("Set the path to interpret from")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("debug")
                .long("debug")
                .help("Enable debug AST printing"),
        )
        .get_matches();

    let ast_print = matches.is_present("debug");

    match matches.value_of("file") {
        Some(file_path) => {
            // Read file
            let source = match std::fs::read_to_string(file_path) {
                Ok(s) => s,
                Err(e) => {
                    println!("Failed to read file \"{}\": {}", file_path, e);
                    exit(1)
                }
            };

            // Parse & evaluate
            let mut parser = Parser::new(&source);
            if let Err(e) = parser.init().and_then(|ast| {
                if ast_print {
                    println!("{:#?}", ast);
                }
                ExecEnv::new().eval_stmts(&ast)
            }) {
                println!(
                    "Error `{:?}` occurred at span: {:?} = `{:?}`",
                    e.kind,
                    e.span.clone(),
                    &source[e.span]
                );
            }
        }
        None => {
            println!("Hi [AbleScript {}]", env!("CARGO_PKG_VERSION"));
            repl::repl(ast_print);
        }
    }
}
