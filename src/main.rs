#![forbid(unsafe_code)]

mod ast;
mod base_55;
mod brian;
mod error;
mod interpret;
mod lexer;
mod parser;
mod repl;
mod variables;

use std::process::exit;

use clap::{App, Arg};
use interpret::ExecEnv;
use logos::Source;
use parser::Parser;

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
                    source.slice(e.span)
                );
            }
        }
        None => {
            println!("Hi [AbleScript {}]", env!("CARGO_PKG_VERSION"));
            repl::repl(ast_print);
        }
    }
}
