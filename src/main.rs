#![forbid(unsafe_code)]

mod ast;
mod base_55;
mod brian;
mod consts;
mod error;
mod interpret;
mod lexer;
mod parser;
mod repl;
mod variables;

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
        .get_matches();

    match matches.value_of("file") {
        Some(file_path) => {
            // Read file
            let source = std::fs::read_to_string(file_path).unwrap();

            // Parse
            let mut parser = Parser::new(&source);
            let ast = parser.init();
            match ast {
                Ok(ast) => {
                    println!("{:#?}", ast);
                    let mut env = ExecEnv::new();
                    println!("{:?}", env.eval_stmts(&ast));
                }
                Err(e) => {
                    println!(
                        "Error `{:?}` occurred at span: {:?} = `{:?}`",
                        e.kind,
                        e.span.clone(),
                        source.slice(e.span)
                    );
                }
            }
        }
        None => {
            println!(
                "Hi [AbleScript {}] - AST Printer & Interpreter",
                env!("CARGO_PKG_VERSION")
            );
            repl::repl();
        }
    }
}
