#![forbid(unsafe_code)]

mod base_55;
mod error;
mod lexer;
mod parser;
mod repl;
mod variables;

use clap::{App, Arg};
use logos::Source;
use parser::Parser;
fn main() {
    // variables::test();

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
                Ok(ast) => println!("{:#?}", ast),
                Err(e) => {
                    println!(
                        "Error `{:?}` occured at span: {:?} = `{:?}`",
                        e.kind,
                        e.position.clone(),
                        source.slice(e.position)
                    );
                }
            }
        }
        None => {
            println!(
                "Hi [AbleScript {}] - AST Printer",
                env!("CARGO_PKG_VERSION")
            );
            repl::repl();
        }
    }
}
