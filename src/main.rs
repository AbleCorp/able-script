#![forbid(unsafe_code)]

mod base_55;
mod error;
mod parser;
mod tokens;
mod variables;

use clap::{App, Arg};
use parser::Parser;
fn main() {
    // variables::test();

    let matches = App::new("AbleScript")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Able <abl3theabove@gmail.com>")
        .about("Does awesome things")
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
            let ast = parser.parse();
            println!("{:#?}", ast);
        }
        None => {
            println!("hi");
            //start the prompt
        }
    }
}
