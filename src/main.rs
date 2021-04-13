extern crate clap;
use clap::{App, Arg};

mod base_55;
mod parser;
pub mod tokens;
mod scanner;

use logos::Logos;
use scanner::Scanner;

fn main() {
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

            // Print token type: `value`
            let mut scanner = Scanner::new(&source);
            scanner.scan();
        }
        None => {
            println!("hi");
            //start the prompt
        }
    }
}
