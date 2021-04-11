extern crate clap;
use clap::{App, Arg};

mod base_55;
mod parser;
pub mod tokens;
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
            // Start parsing that file
            parser::parse(file_path.to_string());
        }
        None => {
            println!("hi");
            //start the prompt
        }
    }
}
