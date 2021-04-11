extern crate clap;
use clap::{App, Arg};
mod base_55;
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
                .help("Sets a custom config file")
                .takes_value(true),
        )
        .get_matches();

    match matches.value_of("file") {
        Some(file_path) => {
            println!("{}", file_path);
            // Start parsing that file
            for x in file_path.chars() {
                println!("{}", base_55::char2num(x));
            }
        }
        None => {
            println!("hi");
            //start the prompt
        }
    }
}
