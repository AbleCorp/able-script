extern crate clap;
use clap::{App, Arg};

fn main() {
    let matches = App::new("My Super Program")
        .version("1.0")
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
        }
        None => {
            println!("hi");
            //start the prompt
        }
    }
}
