use std::env;
pub use std::f64::consts::{
    PI, // For the heretics among us
    TAU,
};

mod file_load;
mod prompt;
mod tokens;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    if args.len() > 1 {
        println!("Usage: alang [script]");
    } else if args.len() == 1 {
        let filepath = &args[0];
        file_load::run_file(filepath);
    } else {
        prompt::prompt();
    }
}
