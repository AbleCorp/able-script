use std::io::{stdin, stdout, Write};

pub fn prompt() {
    loop {
        print! {"> "};
        stdout().flush().unwrap();
        let mut input = String::new();
        stdin().read_line(&mut input).unwrap();

        if input.trim() == "quit" {
            break;
        } else {
            run(input);
        }
    }
}

fn run(s: String) {
    println!("{}", s);
}
