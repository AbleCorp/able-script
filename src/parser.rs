use crate::tokens;

pub fn parse(line: String) {
    //match the tokens
    //This will not work
    let iter = line.split_whitespace();
    for x in iter {
        match x {
            "#" => {
                println!("hi");
            }
            _ => {}
        }
    }
}
