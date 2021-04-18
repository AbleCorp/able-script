use rand::Rng;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Abool {
    Never = -1,
    Sometimes = 0,
    Always = 1,
}

impl Into<bool> for Abool {
    fn into(self) -> bool {
        match self {
            Abool::Never => false,
            Abool::Always => true,
            Abool::Sometimes => rand::thread_rng().gen(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Str(String),
    Int(i32),
    Bool(bool),
    Abool(Abool),
}

#[derive(Debug)]
pub struct Variable {
    melo: bool,
    value: Value,
}
pub fn test() {
    let mut map = HashMap::new();
    let a = Variable {
        melo: false,
        value: Value::Str("1".to_string()),
    };
    let b = Variable {
        melo: false,
        value: Value::Int(2),
    };
    map.insert("a", a);
    map.insert("b", b);

    for (key, value) in &map {
        println!("{}: {:?}", key, value);
    }
}
