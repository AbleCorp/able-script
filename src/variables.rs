use std::collections::HashMap;

#[derive(Debug)]
enum Value {
    Str(String),
    Int(i32),
    Bool(bool),
    //TODO(Able): Add abool and other variable types
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
