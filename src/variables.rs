use rand::Rng;

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
    Nul,
}

#[derive(Debug)]
pub struct Variable {
    melo: bool,
    value: Value,
}
