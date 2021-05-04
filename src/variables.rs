use rand::Rng;

#[derive(Debug, Clone, PartialEq)]
pub enum Abool {
    Never = -1,
    Sometimes = 0,
    Always = 1,
}

impl From<Abool> for bool {
    fn from(val: Abool) -> Self {
        match val {
            Abool::Never => false,
            Abool::Always => true,
            Abool::Sometimes => rand::thread_rng().gen(), // NOTE(Able): This is amazing and should be applied anywhere abooleans exist
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
