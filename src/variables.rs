use std::{convert::TryFrom, fmt::Display};

use rand::Rng;

use crate::error::{Error, ErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Abool {
    Never = -1,
    Sometimes = 0,
    Always = 1,
}

impl Display for Abool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Abool::Never => write!(f, "never"),
            Abool::Sometimes => write!(f, "sometimes"),
            Abool::Always => write!(f, "always"),
        }
    }
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Abool(v) => write!(f, "{}", v),
            Value::Nul => write!(f, "nul"),
        }
    }
}

impl TryFrom<Value> for i32 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(Error {
                kind: ErrorKind::TypeError(format!("Expected int, got {}", value)),
                // TODO: either add some kind of metadata to `Value`
                // so we can tell where the value came from and assign
                // this `position` correctly, or re-write the
                // `error::Error` struct so we can omit the `position`
                // when using some error kinds.
                position: 0..0,
            }),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(b) => Ok(b),
            Value::Abool(b) => Ok(b.into()),
            _ => Err(Error {
                kind: ErrorKind::TypeError(format!("Expected bool, got {}", value)),
                position: 0..0,
            }),
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub melo: bool,
    pub value: Value,
}
