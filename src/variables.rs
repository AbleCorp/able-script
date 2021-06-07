use std::{convert::TryFrom, fmt::Display, io::Write};

use rand::Rng;

use crate::{
    ast::Stmt,
    error::{Error, ErrorKind},
};

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

#[derive(Debug, PartialEq, Clone)]
pub enum Functio {
    BfFunctio(Vec<u8>),
    AbleFunctio(Vec<Stmt>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nul,
    Str(String),
    Int(i32),
    Bool(bool),
    Abool(Abool),
    Functio(Functio),
}

impl Value {
    /// Writes an AbleScript value to a Brainfuck input stream. This
    /// should generally only be called on `Write`rs that cannot fail,
    /// e.g., `Vec<u8>`, because any IO errors will cause a panic.
    ///
    /// The mapping from values to encodings is as follows, where all
    /// multi-byte integers are little-endian:
    ///
    /// | AbleScript representation | Brainfuck representation                                    |
    /// |---------------------------|-------------------------------------------------------------|
    /// | Nul                       | `00`                                                        |
    /// | Str                       | `01` [length, 4 bytes] [string, \[LENGTH\] bytes, as UTF-8] |
    /// | Int                       | `02` [value, 4 bytes]                                       |
    /// | Bool                      | `03` `00` false, `03` `01` true.                            |
    /// | Abool                     | `04` `00` never, `04` `01` always, `04` `02` sometimes.     |
    /// | Brainfuck Functio         | `05` `00` [length, 4 bytes] [source code, \[LENGTH\] bytes] |
    /// | AbleScript Functio        | `05` `01` (todo, not yet finalized or implemented)          |
    ///
    /// The existing mappings should never change, as they are
    /// directly visible from Brainfuck code and modifying them would
    /// break a significant amount of AbleScript code. If more types
    /// are added in the future, they should be assigned the remaining
    /// discriminant bytes from 06..FF.
    pub fn bf_write(&mut self, stream: &mut impl Write) {
        match self {
            Value::Nul => stream.write_all(&[0]),
            Value::Str(s) => stream
                .write_all(&[1])
                .and_then(|_| stream.write_all(&(s.len() as u32).to_le_bytes()))
                .and_then(|_| stream.write_all(&s.as_bytes())),
            Value::Int(v) => stream
                .write_all(&[2])
                .and_then(|_| stream.write_all(&v.to_le_bytes())),
            Value::Bool(b) => stream
                .write_all(&[3])
                .and_then(|_| stream.write_all(&[*b as _])),
            Value::Abool(a) => stream.write_all(&[4]).and_then(|_| {
                stream.write_all(&[match *a {
                    Abool::Never => 0,
                    Abool::Sometimes => 2,
                    Abool::Always => 1,
                }])
            }),
            Value::Functio(f) => stream.write_all(&[5]).and_then(|_| match f {
                Functio::BfFunctio(f) => stream
                    .write_all(&[0])
                    .and_then(|_| stream.write_all(&(f.len() as u32).to_le_bytes()))
                    .and_then(|_| stream.write_all(&f)),
                Functio::AbleFunctio(_) => {
                    todo!()
                }
            }),
        }
        .expect("Failed to write to Brainfuck input");
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nul => write!(f, "nul"),
            Value::Str(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Abool(v) => write!(f, "{}", v),
            Value::Functio(v) => match v {
                Functio::BfFunctio(source) => {
                    write!(
                        f,
                        "{}",
                        String::from_utf8(source.to_owned())
                            .expect("Brainfuck functio source should be UTF-8")
                    )
                }
                Functio::AbleFunctio(source) => {
                    // TODO: what's the proper way to display an
                    // AbleScript functio?
                    write!(f, "{:?}", source)
                }
            },
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
                span: 0..0,
            }),
        }
    }
}

// Coercions from a value to a boolean always succeed, so every value
// can be used in an `if` statement. C does things that way, so how
// could it possibly be a bad idea?
impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            // Booleans and abooleans have a trivial conversion.
            Value::Bool(b) => b,
            Value::Abool(b) => b.into(),
            // The empty string is falsey, other strings are truthy.
            Value::Str(s) => s.len() != 0,
            // 0 is falsey, nonzero is truthy.
            Value::Int(x) => x != 0,
            // Functios are always truthy.
            Value::Functio(_) => true,
            // And nul is truthy as a symbol of the fact that the
            // deep, fundamental truth of this world is nothing but
            // the eternal void.
            Value::Nul => true,
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub melo: bool,
    pub value: Value,
}
