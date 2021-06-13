use std::{fmt::Display, io::Write, ops::Range};

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
    BfFunctio {
        instructions: Vec<u8>,
        tape_len: usize,
    },
    AbleFunctio {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
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
    /// Write an AbleScript value to a Brainfuck input stream. This
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
    pub fn bf_write(&self, stream: &mut impl Write) {
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
                Functio::BfFunctio {
                    instructions,
                    tape_len: _,
                } => {
                    // NOTE(Alex): Tape length should maybe be taken
                    // into account here.
                    stream
                        .write_all(&[0])
                        .and_then(|_| stream.write_all(&(instructions.len() as u32).to_le_bytes()))
                        .and_then(|_| stream.write_all(&instructions))
                }
                Functio::AbleFunctio { params: _, body: _ } => {
                    todo!()
                }
            }),
        }
        .expect("Failed to write to Brainfuck input");
    }

    /// Attempt to coerce a Value to an integer. If the conversion
    /// fails, the generated error message is associated with the
    /// given span.
    pub fn to_i32(self, span: &Range<usize>) -> Result<i32, Error> {
        match self {
            Value::Int(i) => Ok(i),
            _ => Err(Error {
                kind: ErrorKind::TypeError(format!("Expected int, got {}", self)),
                span: span.clone(),
            }),
        }
    }

    /// Coerce a Value to a boolean. The conversion cannot fail.
    pub fn to_bool(self) -> bool {
        match self {
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nul => write!(f, "nul"),
            Value::Str(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Abool(v) => write!(f, "{}", v),
            Value::Functio(v) => match v {
                Functio::BfFunctio {
                    instructions,
                    tape_len,
                } => {
                    write!(
                        f,
                        "({}) {}",
                        tape_len,
                        String::from_utf8(instructions.to_owned())
                            .expect("Brainfuck functio source should be UTF-8")
                    )
                }
                Functio::AbleFunctio { params, body } => {
                    write!(
                        f,
                        "({}) -> {:?}",
                        params.join(", "),
                        // Maybe we should have a pretty-printer for
                        // statement blocks at some point?
                        body,
                    )
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub melo: bool,
    pub value: Value,
}
