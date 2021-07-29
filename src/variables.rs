use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, io::Write, rc::Rc};

use rand::Rng;

use crate::{ast::Stmt, consts};

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone)]
pub enum Value {
    Nul,
    Str(String),
    Int(i32),
    Bool(bool),
    Abool(Abool),
    Functio(Functio),
    Cart(HashMap<Value, Rc<RefCell<Value>>>),
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Nul => (),
            Value::Str(v) => v.hash(state),
            Value::Int(v) => v.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Abool(v) => v.to_string().hash(state),
            Value::Functio(_) => todo!(),
            Value::Cart(_) => self.to_string().hash(state),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nul, Value::Nul) => true,
            (Value::Str(left), Value::Str(right)) => left == right,
            (Value::Int(left), Value::Int(right)) => left == right,
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Abool(left), Value::Abool(right)) => left == right,
            (Value::Functio(left), Value::Functio(right)) => left == right,
            (Value::Cart(_left), Value::Cart(_right)) => {
                todo!()
            }
            (_, _) => false,
            // TODO: do more coercions!
        }
    }
}

impl Eq for Value {}

impl Value {
    /// Write an AbleScript value to a Brainfuck input stream by
    /// coercing the value to an integer, then truncating that integer
    /// to a single byte, then writing that byte. This should only be
    /// called on `Write`rs that cannot fail, e.g., `Vec<u8>`, because
    /// any IO errors will cause a panic.
    pub fn bf_write(&self, stream: &mut impl Write) {
        stream
            .write_all(&[self.clone().to_i32() as u8])
            .expect("Failed to write to Brainfuck input");
    }

    /// Coerce a value to an integer.
    pub fn to_i32(&self) -> i32 {
        match self {
            Value::Abool(a) => *a as _,
            Value::Bool(b) => *b as _,
            Value::Functio(func) => match func {
                Functio::BfFunctio {
                    instructions,
                    tape_len,
                } => (instructions.len() + tape_len) as _,
                Functio::AbleFunctio { params, body } => (params.len() + body.len()) as _,
            },
            Value::Int(i) => *i,
            Value::Nul => consts::ANSWER,
            Value::Str(text) => text.parse().unwrap_or(consts::ANSWER),
            Value::Cart(c) => c.len() as _,
        }
    }

    /// Coerce a Value to a boolean. The conversion cannot fail.
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Abool(b) => (*b).into(),
            Value::Bool(b) => *b,
            Value::Functio(_) => true,
            Value::Int(x) => *x != 0,
            Value::Nul => true,
            Value::Str(s) => !s.is_empty(),
            Value::Cart(c) => !c.is_empty(),
        }
    }

    /// Index a value with another value, as in the "a[b]" syntax.
    pub fn index(&self, index: &Value) -> Rc<RefCell<Value>> {
        Rc::new(RefCell::new(match self {
            Value::Nul => Value::Nul,
            Value::Str(s) => Value::Int(s.as_bytes()[index.to_i32() as usize] as i32),
            Value::Int(i) => Value::Int(
                (format!("{}", i).as_bytes()[index.to_i32() as usize] - b'0') as i32,
            ),
            Value::Bool(b) => Value::Int(
                format!("{}", b)
                    .chars()
                    .nth(index.to_i32() as usize)
                    .unwrap_or('?') as i32,
            ),
            Value::Abool(b) => Value::Int(*b as i32),
            Value::Functio(_) => Value::Int(42),
            Value::Cart(c) => {
                return (c.get(index).cloned()).unwrap_or_else(|| Rc::new(RefCell::new(Value::Nul)))
            }
        }))
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
            Value::Cart(c) => {
                write!(f, "[")?;

                for (key, value) in c {
                    write!(f, "{} <= {},", value.borrow(), key)?;
                }

                write!(f, "]")
            }
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    pub melo: bool,

    // Multiple Variables can reference the same underlying Value when
    // pass-by-reference is used, therefore we use Rc here.
    pub value: Rc<RefCell<Value>>,
}
