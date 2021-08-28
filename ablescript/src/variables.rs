use std::{
    cell::RefCell, collections::HashMap, convert::TryFrom, fmt::Display, hash::Hash, io::Write,
    mem::discriminant, ops, rc::Rc, vec,
};

use rand::Rng;

use crate::{ast::Stmt, consts};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Functio {
    BfFunctio {
        instructions: Vec<u8>,
        tape_len: usize,
    },
    AbleFunctio {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    Eval(String),
}

pub type Cart = HashMap<Value, Rc<RefCell<Value>>>;

#[derive(Debug, Clone)]
pub enum Value {
    Nul,
    Str(String),
    Int(i32),
    Bool(bool),
    Abool(Abool),
    Functio(Functio),
    Cart(Cart),
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);
        match self {
            Value::Nul => (),
            Value::Str(v) => v.hash(state),
            Value::Int(v) => v.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::Abool(v) => v.to_string().hash(state),
            Value::Functio(statements) => statements.hash(state),
            Value::Cart(_) => self.to_string().hash(state),
        }
    }
}

impl Value {
    /// Write an AbleScript value to a Brainfuck input stream by
    /// coercing the value to an integer, then truncating that integer
    /// to a single byte, then writing that byte. This should only be
    /// called on `Write`rs that cannot fail, e.g., `Vec<u8>`, because
    /// any IO errors will cause a panic.
    pub fn bf_write(&self, stream: &mut impl Write) {
        stream
            .write_all(&[self.clone().into_i32() as u8])
            .expect("Failed to write to Brainfuck input");
    }

    /// Coerce a value to an integer.
    pub fn into_i32(self) -> i32 {
        match self {
            Value::Abool(a) => a as _,
            Value::Bool(b) => b as _,
            Value::Functio(func) => match func {
                Functio::BfFunctio {
                    instructions,
                    tape_len,
                } => (instructions.len() + tape_len) as _,
                Functio::AbleFunctio { params, body } => (params.len() + body.len()) as _,
                Functio::Eval(s) => s.len() as _,
            },
            Value::Int(i) => i,
            Value::Nul => consts::ANSWER,
            Value::Str(text) => text.parse().unwrap_or(consts::ANSWER),
            Value::Cart(c) => c.len() as _,
        }
    }

    /// Coerce a Value to a boolean. The conversion cannot fail.
    pub fn into_bool(self) -> bool {
        match self {
            Value::Abool(b) => b.into(),
            Value::Bool(b) => b,
            Value::Functio(_) => true,
            Value::Int(x) => x != 0,
            Value::Nul => false,
            Value::Str(s) => match s.to_lowercase().as_str() {
                "false" | "no" | "🇳🇴" => false,
                "true" | "yes" => true,
                s => !s.is_empty(),
            },
            Value::Cart(c) => !c.is_empty(),
        }
    }

    /// Coerce a Value to an aboolean
    pub fn into_abool(self) -> Abool {
        match self {
            Value::Nul => Abool::Never,
            Value::Str(s) => match s.to_lowercase().as_str() {
                "never" => Abool::Never,
                "sometimes" => Abool::Sometimes,
                "always" => Abool::Always,
                s => {
                    if s.is_empty() {
                        Abool::Never
                    } else {
                        Abool::Always
                    }
                }
            },
            Value::Int(x) => match x.cmp(&0) {
                std::cmp::Ordering::Less => Abool::Never,
                std::cmp::Ordering::Equal => Abool::Sometimes,
                std::cmp::Ordering::Greater => Abool::Always,
            },
            Value::Bool(b) => {
                if b {
                    Abool::Always
                } else {
                    Abool::Never
                }
            }
            Value::Abool(a) => a,
            Value::Functio(_) => todo!(),
            Value::Cart(c) => {
                if c.is_empty() {
                    Abool::Never
                } else {
                    Abool::Always
                }
            }
        }
    }

    /// Coerce a Value to a functio
    pub fn into_functio(self) -> Functio {
        match self {
            Value::Nul => Functio::AbleFunctio {
                body: vec![],
                params: vec![],
            },
            Value::Str(s) => Functio::Eval(s),
            Value::Int(i) => todo!(),
            Value::Bool(_) => todo!(),
            Value::Abool(_) => todo!(),
            Value::Functio(f) => f,
            Value::Cart(_) => todo!(),
        }
    }

    pub fn into_cart(self) -> Cart {
        match self {
            Value::Nul => HashMap::new(),
            Value::Str(s) => s
                .chars()
                .enumerate()
                .map(|(i, x)| {
                    (
                        Value::Int(i as i32 + 1),
                        Rc::new(RefCell::new(Value::Str(x.to_string()))),
                    )
                })
                .collect(),
            Value::Int(i) => Value::Str(i.to_string()).into_cart(),
            Value::Bool(b) => Value::Str(b.to_string()).into_cart(),
            Value::Abool(a) => Value::Str(a.to_string()).into_cart(),
            Value::Functio(f) => match f {
                Functio::BfFunctio {
                    instructions,
                    tape_len,
                } => {
                    let mut cart: Cart = instructions
                        .into_iter()
                        .enumerate()
                        .map(|(i, x)| {
                            (
                                Value::Int(i as i32 + 1),
                                Rc::new(RefCell::new(
                                    char::from_u32(x as u32)
                                        .map(|x| Value::Str(x.to_string()))
                                        .unwrap_or(Value::Nul),
                                )),
                            )
                        })
                        .collect();

                    cart.insert(
                        Value::Str("tapelen".to_owned()),
                        Rc::new(RefCell::new(Value::Int(tape_len as _))),
                    );
                    cart
                }
                Functio::AbleFunctio { params, body } => {
                    let params: Cart = params
                        .into_iter()
                        .enumerate()
                        .map(|(i, x)| {
                            (
                                Value::Int(i as i32 + 1),
                                Rc::new(RefCell::new(Value::Str(x))),
                            )
                        })
                        .collect();

                    let body: Cart = body
                        .into_iter()
                        .enumerate()
                        .map(|(i, x)| {
                            (
                                Value::Int(i as i32 + 1),
                                Rc::new(RefCell::new(Value::Str(format!("{:?}", x)))),
                            )
                        })
                        .collect();

                    let mut cart = HashMap::new();
                    cart.insert(
                        Value::Str("params".to_owned()),
                        Rc::new(RefCell::new(Value::Cart(params))),
                    );

                    cart.insert(
                        Value::Str("body".to_owned()),
                        Rc::new(RefCell::new(Value::Cart(body))),
                    );

                    cart
                }
                Functio::Eval(s) => Value::Str(s).into_cart(),
            },
            Value::Cart(c) => c,
        }
    }
}

impl ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Nul => todo!(),
            Value::Str(s) => Value::Str(format!("{}{}", s, rhs.to_string())),
            Value::Int(i) => Value::Int(i.checked_add(rhs.into_i32()).unwrap_or(consts::ANSWER)),
            Value::Bool(b) => Value::Bool(b ^ rhs.into_bool()),
            Value::Abool(a) => Value::Abool({
                let rhs = rhs.into_abool();
                if a == rhs {
                    a
                } else if a == Abool::Sometimes {
                    if rand::thread_rng().gen() {
                        Abool::Sometimes
                    } else {
                        rhs
                    }
                } else if rhs == Abool::Sometimes {
                    if rand::thread_rng().gen() {
                        Abool::Sometimes
                    } else {
                        a
                    }
                } else {
                    Abool::Sometimes
                }
            }),
            Value::Functio(f) => Value::Functio(todo!()),
            Value::Cart(c) => {
                Value::Cart(c.into_iter().chain(rhs.into_cart().into_iter()).collect())
            }
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let other = other.clone();

        match self {
            Value::Nul => other == Value::Nul,
            Value::Str(s) => *s == other.to_string(),
            Value::Int(i) => *i == other.into_i32(),
            Value::Bool(b) => *b == other.into_bool(),
            Value::Abool(a) => *a == other.into_abool(),
            Value::Functio(f) => *f == other.into_functio(),
            Value::Cart(c) => *c == other.into_cart(),
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        let other = other.clone();

        match self {
            Value::Nul => {
                if other == Value::Nul {
                    Some(Equal)
                } else {
                    None
                }
            }
            Value::Str(s) => Some(s.cmp(&other.to_string())),
            Value::Int(i) => Some(i.cmp(&other.into_i32())),
            Value::Bool(b) => Some(b.cmp(&other.into_bool())),
            Value::Abool(a) => a.partial_cmp(&other.into_abool()),
            Value::Functio(f) => {
                // Compares lengths of functions:
                // BfFunctio - Sum of lengths of instructions and length of tape
                // AbleFunctio - Sum of argument count and body length
                // Eval - Length of input code

                let selfl = match f {
                    Functio::BfFunctio {
                        instructions,
                        tape_len,
                    } => instructions.len() + tape_len,
                    Functio::AbleFunctio { params, body } => params.len() + body.len(),
                    Functio::Eval(s) => s.len(),
                };

                let otherl = match other.into_functio() {
                    Functio::BfFunctio {
                        instructions,
                        tape_len,
                    } => instructions.len() + tape_len,
                    Functio::AbleFunctio { params, body } => params.len() + body.len(),
                    Functio::Eval(s) => s.len(),
                };

                Some(selfl.cmp(&otherl))
            }
            Value::Cart(c) => Some(c.len().cmp(&other.into_cart().len())),
        }
    }
}

impl ops::BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl ops::BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        todo!()
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
                Functio::Eval(s) => write!(f, "{}", s),
            },
            Value::Cart(c) => {
                write!(f, "[")?;
                let mut cart_vec = c.into_iter().collect::<Vec<_>>();
                cart_vec.sort_by(|x, y| x.0.partial_cmp(&y.0).unwrap_or(std::cmp::Ordering::Less));

                for (key, value) in cart_vec {
                    write!(f, "{} <= {}, ", value.borrow(), key)?;
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
