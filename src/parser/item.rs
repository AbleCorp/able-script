use std::collections::HashMap;

use crate::variables::Value;

#[derive(Debug, Clone)]
pub struct Iden(pub String);

#[derive(Debug, Clone)]
pub enum Expr {
    VariableDeclaration {
        iden: String,
        init: Option<Box<Expr>>,
    },
    FunctionDeclaration {
        iden: String,
        args: Vec<Iden>,
        body: Vec<Expr>,
    },
    BfFDeclaration {
        iden: String,
        body: String,
    },
    If {
        cond: Box<Expr>,
        body: Vec<Expr>,
    },

    FunctionCall {
        iden: Iden,
        args: HashMap<Iden, Value>,
    },
    Literal(Value),
    Melo(Iden),
}
