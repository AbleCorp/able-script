use std::collections::HashMap;

use crate::variables::Value;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Iden(pub String);

impl From<Iden> for Expr {
    fn from(iden: Iden) -> Self {
        Self::Identifier(iden)
    }
}

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
        args: Vec<Expr>,
    },
    Literal(Value),
    Identifier(Iden),
    Melo(Iden),
}
