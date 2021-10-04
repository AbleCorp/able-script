//! AbleScript's Abstract Syntax tree
//!
//! Statements are the type which is AST made of, as they
//! express an effect.
//!
//! Expressions are just operations and they cannot be
//! used as statements. Functions in AbleScript are in fact
//! just plain subroutines and they do not return any value,
//! so their calls are statements.

use std::hash::Hash;

use crate::variables::Value;

type Span = std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub struct Ident {
    pub ident: String,
    pub span: Span,
}

impl Ident {
    pub fn new(ident: String, span: Span) -> Self {
        Self { ident, span }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub struct Block {
    pub block: Vec<Stmt>,
}

/// A syntactic unit expressing an effect.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Hash for Stmt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum StmtKind {
    // Control flow
    If {
        cond: Expr,
        body: Block,
    },
    Loop {
        body: Block,
    },
    Break,
    HopBack,

    Var {
        ident: Ident,
        init: Option<Expr>,
    },
    Assign {
        ident: Ident,
        value: Expr,
    },

    Functio {
        ident: Ident,
        params: Vec<Ident>,
        body: Block,
    },
    BfFunctio {
        ident: Ident,
        tape_len: Option<Expr>,
        code: Vec<u8>,
    },
    Call {
        expr: Expr,
        args: Vec<Expr>,
    },
    Print(Expr),
    Read(Ident),
    Melo(Ident),
    Rlyeh,
    Rickroll,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Expression is parse unit which do not cause any effect,
/// like math and logical operations or values.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum ExprKind {
    BinOp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: BinOpKind,
    },
    Not(Box<Expr>),
    Literal(Value),
    Cart(Vec<(Expr, Expr)>),
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Variable(String),
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum BinOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Greater,
    Less,
    Equal,
    NotEqual,
}

impl BinOpKind {
    pub fn from_token(t: crate::lexer::Token) -> Result<Self, crate::error::ErrorKind> {
        use crate::lexer::Token;

        match t {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Subtract),
            Token::Star => Ok(Self::Multiply),
            Token::FwdSlash => Ok(Self::Divide),
            Token::GreaterThan => Ok(Self::Greater),
            Token::LessThan => Ok(Self::Less),
            Token::EqualEqual => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            t => Err(crate::error::ErrorKind::UnexpectedToken(t)),
        }
    }
}
