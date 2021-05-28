use logos::Span;

use crate::variables::Value;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Iden(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedIden {
    pub iden: Iden,
    pub span: Span,
}

impl SpannedIden {
    pub fn new(iden: Iden, span: Span) -> Self {
        Self { iden, span }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Stmt(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<Expr> for Item {
    fn from(e: Expr) -> Self {
        Item::Expr(e)
    }
}

impl From<Stmt> for Item {
    fn from(s: Stmt) -> Self {
        Item::Stmt(s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Add { left: Box<Expr>, right: Box<Expr> },
    Subtract { left: Box<Expr>, right: Box<Expr> },
    Multiply { left: Box<Expr>, right: Box<Expr> },
    Divide { left: Box<Expr>, right: Box<Expr> },
    Lt { left: Box<Expr>, right: Box<Expr> },
    Gt { left: Box<Expr>, right: Box<Expr> },
    Eq { left: Box<Expr>, right: Box<Expr> },
    Neq { left: Box<Expr>, right: Box<Expr> },
    And { left: Box<Expr>, right: Box<Expr> },
    Or { left: Box<Expr>, right: Box<Expr> },
    Not(Box<Expr>),
    Literal(Value),
    Identifier(Iden),
}

impl From<Iden> for ExprKind {
    fn from(i: Iden) -> Self {
        Self::Identifier(i)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    VariableDeclaration {
        iden: SpannedIden,
        init: Option<Expr>,
    },
    FunctionDeclaration {
        iden: SpannedIden,
        args: Vec<Iden>,
        body: Vec<Item>,
    },
    BfFDeclaration {
        iden: SpannedIden,
        body: String,
    },
    If {
        cond: Expr,
        body: Vec<Item>,
    },
    FunctionCall {
        iden: SpannedIden,
        args: Vec<Expr>,
    },
    Loop {
        body: Vec<Item>,
    },

    VarAssignment {
        iden: SpannedIden,
        value: Expr,
    },
    Break,
    HopBack,
    Print(Expr),
    Melo(SpannedIden),
}
