use crate::variables::Value;

type Span = std::ops::Range<usize>;

#[derive(Debug)]
pub struct Iden(String);

#[derive(Debug)]
pub struct Block {
    pub block: Vec<Stmt>,
}

/// A syntactic unit expressing an effect.
#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
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
        iden: Iden,
        init: Option<Expr>,
    },

    Functio {
        iden: Iden,
        args: Vec<Iden>,
        body: Block,
    },
    Call {
        iden: Iden,
        args: Vec<Expr>,
    },
    Print(Expr),
    Melo(Iden),
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Expression is parse unit which do not cause any effect,
/// like math and logical operations or values.
#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: BinOpKind,
    },
    Not(Box<Expr>),
    Literal(Value),
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug)]
pub enum BinOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Greater,
    Less,
    Equal,
    NotEqual,
    And,
    Or,
}
