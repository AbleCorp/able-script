use crate::variables::Value;

#[derive(Debug, Clone)]
pub enum Expr {
    VariableDeclaration {
        iden: String,
        init: Option<Box<Expr>>,
    },
    FunctionDeclaration {
        iden: String,
        body: Vec<Expr>,
    },
    BfFDeclaration {
        iden: String,
        body: String,
    },
    Literal(Value),
}
