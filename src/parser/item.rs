#[derive(Debug, Clone)]
pub enum Expr {
    VariableDeclaration { iden: String, init: Option<String> },
    FunctionDeclaration { iden: String, body: Vec<Expr> },
    BfFDeclaration { iden: String, code: String },
}
