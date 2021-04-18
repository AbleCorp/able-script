#[derive(Debug, Clone)]
pub enum Expr {
    DeclareVariable { iden: String, init: Option<String> },
}