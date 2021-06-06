use std::ops::Range;

use crate::brian::InterpretError;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    SyntaxError(String),
    EndOfTokenStream,
    InvalidIdentifier,
    UnknownVariable(String),
    MeloVariable(String),
    TypeError(String),
    TopLevelBreak,
    ArithmeticError,
    BfInterpretError(InterpretError),
}
