use std::ops::Range;

use crate::{brian::InterpretError, lexer::Token};

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    SyntaxError(String),
    UnexpectedEof,
    UnexpectedToken(Token),
    InvalidIdentifier,
    UnknownVariable(String),
    MeloVariable(String),
    TypeError(String),
    TopLevelBreak,
    ArithmeticError,
    BfInterpretError(InterpretError),
    MissingLhs,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }

    /// Create an UnexpectedEof error, where the EOF occurs at the
    /// given index in the file.
    pub fn unexpected_eof(index: usize) -> Self {
        Self::new(ErrorKind::UnexpectedEof, index..index)
    }
}
