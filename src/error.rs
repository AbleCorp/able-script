use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub position: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    SyntaxError(String),
    EndOfTokenStream,
    InvalidIdentifier,
    UnknownVariable(String),
    MeloVariable(String),
    TypeError(String),
}
