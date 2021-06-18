use std::{io, ops::Range};

use crate::{brian::InterpretError, lexer::Token};

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug)]
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
    MismatchedArgumentError,
    MissingLhs,
    IOError(io::Error),
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

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self {
            kind: ErrorKind::IOError(e),
            span: 0..0,
        }
    }
}
