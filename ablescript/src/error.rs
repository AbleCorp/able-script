use std::{fmt::Display, io, ops::Range};

use crate::{brian::InterpretError, lexer::Token};

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken(Token),
    UnknownVariable(String),
    MeloVariable(String),
    TopLevelBreak,
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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error at range {}-{}: {}",
            self.span.start, self.span.end, self.kind
        )
    }
}
impl std::error::Error for Error {}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedEof => write!(f, "unexpected end of file"),
            ErrorKind::UnexpectedToken(token) => write!(f, "unexpected token {:?}", token),
            ErrorKind::UnknownVariable(name) => write!(f, "unknown identifier \"{}\"", name),
            ErrorKind::MeloVariable(name) => write!(f, "banned variable \"{}\"", name),
            ErrorKind::TopLevelBreak => write!(f, "can only `break` out of a loop"),
            ErrorKind::BfInterpretError(err) => write!(f, "brainfuck error: {}", err),
            // TODO: give concrete numbers here.
            ErrorKind::MismatchedArgumentError => write!(f, "wrong number of function arguments"),
            ErrorKind::MissingLhs => write!(f, "missing expression before binary operation"),
            ErrorKind::IOError(err) => write!(f, "I/O error: {}", err),
        }
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
