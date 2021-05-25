use logos::Span;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    SyntaxError(String),
    EndOfTokenStream,
    InvalidIdentifier,
    UnexpectedToken,
}

impl Error {
    pub fn end_of_token_stream() -> Self {
        Self {
            kind: ErrorKind::EndOfTokenStream,
            span: 0..0, // TODO: FIX THIS
        }
    }

    pub fn invalid_identifier(span: Span) -> Self {
        Self {
            kind: ErrorKind::InvalidIdentifier,
            span,
        }
    }

    pub fn syntax_error(span: Span, message: String) -> Self {
        Self {
            kind: ErrorKind::SyntaxError(message),
            span,
        }
    }

    pub fn unexpected_token(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken,
            span,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match &self.kind {
            ErrorKind::SyntaxError(e) => format!("Syntax Error: {}", e.clone()),
            ErrorKind::EndOfTokenStream => "Unexpected end of source code".to_owned(),
            ErrorKind::InvalidIdentifier => "Invalid identifier".to_owned(),
            ErrorKind::UnexpectedToken => "Unexpected token".to_owned(),
        };

        write!(f, "{} on {:?}", msg, self.span)
    }
}
