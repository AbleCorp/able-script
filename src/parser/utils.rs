use crate::error::{Error, ErrorKind};
use crate::tokens::Token;
use crate::variables::Abool;

use super::Parser;

pub fn abool2num(abool: Abool) -> i32 {
    match abool {
        Abool::Never => -1,
        Abool::Sometimes => 0,
        Abool::Always => 1,
    }
}
pub fn num2abool(number: i32) -> Abool {
    match number {
        -1 => Abool::Never,
        0 => Abool::Sometimes,
        1 => Abool::Always,
        _ => Abool::Sometimes,
    }
}

impl<'a> Parser<'a> {
    /// Require type of token as next and return it's value (sometimes irrelevant)
    pub(super) fn require(&mut self, with: Token) -> Result<String, Error> {
        if self.lexer.next() == Some(with) {
            Ok(self.lexer.slice().to_owned())
        } else {
            Err(Error {
                kind: ErrorKind::SyntaxError("Mysterious parse error".to_owned()),
                position: self.lexer.span(),
            })
        }
    }
}
