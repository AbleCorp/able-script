use crate::error::{Error, ErrorKind};
use crate::lexer::Token;
use crate::variables::Abool;

use super::{
    item::{Iden, Item},
    Parser,
};

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
        if self.lexer.next() == Some(with.clone()) {
            Ok(self.lexer.slice().to_owned())
        } else {
            Err(self.unexpected_token(Some(with)))
        }
    }

    /// Require an identifier on next and return it
    pub(super) fn require_iden(&mut self) -> Result<Iden, Error> {
        if let Some(Token::Identifier(id)) = self.lexer.next() {
            if self.tdark {
                Ok(Iden(id.replace("lang", "script")))
            } else {
                Ok(Iden(id))
            }
        } else {
            Err(Error {
                kind: ErrorKind::InvalidIdentifier,
                position: self.lexer.span(),
            })
        }
    }

    /// Throw unexpected token error (optionally what was expected)
    pub(super) fn unexpected_token(&mut self, expected: Option<Token>) -> Error {
        let error_msg = match expected {
            Some(s) => format!(
                "Unexpected token: `{}` (required: `{:?}`)",
                self.lexer.slice(),
                s
            ),
            None => format!("Unexpected token: `{}`", self.lexer.slice(),),
        };
        Error {
            kind: ErrorKind::SyntaxError(error_msg),
            position: self.lexer.span(),
        }
    }

    pub(super) fn parse_body(&mut self) -> Result<Vec<Item>, Error> {
        let mut body = Vec::new();
        loop {
            let token = {
                match self.lexer.next() {
                    Some(t) => t,
                    None => {
                        return Err(Error {
                            kind: ErrorKind::EndOfTokenStream,
                            position: self.lexer.span(),
                        })
                    }
                }
            };

            if token == Token::RightBrace {
                break;
            }
            body.push(self.parse_item(Some(token))?);
        }
        Ok(body)
    }
}
