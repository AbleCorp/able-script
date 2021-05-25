use logos::Span;

use crate::{error::Error, lexer::SpannedToken, lexer::Token, variables::Abool};

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
    pub(super) fn require(&mut self, with: Token) -> Result<SpannedToken, Error> {
        let next = self.lexer.next();
        match next {
            Some(st) if st.0 == with => Ok(st),
            Some((_, span)) => Err(Error::unexpected_token(span)),
            None => Err(Error::end_of_token_stream()),
        }
    }

    /// Require an identifier on next and return it
    pub(super) fn require_iden(&mut self) -> Result<(Iden, Span), Error> {
        let next = self.lexer.next();
        match next {
            Some((Token::Identifier(i), span)) => Ok((
                Iden(if self.tdark {
                    i.replace("lang", "script")
                } else {
                    i
                }),
                span,
            )),
            Some((_, span)) => Err(Error::invalid_identifier(span)),
            None => Err(Error::end_of_token_stream()),
        }
    }

    pub(super) fn parse_body(&mut self) -> Result<(Vec<Item>, usize), Error> {
        let mut body = Vec::new();
        loop {
            let token = {
                match self.lexer.peek().cloned() {
                    Some(t) => t,
                    None => unimplemented!(),
                }
            };

            if let (Token::RightBrace, span) = token {
                self.lexer.next();
                break Ok((body, span.end));
            }

            body.push(self.parse_item(Some(token))?);
        }
    }
}
