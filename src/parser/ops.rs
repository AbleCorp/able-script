use std::collections::HashMap;

use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_ops(&mut self, token: Token) -> Result<Expr, Error> {
        let iden = if let Token::Identifier(i) = token {
            Iden(i)
        } else {
            unimplemented!()
        };

        let mut buf = Vec::new();

        buf.push(match self.lexer.peek() {
            Some(Token::LeftParenthesis) => self.fn_call(iden)?,
            _ => unimplemented!(),
        });

        Ok(buf[0].clone())
    }

    fn fn_call(&mut self, iden: Iden) -> Result<Expr, Error> {
        self.lexer.next();
        self.require(Token::RightParenthesis)?;
        Ok(Expr::FunctionCall {
            iden,
            args: HashMap::new(),
        })
    }
}
