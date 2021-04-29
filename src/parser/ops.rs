use super::*;

impl<'a> Parser<'a> {
    /// Parse operations (got identifier/value)
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

    /// Parse function call
    fn fn_call(&mut self, iden: Iden) -> Result<Expr, Error> {
        self.lexer.next();
        let mut args: Vec<Expr> = Vec::new();

        while let Some(token) = self.lexer.peek() {
            match token {
                Token::Identifier(id) => {
                    args.push(Iden(id.clone()).into());
                    self.lexer.next();
                }
                Token::RightParenthesis => break,
                _ => {
                    let next = self.lexer.next();
                    args.push(self.parse_expr(next)?)
                }
            }
            self.require(Token::Comma)?;
        }
        self.require(Token::RightParenthesis)?;
        Ok(Expr::FunctionCall { iden, args })
    }
}
