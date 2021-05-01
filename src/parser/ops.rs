use super::*;

type ExprResult = Result<Expr, Error>;

impl<'a> Parser<'a> {
    /// Parse operations (got identifier/value)
    pub(super) fn parse_ops(&mut self, token: Token) -> ExprResult {
        todo!()
    }

    /// Parse function call
    fn fn_call(&mut self, iden: Iden) -> ExprResult {
        self.lexer.next();
        let mut args: Vec<Expr> = Vec::new();

        while let Some(token) = self.lexer.peek() {
            match token {
                Token::Identifier(id) => {
                    args.push(Expr::Identifier(Iden(id.clone())));
                    self.lexer.next();
                }
                Token::RightParenthesis => break,
                _ => {
                    let next = self.lexer.next();
                }
            }
            self.require(Token::Comma)?;
        }
        self.require(Token::RightParenthesis)?;
        Ok(Expr::FunctionCall { iden, args })
    }
}
