use super::*;

type ExprResult = Result<Expr, Error>;

#[macro_export]
macro_rules! gen_infix {
    ($($fn_name: ident => $type: tt);*$(;)?) => {$(
        fn $fn_name(&mut self, left: Expr) -> ExprResult {
            let next = self.lexer.next();
            let right = self.ensure_expr(next)?;
            Ok(Expr::$type { left: Box::new(left), right: Box::new(right) })
        })*
    };
}

impl<'a> Parser<'a> {
    pub(super) fn parse_ops(&mut self, token: Token) -> ParseResult {
        let mut buf: Expr = self.ensure_expr(Some(token))?;

        loop {
            buf = match self.lexer.next() {
                Some(Token::Addition) => self.addition(buf)?,
                Some(Token::Subtract) => self.subtract(buf)?,
                Some(Token::Multiply) => self.multiply(buf)?,
                Some(Token::Divide) => self.divide(buf)?,
                _ => return Ok(buf.into()),
            }
        }
    }

    // Generate infix
    gen_infix! {
        addition => Add;
        subtract => Subtract;
        multiply => Multiply;
        divide => Divide;
    }

    /// Ensure that input token is an expression
    fn ensure_expr(&mut self, token: Option<Token>) -> ExprResult {
        let token = token.ok_or(Error {
            kind: ErrorKind::EndOfTokenStream,
            position: self.lexer.span(),
        })?;
        match token {
            Token::Boolean(b) => Ok(Expr::Literal(Value::Bool(b))),
            Token::Integer(i) => Ok(Expr::Literal(Value::Int(i))),
            Token::String(s) => Ok(Expr::Literal(Value::Str(s))),
            Token::Aboolean(a) => Ok(Expr::Literal(Value::Abool(a))),
            Token::Identifier(i) => Ok(Expr::Identifier(Iden(i))),
            t => Err(self.unexpected_token(Some(t))),
        }
    }

    /// Parse function call
    fn fn_call(&mut self, iden: Iden) -> ExprResult {
        return todo!();
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
