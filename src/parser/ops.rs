use super::*;

type ExprResult = Result<Expr, Error>;

/// Generate infix expression by pattern left <op> right
///
/// Credits: `@! ! Reiter#4543`
#[macro_export]
macro_rules! gen_infix {
    ($($fn_name: ident => $type: tt);*$(;)?) => {$(
        /// Generated function for infix operator
        fn $fn_name(&mut self, left: Expr) -> ExprResult {
            self.lexer.next();
            let next = self.lexer.next();
            let right = self.parse_expr(next)?;
            Ok(Expr::$type { left: Box::new(left), right: Box::new(right) })
        })*
    };
}

impl<'a> Parser<'a> {
    pub(super) fn parse_ops(&mut self, token: Token) -> ParseResult {
        if matches!(self.lexer.peek(), Some(Token::LeftParenthesis)) {
            return self.fn_call(token);
        }

        let mut buf: Expr = self.parse_expr(Some(token))?;

        loop {
            buf = match self.lexer.peek() {
                Some(Token::Addition) => self.addition(buf)?,
                Some(Token::Subtract) => self.subtract(buf)?,
                Some(Token::Multiply) => self.multiply(buf)?,
                Some(Token::Divide) => self.divide(buf)?,
                Some(Token::Print) => {
                    self.lexer.next();
                    self.require(Token::Semicolon)?;
                    return Ok(Stmt::Print(buf).into());
                }
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
    fn parse_expr(&mut self, token: Option<Token>) -> ExprResult {
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
            Token::LeftParenthesis => self.parse_paren(),
            t => Err(self.unexpected_token(Some(t))),
        }
    }

    /// Parse parenthesieted expression
    fn parse_paren(&mut self) -> ExprResult {
        let next = self.lexer.next();
        let mut buf = self.parse_expr(next)?;
        loop {
            let next = self.lexer.peek().clone().ok_or(Error {
                kind: ErrorKind::EndOfTokenStream,
                position: self.lexer.span(),
            })?;

            buf = match Some(next) {
                Some(Token::Addition) => self.addition(buf)?,
                Some(Token::Subtract) => self.subtract(buf)?,
                Some(Token::Multiply) => self.multiply(buf)?,
                Some(Token::Divide) => self.divide(buf)?,
                Some(Token::LeftParenthesis) => {
                    return Err(Error {
                        kind: ErrorKind::SyntaxError(
                            "Function call isn't an expression!".to_owned(),
                        ),
                        position: self.lexer.span(),
                    })
                }
                Some(Token::RightParenthesis) => {
                    self.lexer.next();
                    return Ok(buf);
                }
                _ => return Ok(buf.into()),
            };
        }
    }

    /// Parse function call
    fn fn_call(&mut self, token: Token) -> ParseResult {
        let iden = if let Token::Identifier(i) = token {
            Iden(i)
        } else {
            return Err(Error {
                kind: ErrorKind::InvalidIdentifier,
                position: self.lexer.span(),
            });
        };

        self.lexer.next();
        let mut args = Vec::new();
        loop {
            let next = self.lexer.next();

            // No argument function
            if matches!(next, Some(Token::RightParenthesis)) {
                break;
            }

            args.push(self.parse_expr(next)?);
            match self.lexer.next() {
                Some(Token::RightParenthesis) => break,
                Some(Token::Comma) => continue,
                _ => return Err(self.unexpected_token(None)),
            }
        }
        self.require(Token::Semicolon)?;
        Ok(Stmt::FunctionCall { iden, args }.into())
    }
}
