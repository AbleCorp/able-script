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
        match self.lexer.peek() {
            Some(Token::LeftParenthesis) => return self.fn_call(token),
            Some(Token::Assignment) => return self.parse_assignment(token),
            _ => (),
        }

        let mut buf: Expr = self.parse_expr(Some(token))?;

        loop {
            let peek = self.lexer.peek().clone();
            buf = match peek {
                Some(Token::Print) => {
                    self.lexer.next();
                    self.require(Token::Semicolon)?;
                    return Ok(Stmt::Print(buf).into());
                }
                None => return Ok(buf.into()),
                _ => self.parse_operation(peek, buf)?,
            }
        }
    }

    /// Match and perform
    pub(super) fn parse_operation(&mut self, token: Option<Token>, buf: Expr) -> ExprResult {
        match token {
            Some(Token::Addition) => self.addition(buf),
            Some(Token::Subtract) => self.subtract(buf),
            Some(Token::Multiply) => self.multiply(buf),
            Some(Token::Divide) => self.divide(buf),
            Some(Token::OpLt) => self.cmplt(buf),
            Some(Token::OpGt) => self.cmpgt(buf),
            Some(Token::OpEq) => self.cmpeq(buf),
            Some(Token::OpNeq) => self.cmpneq(buf),
            Some(Token::LogAnd) => self.logand(buf),
            Some(Token::LogOr) => self.logor(buf),
            Some(Token::LeftParenthesis) => Err(Error {
                kind: ErrorKind::SyntaxError("Function call isn't an expression!".to_owned()),
                position: self.lexer.span(),
            }),
            Some(_) | None => Err(self.unexpected_token(None)),
        }
    }

    fn parse_assignment(&mut self, token: Token) -> ParseResult {
        self.lexer.next();
        let iden = if let Token::Identifier(i) = token {
            Iden(i)
        } else {
            return Err(Error {
                kind: ErrorKind::InvalidIdentifier,
                position: self.lexer.span(),
            });
        };

        let next = self.lexer.next();
        let mut value = self.parse_expr(next)?;
        loop {
            let peek = self.lexer.peek().clone();
            value = match peek {
                Some(Token::Semicolon) => break,
                None => {
                    return Err(Error {
                        kind: ErrorKind::EndOfTokenStream,
                        position: self.lexer.span(),
                    })
                }
                Some(t) => self.parse_operation(Some(t), value)?,
            };
        }
        self.lexer.next();

        Ok(Stmt::VarAssignment { iden, value }.into())
    }
    // Generate infix
    gen_infix! {
        addition => Add;
        subtract => Subtract;
        multiply => Multiply;
        divide => Divide;
        cmplt => Lt;
        cmpgt => Gt;
        cmpeq => Eq;
        cmpneq => Neq;
        logand => And;
        logor => Or;
    }

    /// Ensure that input token is an expression
    pub(super) fn parse_expr(&mut self, token: Option<Token>) -> ExprResult {
        let token = token.ok_or(Error {
            kind: ErrorKind::EndOfTokenStream,
            position: self.lexer.span(),
        })?;

        match token {
            Token::Boolean(b) => Ok(Expr::Literal(Value::Bool(b))),
            Token::Integer(i) => Ok(Expr::Literal(Value::Int(i))),
            Token::String(s) => Ok(Expr::Literal(Value::Str(if self.tdark {
                s.replace("lang", "script")
            } else {
                s
            }))),
            Token::Aboolean(a) => Ok(Expr::Literal(Value::Abool(a))),
            Token::Identifier(i) => Ok(Expr::Identifier(Iden(if self.tdark {
                i.replace("lang", "script")
            } else {
                i
            }))),
            Token::Nul => Ok(Expr::Literal(Value::Nul)),
            Token::LogNot => {
                let next = self.lexer.next();
                Ok(Expr::Not(Box::new(self.parse_expr(next)?)))
            }
            Token::LeftParenthesis => self.parse_paren(),
            t => Err(self.unexpected_token(Some(t))),
        }
    }

    /// Parse parenthesieted expression
    pub(super) fn parse_paren(&mut self) -> ExprResult {
        let next = self.lexer.next();
        let mut buf = self.parse_expr(next)?;
        loop {
            let next = self.lexer.peek().clone().ok_or(Error {
                kind: ErrorKind::EndOfTokenStream,
                position: self.lexer.span(),
            })?;

            buf = match Some(next) {
                Some(Token::RightParenthesis) => {
                    self.lexer.next();
                    return Ok(buf);
                }
                None => return Ok(buf),
                Some(t) => self.parse_operation(Some(t), buf)?,
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
