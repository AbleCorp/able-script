use super::*;

type ExprResult = Result<Expr, Error>;

/// Generate infix expression by pattern left <op> right
///
/// Credits: `@! ! Reiter#4543`
#[macro_export]
macro_rules! gen_infix {
    ($($fn_name: ident => $type: tt);*$(;)?) => {$(
        /// Generated function for infix operator
        fn $fn_name(&mut self, left: Expr, span: std::ops::Range<usize>) -> ExprResult {
            self.lexer.next();
            let next = self.lexer.next();
            let right = self.parse_expr(next)?;
            Ok(Expr::new(ExprKind::$type { left: Box::new(left), right: Box::new(right) }, span))
        })*
    };
}

impl<'a> Parser<'a> {
    pub(super) fn parse_ops(&mut self, token: SpannedToken) -> ParseResult {
        // Statements
        match self.lexer.peek() {
            Some((Token::LeftParenthesis, _)) => return self.fn_call(token),
            Some((Token::Assignment, _)) => return self.parse_assignment(token),
            _ => (),
        }

        let mut buf: Expr = self.parse_expr(Some(token))?;

        loop {
            let peek = self.lexer.peek().clone();
            buf = match peek {
                // Print statement
                Some((Token::Print, span)) => {
                    self.lexer.next();
                    self.require(Token::Semicolon)?;
                    return Ok(Stmt::new(StmtKind::Print(buf), buf.span.start..span.end).into());
                }
                None => return Ok(buf.into()),

                // An expression
                _ => self.parse_operation(peek.map(|x| *x), buf)?,
            }
        }
    }

    /// Match and perform
    pub(super) fn parse_operation(&mut self, token: Option<SpannedToken>, buf: Expr) -> ExprResult {
        match token {
            Some((Token::Addition, span)) => self.addition(buf, span),
            Some((Token::Subtract, span)) => self.subtract(buf, span),
            Some((Token::Multiply, span)) => self.multiply(buf, span),
            Some((Token::Divide, span)) => self.divide(buf, span),
            Some((Token::OpLt, span)) => self.cmplt(buf, span),
            Some((Token::OpGt, span)) => self.cmpgt(buf, span),
            Some((Token::OpEq, span)) => self.cmpeq(buf, span),
            Some((Token::OpNeq, span)) => self.cmpneq(buf, span),
            Some((Token::LogAnd, span)) => self.logand(buf, span),
            Some((Token::LogOr, span)) => self.logor(buf, span),
            Some((Token::LeftParenthesis, span)) | Some((_, span)) => {
                Err(Error::unexpected_token(span))
            }
            None => Err(Error::end_of_token_stream()),
        }
    }

    fn parse_assignment(&mut self, token: SpannedToken) -> ParseResult {
        let start = token.1.start;
        self.lexer.next(); // Eat

        // Extract identifier
        let iden = match token {
            (Token::Identifier(i), _span) => Iden(i),
            (_, span) => return Err(Error::invalid_identifier(span)),
        };

        let next = self.lexer.next();
        let mut value = self.parse_expr(next)?;

        loop {
            let peek = self.lexer.peek().clone();
            value = match peek {
                Some((Token::Semicolon, span)) => {
                    self.lexer.next();
                    break Ok(
                        Stmt::new(StmtKind::VarAssignment { iden, value }, start..span.end).into(),
                    );
                }
                None => break Err(Error::end_of_token_stream()),
                Some(&t) => self.parse_operation(Some(t), value)?,
            };
        }
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
    pub(super) fn parse_expr(&mut self, token: Option<SpannedToken>) -> ExprResult {
        let (token, span) = token.ok_or(Error::end_of_token_stream())?;

        match token {
            Token::Boolean(b) => Ok(Expr::new(ExprKind::Literal(Value::Bool(b)), span)),
            Token::Integer(i) => Ok(Expr::new(ExprKind::Literal(Value::Int(i)), span)),
            Token::String(s) => Ok(Expr::new(
                ExprKind::Literal(Value::Str(if self.tdark {
                    s.replace("lang", "script")
                } else {
                    s
                })),
                span,
            )),
            Token::Aboolean(a) => Ok(Expr::new(ExprKind::Literal(Value::Abool(a)), span)),
            Token::Identifier(i) => Ok(Expr::new(
                ExprKind::Identifier(Iden(if self.tdark {
                    i.replace("lang", "script")
                } else {
                    i
                })),
                span,
            )),
            Token::Nul => Ok(Expr::new(ExprKind::Literal(Value::Nul), span)),
            Token::LogNot => {
                let next = self.lexer.next();
                let expr = self.parse_expr(next)?;
                Ok(Expr::new(
                    ExprKind::Not(Box::new(expr)),
                    span.start..expr.span.end,
                ))
            }
            Token::LeftParenthesis => self.parse_paren(span.start),
            _ => Err(Error::unexpected_token(span)),
        }
    }

    /// Parse parenthesieted expression
    pub(super) fn parse_paren(&mut self, start: usize) -> ExprResult {
        let next = self.lexer.next();
        let mut buf = self.parse_expr(next)?;
        loop {
            let peek = self.lexer.peek().clone();
            buf = match peek {
                Some((Token::RightParenthesis, span)) => {
                    let next = self.lexer.next();
                    break Ok(Expr::new(buf.kind, start..span.end));
                }
                None => break Err(Error::end_of_token_stream()),
                Some(&t) => self.parse_operation(Some(t), buf)?,
            }
        }
    }

    /// Parse function call
    fn fn_call(&mut self, token: SpannedToken) -> ParseResult {
        let iden = match token {
            (Token::Identifier(i), _span) => Iden(i),
            (_, span) => return Err(Error::invalid_identifier(span)),
        };

        self.lexer.next();
        let mut args = Vec::new();
        loop {
            let next = self.lexer.next();

            // No argument function
            if let Some((Token::RightParenthesis, span)) = next {
                self.require(Token::Semicolon)?;
                break Ok(Stmt::new(StmtKind::FunctionCall { iden, args }, span).into());
            }

            args.push(self.parse_expr(next)?);
            match self.lexer.next() {
                Some((Token::RightParenthesis, span)) => {
                    self.require(Token::Semicolon)?;
                    break Ok(Stmt::new(StmtKind::FunctionCall { iden, args }, span).into());
                }
                Some((Token::Comma, _)) => continue,
                Some((_, span)) => break Err(Error::unexpected_token(span)),
                None => (),
            }
        }
    }
}
