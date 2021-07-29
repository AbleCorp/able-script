//! AbleScript Parser
//!
//! Type of this parser is recursive descent

use logos::{Lexer, Logos};

use crate::ast::*;
use crate::error::{Error, ErrorKind};
use crate::lexer::Token;
use crate::variables::Value;

/// Parser structure which holds lexer and metadata
///
/// Make one using [`Parser::new`] function
pub struct Parser<'source> {
    lexer: Lexer<'source, Token>,
    tdark: bool,
}

impl<'source> Parser<'source> {
    /// Create a new parser from source code
    pub fn new(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source),
            tdark: false,
        }
    }

    /// Start parsing tokens
    ///
    /// Loops trough lexer, parses statements, returns AST
    pub fn init(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut ast = vec![];
        while let Some(token) = self.lexer.next() {
            match token {
                // Ignore comments
                Token::Comment => continue,

                // T-Dark block (replace `lang` with `script`)
                Token::TDark => {
                    self.tdark = true;
                    let mut block = self.get_block()?;
                    ast.append(&mut block.block);
                    self.tdark = false;
                }
                token => ast.push(self.parse(token)?),
            }
        }
        Ok(ast)
    }

    /// Get next item
    ///
    /// If EOF, return Error instead of None
    fn checked_next(&mut self) -> Result<Token, Error> {
        self.lexer
            .next()
            .ok_or_else(|| Error::unexpected_eof(self.lexer.span().start))
    }

    /// Parse a token
    ///
    /// This function will route to corresponding flow functions
    /// which may advance the lexer iterator
    fn parse(&mut self, token: Token) -> Result<Stmt, Error> {
        let start = self.lexer.span().start;

        match token {
            Token::If => Ok(Stmt::new(self.if_flow()?, start..self.lexer.span().end)),
            Token::Functio => Ok(Stmt::new(
                self.functio_flow()?,
                start..self.lexer.span().end,
            )),
            Token::Bff => Ok(Stmt::new(self.bff_flow()?, start..self.lexer.span().end)),
            Token::Var => Ok(Stmt::new(self.var_flow()?, start..self.lexer.span().end)),
            Token::Melo => Ok(Stmt::new(self.melo_flow()?, start..self.lexer.span().end)),
            Token::Loop => Ok(Stmt::new(self.loop_flow()?, start..self.lexer.span().end)),
            Token::Break => Ok(Stmt::new(
                self.semi_terminated(StmtKind::Break)?,
                start..self.lexer.span().end,
            )),
            Token::HopBack => Ok(Stmt::new(
                self.semi_terminated(StmtKind::HopBack)?,
                start..self.lexer.span().end,
            )),
            Token::Rlyeh => Ok(Stmt::new(
                self.semi_terminated(StmtKind::Rlyeh)?,
                start..self.lexer.span().end,
            )),
            Token::Rickroll => Ok(Stmt::new(
                self.semi_terminated(StmtKind::Rickroll)?,
                start..self.lexer.span().end,
            )),

            Token::Identifier(_)
            | Token::Char
            | Token::String(_)
            | Token::Integer(_)
            | Token::Abool(_)
            | Token::Bool(_)
            | Token::LeftBracket
            | Token::LeftParen => Ok(Stmt::new(
                self.value_flow(token)?,
                start..self.lexer.span().end,
            )),

            t => Err(Error {
                kind: ErrorKind::UnexpectedToken(t),
                span: start..self.lexer.span().end,
            }),
        }
    }

    /// Require statement to be semicolon terminated
    ///
    /// Utility function for short statements
    fn semi_terminated(&mut self, stmt_kind: StmtKind) -> Result<StmtKind, Error> {
        self.require(Token::Semicolon)?;
        Ok(stmt_kind)
    }

    /// Require next item to be equal with expected one
    fn require(&mut self, expected: Token) -> Result<(), Error> {
        match self.lexer.next() {
            Some(t) if t == expected => Ok(()),
            Some(t) => Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
            None => Err(Error::unexpected_eof(self.lexer.span().start)),
        }
    }

    /// Get an Identifier
    fn get_iden(&mut self) -> Result<Iden, Error> {
        match self.checked_next()? {
            Token::Identifier(iden) => Ok(Iden {
                iden: if self.tdark {
                    iden.replace("lang", "script")
                } else {
                    iden
                },
                span: self.lexer.span(),
            }),
            t => Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
        }
    }

    /// Parse an expression
    ///
    /// AbleScript strongly separates expressions from statements.
    /// Expressions do not have any side effects and the are
    /// only mathematial and logical operations or values.
    fn parse_expr(&mut self, token: Token, buf: &mut Option<Expr>) -> Result<Expr, Error> {
        let start = match buf {
            Some(e) => e.span.start,
            None => self.lexer.span().start,
        };

        match token {
            // Values
            Token::Identifier(i) => Ok(Expr::new(
                ExprKind::Variable(if self.tdark {
                    i.replace("lang", "script")
                } else {
                    i
                }),
                start..self.lexer.span().end,
            )),
            Token::Abool(a) => Ok(Expr::new(
                ExprKind::Literal(Value::Abool(a)),
                start..self.lexer.span().end,
            )),
            Token::Bool(b) => Ok(Expr::new(
                ExprKind::Literal(Value::Bool(b)),
                start..self.lexer.span().end,
            )),
            Token::Integer(i) => Ok(Expr::new(
                ExprKind::Literal(Value::Int(i)),
                start..self.lexer.span().end,
            )),
            Token::String(s) => Ok(Expr::new(
                ExprKind::Literal(Value::Str(if self.tdark {
                    s.replace("lang", "script")
                } else {
                    s
                })),
                start..self.lexer.span().end,
            )),
            Token::Nul => Ok(Expr::new(
                ExprKind::Literal(Value::Nul),
                start..self.lexer.span().end,
            )),

            Token::LeftBracket => match buf.take() {
                Some(buf) => Ok(Expr::new(
                    ExprKind::Index {
                        cart: Box::new(buf),
                        index: Box::new(self.expr_flow(Token::RightBracket)?),
                    },
                    start..self.lexer.span().end,
                )),
                None => Ok(Expr::new(self.cart_flow()?, start..self.lexer.span().end)),
            },

            // Operations
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::FwdSlash
            | Token::EqualEqual
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan
            | Token::And
            | Token::Or => Ok(Expr::new(
                self.binop_flow(
                    BinOpKind::from_token(token).map_err(|e| Error::new(e, self.lexer.span()))?,
                    buf,
                )?,
                start..self.lexer.span().end,
            )),

            Token::Not => Ok(Expr::new(
                {
                    let next = self.checked_next()?;
                    ExprKind::Not(Box::new(self.parse_expr(next, buf)?))
                },
                start..self.lexer.span().end,
            )),

            Token::LeftParen => self.expr_flow(Token::RightParen),
            t => Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
        }
    }

    /// Flow for creating carts
    fn cart_flow(&mut self) -> Result<ExprKind, Error> {
        let mut cart = vec![];
        let mut buf = None;

        match self.checked_next()? {
            Token::RightBracket => return Ok(ExprKind::Cart(cart)),
            t => buf = Some(self.parse_expr(t, &mut buf)?),
        }

        'cart: loop {
            let value = loop {
                match self.checked_next()? {
                    Token::Arrow => break buf.take(),
                    t => buf = Some(self.parse_expr(t, &mut buf)?),
                }
            }
            .ok_or_else(|| {
                Error::new(ErrorKind::UnexpectedToken(Token::Arrow), self.lexer.span())
            })?;

            let key = loop {
                match self.checked_next()? {
                    Token::RightBracket => {
                        cart.push((
                            value,
                            buf.take()
                                .ok_or_else(|| Error::unexpected_eof(self.lexer.span().start))?,
                        ));

                        break 'cart;
                    }
                    Token::Comma => break buf.take(),
                    t => buf = Some(self.parse_expr(t, &mut buf)?),
                }
            }
            .ok_or_else(|| Error::unexpected_eof(self.lexer.span().start))?;

            cart.push((value, key));
        }

        Ok(ExprKind::Cart(cart))
    }

    /// Flow for operators
    ///
    /// Generates operation from LHS buffer and next expression as RHS
    ///
    /// This is unaware of precedence, as AbleScript do not have it
    fn binop_flow(&mut self, kind: BinOpKind, lhs: &mut Option<Expr>) -> Result<ExprKind, Error> {
        Ok(ExprKind::BinOp {
            lhs: Box::new(
                lhs.take()
                    .ok_or_else(|| Error::new(ErrorKind::MissingLhs, self.lexer.span()))?,
            ),
            rhs: {
                let next = self
                    .lexer
                    .next()
                    .ok_or_else(|| Error::unexpected_eof(self.lexer.span().start))?;
                Box::new(self.parse_expr(next, &mut None)?)
            },
            kind,
        })
    }

    /// Parse expressions until terminate token
    fn expr_flow(&mut self, terminate: Token) -> Result<Expr, Error> {
        let mut buf = None;
        Ok(loop {
            match self.checked_next()? {
                t if t == terminate => {
                    break buf.take().ok_or_else(|| {
                        Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())
                    })?
                }
                t => buf = Some(self.parse_expr(t, &mut buf)?),
            }
        })
    }

    /// Parse a list of statements between curly braces
    fn get_block(&mut self) -> Result<Block, Error> {
        self.require(Token::LeftCurly)?;
        let mut block = vec![];

        loop {
            match self.checked_next()? {
                Token::RightCurly => break,
                t => block.push(self.parse(t)?),
            }
        }
        Ok(Block { block })
    }

    /// If Statement parser gets any kind of value (Identifier or Literal)
    /// It cannot parse it as it do not parse expressions. Instead of it it
    /// will parse it to function call or print statement.
    fn value_flow(&mut self, init: Token) -> Result<StmtKind, Error> {
        let mut buf = Some(self.parse_expr(init, &mut None)?);
        let r = loop {
            match self.checked_next()? {
                // Print to stdout
                Token::Print => {
                    let stmt = StmtKind::Print(buf.take().ok_or_else(|| {
                        Error::new(ErrorKind::UnexpectedToken(Token::Print), self.lexer.span())
                    })?);
                    break self.semi_terminated(stmt)?;
                }

                // Functio call
                Token::LeftParen => {
                    if let Some(Expr {
                        kind: ExprKind::Variable(iden),
                        span,
                    }) = buf
                    {
                        break self.functio_call_flow(Iden::new(iden, span))?;
                    }
                }

                // Variable Assignment
                Token::Equal => {
                    if let Some(Expr {
                        kind: ExprKind::Variable(iden),
                        span,
                    }) = buf
                    {
                        break StmtKind::Assign {
                            iden: Iden::new(iden, span),
                            value: self.expr_flow(Token::Semicolon)?,
                        };
                    }
                }

                // Read input
                Token::Read => {
                    if let Some(Expr {
                        kind: ExprKind::Variable(iden),
                        span,
                    }) = buf
                    {
                        break self.semi_terminated(StmtKind::Read(Iden::new(iden, span)))?;
                    }
                }

                t => buf = Some(self.parse_expr(t, &mut buf)?),
            }
        };

        Ok(r)
    }

    /// Parse If flow
    ///
    /// Consists of condition and block, there is no else
    fn if_flow(&mut self) -> Result<StmtKind, Error> {
        self.require(Token::LeftParen)?;

        let cond = self.expr_flow(Token::RightParen)?;

        let body = self.get_block()?;

        Ok(StmtKind::If { cond, body })
    }

    /// Parse functio flow
    ///
    /// functio $iden (a, b, c) { ... }
    fn functio_flow(&mut self) -> Result<StmtKind, Error> {
        let iden = self.get_iden()?;

        self.require(Token::LeftParen)?;

        let mut params = vec![];
        loop {
            match self.checked_next()? {
                Token::RightParen => break,
                Token::Identifier(i) => {
                    params.push(Iden::new(i, self.lexer.span()));

                    // Require comma (next) or right paren (end) after identifier
                    match self.checked_next()? {
                        Token::Comma => continue,
                        Token::RightParen => break,
                        t => {
                            return Err(Error::new(
                                ErrorKind::UnexpectedToken(t),
                                self.lexer.span(),
                            ))
                        }
                    }
                }
                t => return Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
            }
        }

        let body = self.get_block()?;

        Ok(StmtKind::Functio { iden, params, body })
    }

    /// Parse BF function declaration
    ///
    /// `bff $iden ([tapelen]) { ... }`
    fn bff_flow(&mut self) -> Result<StmtKind, Error> {
        let iden = self.get_iden()?;

        let tape_len = match self.checked_next()? {
            Token::LeftParen => {
                let len = Some(self.expr_flow(Token::RightParen)?);
                self.require(Token::LeftCurly)?;
                len
            }
            Token::LeftCurly => None,
            token => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken(token),
                    self.lexer.span(),
                ))
            }
        };

        let mut code: Vec<u8> = vec![];
        loop {
            match self.checked_next()? {
                Token::Plus
                | Token::Minus
                | Token::Dot
                | Token::Comma
                | Token::LeftBracket
                | Token::RightBracket
                | Token::LessThan
                | Token::GreaterThan => code.push(self.lexer.slice().as_bytes()[0]),
                Token::RightCurly => break,
                _ => (),
            }
        }

        Ok(StmtKind::BfFunctio {
            iden,
            tape_len,
            code,
        })
    }

    /// Parse functio call flow
    fn functio_call_flow(&mut self, iden: Iden) -> Result<StmtKind, Error> {
        let mut args = vec![];
        let mut buf = None;
        loop {
            match self.checked_next()? {
                // End of argument list
                Token::RightParen => {
                    if let Some(expr) = buf.take() {
                        args.push(expr)
                    }
                    break;
                }

                // Next argument
                Token::Comma => match buf.take() {
                    Some(expr) => args.push(expr),
                    // Comma alone
                    None => {
                        return Err(Error::new(
                            ErrorKind::UnexpectedToken(Token::Comma),
                            self.lexer.span(),
                        ))
                    }
                },
                t => buf = Some(self.parse_expr(t, &mut buf)?),
            }
        }

        self.require(Token::Semicolon)?;
        Ok(StmtKind::Call { iden, args })
    }

    /// Parse variable declaration
    fn var_flow(&mut self) -> Result<StmtKind, Error> {
        let iden = self.get_iden()?;
        let init = match self.checked_next()? {
            Token::Equal => Some(self.expr_flow(Token::Semicolon)?),
            Token::Semicolon => None,
            t => return Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
        };

        Ok(StmtKind::Var { iden, init })
    }

    /// Parse Melo flow
    fn melo_flow(&mut self) -> Result<StmtKind, Error> {
        let iden = self.get_iden()?;
        self.semi_terminated(StmtKind::Melo(iden))
    }

    /// Parse loop flow
    ///
    /// `loop` is an infinite loop, no condition, only body
    fn loop_flow(&mut self) -> Result<StmtKind, Error> {
        Ok(StmtKind::Loop {
            body: self.get_block()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_math() {
        let code = r#"1 * (a + 3) / 666 print;"#;
        let expected = &[Stmt {
            kind: StmtKind::Print(Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::BinOp {
                            lhs: Box::new(Expr {
                                kind: ExprKind::Literal(Value::Int(1)),
                                span: 0..1,
                            }),
                            rhs: Box::new(Expr {
                                kind: ExprKind::BinOp {
                                    lhs: Box::new(Expr {
                                        kind: ExprKind::Variable("a".to_string()),
                                        span: 5..6,
                                    }),
                                    rhs: Box::new(Expr {
                                        kind: ExprKind::Literal(Value::Int(3)),
                                        span: 9..10,
                                    }),
                                    kind: BinOpKind::Add,
                                },
                                span: 5..10,
                            }),
                            kind: BinOpKind::Multiply,
                        },
                        span: 0..11,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(666)),
                        span: 14..17,
                    }),
                    kind: BinOpKind::Divide,
                },
                span: 0..17,
            }),
            span: 0..24,
        }];

        let ast = Parser::new(code).init().unwrap();
        assert_eq!(ast, expected);
    }

    #[test]
    fn variable_declaration() {
        let code = r#"var a = 42;"#;
        let expected = &[Stmt {
            kind: StmtKind::Var {
                iden: Iden {
                    iden: "a".to_string(),
                    span: 4..5,
                },
                init: Some(Expr {
                    kind: ExprKind::Literal(Value::Int(42)),
                    span: 8..10,
                }),
            },
            span: 0..11,
        }];

        let ast = Parser::new(code).init().unwrap();
        assert_eq!(ast, expected);
    }

    #[test]
    fn if_flow() {
        let code = r#"if (a == always) { "Buy Able products!" print; }"#;
        let expected = &[Stmt {
            kind: StmtKind::If {
                cond: Expr {
                    kind: ExprKind::BinOp {
                        lhs: Box::new(Expr {
                            kind: ExprKind::Variable("a".to_owned()),
                            span: 4..5,
                        }),
                        rhs: Box::new(Expr {
                            kind: ExprKind::Literal(Value::Abool(crate::variables::Abool::Always)),
                            span: 9..15,
                        }),
                        kind: BinOpKind::Equal,
                    },
                    span: 4..15,
                },
                body: Block {
                    block: vec![Stmt {
                        kind: StmtKind::Print(Expr {
                            kind: ExprKind::Literal(Value::Str("Buy Able products!".to_string())),
                            span: 19..39,
                        }),
                        span: 19..46,
                    }],
                },
            },
            span: 0..48,
        }];

        let ast = Parser::new(code).init().unwrap();
        assert_eq!(ast, expected);
    }

    #[test]
    fn tdark() {
        let code = r#"T-Dark { var lang = "lang" + lang; }"#;
        let expected = &[Stmt {
            kind: StmtKind::Var {
                iden: Iden {
                    iden: "script".to_string(),
                    span: 13..17,
                },
                init: Some(Expr {
                    kind: ExprKind::BinOp {
                        lhs: Box::new(Expr {
                            kind: ExprKind::Literal(Value::Str("script".to_string())),
                            span: 20..26,
                        }),
                        rhs: Box::new(Expr {
                            kind: ExprKind::Variable("script".to_string()),
                            span: 29..33,
                        }),
                        kind: BinOpKind::Add,
                    },
                    span: 20..33,
                }),
            },
            span: 9..34,
        }];

        let ast = Parser::new(code).init().unwrap();
        assert_eq!(ast, expected);
    }
}
