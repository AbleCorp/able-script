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
}

impl<'source> Parser<'source> {
    /// Create a new parser from source code
    pub fn new(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source),
        }
    }

    /// Start parsing tokens
    ///
    /// Loops trough lexer, parses statements, returns AST
    pub fn init(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut ast = vec![];
        while let Some(token) = self.lexer.next() {
            ast.push(self.parse(token)?);
        }
        Ok(ast)
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
            t => Err(Error {
                kind: ErrorKind::UnexpectedToken(t),
                span: start..self.lexer.span().end,
            }),
        }
    }

    /// Require next item to be equal with expected one
    fn require(&mut self, expected: Token) -> Result<(), Error> {
        match self.lexer.next() {
            Some(t) if t == expected => Ok(()),
            Some(t) => Err(Error::new(ErrorKind::UnexpectedToken(t), self.lexer.span())),
            None => Err(Error::unexpected_eof()),
        }
    }

    /// Get an Identifier
    fn get_iden(&mut self) -> Result<Iden, Error> {
        match self.lexer.next().ok_or(Error::unexpected_eof())? {
            Token::Identifier(iden) => Ok(Iden {
                iden,
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
        let start = self.lexer.span().start;

        match token {
            // Values
            Token::Identifier(i) => Ok(Expr::new(
                ExprKind::Variable(i),
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
                ExprKind::Literal(Value::Str(s)),
                start..self.lexer.span().end,
            )),
            Token::Nul => Ok(Expr::new(
                ExprKind::Literal(Value::Nul),
                start..self.lexer.span().end,
            )),

            // Operations
            Token::Plus => Ok(Expr::new(
                self.op_flow(BinOpKind::Add, buf)?,
                start..self.lexer.span().end,
            )),
            Token::Minus => Ok(Expr::new(
                self.op_flow(BinOpKind::Subtract, buf)?,
                start..self.lexer.span().end,
            )),
            Token::Star => Ok(Expr::new(
                self.op_flow(BinOpKind::Multiply, buf)?,
                start..self.lexer.span().end,
            )),
            Token::FwdSlash => Ok(Expr::new(
                self.op_flow(BinOpKind::Divide, buf)?,
                start..self.lexer.span().end,
            )),
            Token::EqualEqual => Ok(Expr::new(
                self.op_flow(BinOpKind::Equal, buf)?,
                start..self.lexer.span().end,
            )),
            Token::NotEqual => Ok(Expr::new(
                self.op_flow(BinOpKind::NotEqual, buf)?,
                start..self.lexer.span().end,
            )),
            Token::And => Ok(Expr::new(
                self.op_flow(BinOpKind::And, buf)?,
                start..self.lexer.span().end,
            )),
            Token::Or => Ok(Expr::new(
                self.op_flow(BinOpKind::Or, buf)?,
                start..self.lexer.span().end,
            )),
            Token::LessThan => Ok(Expr::new(
                self.op_flow(BinOpKind::Less, buf)?,
                start..self.lexer.span().end,
            )),
            Token::GreaterThan => Ok(Expr::new(
                self.op_flow(BinOpKind::Greater, buf)?,
                start..self.lexer.span().end,
            )),

            Token::Not => Ok(Expr::new(
                {
                    let next = self.lexer.next().ok_or(Error::unexpected_eof())?;
                    ExprKind::Not(Box::new(self.parse_expr(next, buf)?))
                },
                start..self.lexer.span().end,
            )),
            Token::LeftParen => self.expr_flow(Token::RightParen),
            t => Err(Error::new(
                ErrorKind::UnexpectedToken(t),
                start..self.lexer.span().end,
            )),
        }
    }

    /// Flow for operators
    ///
    /// Generates operation from LHS buffer and next expression as RHS
    ///
    /// This is unaware of precedence, as AbleScript do not have it
    fn op_flow(&mut self, kind: BinOpKind, lhs: &mut Option<Expr>) -> Result<ExprKind, Error> {
        Ok(ExprKind::BinOp {
            lhs: Box::new(
                lhs.take()
                    .ok_or(Error::new(ErrorKind::MissingLhs, self.lexer.span()))?,
            ),
            rhs: {
                let next = self.lexer.next().ok_or(Error::unexpected_eof())?;
                Box::new(self.parse_expr(next, &mut None)?)
            },
            kind,
        })
    }

    /// Parse expressions until terminate token
    fn expr_flow(&mut self, terminate: Token) -> Result<Expr, Error> {
        let mut buf = None;
        Ok(loop {
            match self.lexer.next().ok_or(Error::unexpected_eof())? {
                t if t == terminate => break buf.take().unwrap(),
                t => buf = Some(self.parse_expr(t, &mut buf)?),
            }
        })
    }

    /// Parse a list of statements between curly braces
    fn parse_block(&mut self) -> Result<Block, Error> {
        self.require(Token::LeftCurly)?;
        let mut block = vec![];

        loop {
            match self.lexer.next().ok_or(Error::unexpected_eof())? {
                Token::RightCurly => break,
                t => block.push(self.parse(t)?),
            }
        }
        Ok(Block { block })
    }

    /// Parse If flow
    ///
    /// Consists of condition and block, there is no else
    fn if_flow(&mut self) -> Result<StmtKind, Error> {
        self.require(Token::LeftParen)?;

        let cond = self.expr_flow(Token::RightParen)?;
        
        let body = self.parse_block()?;

        Ok(StmtKind::If { cond, body })
    }

    /// Parse functio flow
    ///
    /// functio $iden (a, b, c) { ... }
    fn functio_flow(&mut self) -> Result<StmtKind, Error> {
        let iden = self.get_iden()?;

        self.require(Token::LeftParen)?;

        let mut args = vec![];
        loop {
            match self.lexer.next().ok_or(Error::unexpected_eof())? {
                Token::RightParen => break,
                Token::Identifier(i) => {
                    args.push(Iden::new(i, self.lexer.span()));

                    match self.lexer.next().ok_or(Error::unexpected_eof())? {
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

        let body = self.parse_block()?;

        Ok(StmtKind::Functio { iden, args, body })
    }
}
