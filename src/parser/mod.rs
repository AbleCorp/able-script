mod item;
mod ops;
mod utils;

use item::Item;

use crate::{
    error::{Error, ErrorKind},
    lexer::PeekableLexer,
    parser::item::{Expr, Stmt},
    variables::Value,
};
use crate::{lexer::Token, parser::item::Iden};

pub type ParseResult = Result<Item, Error>;

/// Parser structure / state machine
pub struct Parser<'a> {
    lexer: PeekableLexer<'a>,
    ast: Vec<Item>,
}

impl<'a> Parser<'a> {
    /// Create a new parser object
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: PeekableLexer::lexer(source),
            ast: Vec::new(),
        }
    }

    pub fn init(&mut self) -> Result<Vec<Item>, Error> {
        loop {
            let token = self.lexer.next();
            if token.is_none() {
                return Ok(self.ast.clone());
            };

            let expr = self.parse_item(token)?;
            self.ast.push(expr);
        }
    }

    fn parse_item(&mut self, token: Option<Token>) -> ParseResult {
        if matches!(token, None) {
            return Err(Error {
                kind: ErrorKind::EndOfTokenStream,
                position: self.lexer.span(),
            });
        }

        let token = token.unwrap();
        let start = self.lexer.span().start;

        match token {
            Token::Identifier(_) => self.parse_ops(token).map(|x| x.into()),
            // Control flow
            Token::If => self.if_cond(),

            // Declarations
            Token::Variable => self.variable_declaration(),
            Token::Function => self.function_declaration(),
            Token::BfFunction => self.bff_declaration(),

            // Literals
            Token::String(x) => Ok(Expr::Literal(Value::Str(x)).into()),
            Token::Integer(x) => Ok(Expr::Literal(Value::Int(x)).into()),
            Token::Boolean(x) => Ok(Expr::Literal(Value::Bool(x)).into()),
            Token::Aboolean(x) => Ok(Expr::Literal(Value::Abool(x)).into()),

            // Prefix keywords
            // Melo - ban variable from next usage (runtime error)
            Token::Melo => {
                let e = self.require_iden()?;
                self.require(Token::Semicolon)?;
                Ok(Stmt::Melo(e).into())
            }

            _ => Err(Error {
                kind: ErrorKind::SyntaxError("Unexpected identifier".to_owned()),
                position: start..self.lexer.span().end,
            }),
        }
    }

    /// Parse variable declaration
    ///
    /// `var [iden] = [literal];`
    fn variable_declaration(&mut self) -> ParseResult {
        let iden = self.require_iden()?;

        let init = match self.lexer.next() {
            Some(Token::Semicolon) => None,
            Some(Token::Assignment) => {
                let value = self.lexer.next();
                let value = self.parse_item(value)?;
                self.require(Token::Semicolon)?;
                Some(Box::new(value))
            }
            _ => {
                return Err(Error {
                    kind: ErrorKind::SyntaxError("Unexpected token".to_owned()),
                    position: self.lexer.span(),
                })
            }
        };

        Ok(Stmt::VariableDeclaration { iden, init }.into())
    }

    /// Declare function
    ///
    /// `functio [iden] ([expr], [expr]) { ... }
    fn function_declaration(&mut self) -> ParseResult {
        let iden = self.require_iden()?;
        self.require(Token::LeftParenthesis)?;
        let args = vec![];
        self.require(Token::RightParenthesis)?;

        self.require(Token::LeftBrace)?;
        // Parse function body
        let body = self.parse_body()?;

        Ok(Stmt::FunctionDeclaration { iden, args, body }.into())
    }

    /// Declare BF FFI Function
    ///
    /// `bff [iden] { ... }`
    fn bff_declaration(&mut self) -> ParseResult {
        let iden = self.require_iden()?;
        self.require(Token::LeftBrace)?;

        let mut body = String::new();
        loop {
            let token = {
                match self.lexer.next() {
                    Some(t) => t,
                    None => {
                        return Err(Error {
                            kind: ErrorKind::EndOfTokenStream,
                            position: self.lexer.span(),
                        })
                    }
                }
            };

            body.push_str(match token {
                Token::OpGt
                | Token::OpLt
                | Token::Addition
                | Token::Subtract
                | Token::FullStop
                | Token::Comma
                | Token::LeftBracket
                | Token::RightBracket => self.lexer.slice(),
                Token::RightBrace => break,
                _ => return Err(self.unexpected_token(None)),
            });
        }
        Ok(Stmt::BfFDeclaration { iden, body }.into())
    }

    /// Parse If-expression
    pub fn if_cond(&mut self) -> ParseResult {
        self.require(Token::LeftParenthesis)?;
        let cond = self.lexer.next();
        let cond = self.parse_item(cond)?;
        self.require(Token::RightParenthesis)?;

        self.require(Token::LeftBrace)?;

        let body = self.parse_body()?;

        Ok(Stmt::If {
            cond: Box::new(cond),
            body,
        }
        .into())
    }
}
