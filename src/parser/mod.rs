mod item;
mod utils;

use item::Expr;

use crate::variables::Value;
use crate::{
    error::{Error, ErrorKind},
    tokens::Token,
};

use logos::Logos;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken,
    LexError,
    UnexpectedEOF,
}

/// Parser structure / state machine
pub struct Parser<'a> {
    lexer: logos::Lexer<'a, Token>,
}

impl<'a> Parser<'a> {
    /// Create a new parser object
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Token::lexer(source),
        }
    }

    /// Start parsing Token Vector into Abstract Syntax Tree
    pub fn parse(&mut self) -> Vec<Expr> {
        let mut ast = vec![];
        while let Some(token) = self.lexer.next() {
            let expr = match token {
                Token::Variable => self.variable_declaration(),
                Token::Function => self.function_declaration(),
                Token::BfFunction => self.bff_declaration(),
                Token::RightBrace => return ast,
                _ => Err(Error {
                    kind: ErrorKind::SyntaxError,
                    position: 0..0,
                }),
            };
            match expr {
                Ok(o) => ast.push(o),
                Err(e) => {
                    e.panic(self.lexer.slice());
                    break;
                }
            }
        }

        ast
    }

    /// Parse variable declaration
    ///
    /// `var [iden] = [literal];`
    fn variable_declaration(&mut self) -> Result<Expr, Error> {
        let iden = self.require(Token::Identifier)?;

        let init = match self.lexer.next() {
            Some(Token::Semicolon) => None,
            Some(Token::Assignment) => {
                let value = self.require(Token::Boolean)?; // TODO: Shouldn't be limited to boolean (pattern match?)
                self.require(Token::Semicolon)?;
                Some(value)
            }
            _ => {
                return Err(Error {
                    kind: ErrorKind::SyntaxError,
                    position: self.lexer.span(),
                })
            }
        };

        Ok(Expr::VariableDeclaration { iden, init })
    }

    /// Declare function
    ///
    /// `functio [iden] ([expr], [expr]) { ... }
    fn function_declaration(&mut self) -> Result<Expr, Error> {
        let iden = self.require(Token::Identifier)?;
        self.require(Token::LeftParenthesis)?;
        // TODO: Arguments
        self.require(Token::RightParenthesis)?;
        self.require(Token::LeftBrace)?;
        let body = self.parse();

        Ok(Expr::FunctionDeclaration { iden, body })
    }

    /// Declare BF FFI Function
    ///
    /// `bff [iden] { ... }`
    fn bff_declaration(&mut self) -> Result<Expr, Error> {
        let iden = self.require(Token::Identifier)?;
        self.require(Token::LeftBrace)?;
        let code = self.require(Token::String)?; // <-- Nasty hack, but works
        self.require(Token::RightBrace)?;
        Ok(Expr::BfFDeclaration { iden, code })
    }
}
