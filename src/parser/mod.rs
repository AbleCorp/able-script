mod item;
mod utils;

use item::Expr;

use crate::tokens::Token;
use crate::variables::Value;

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
    ast: Vec<Expr>,
}

impl<'a> Parser<'a> {
    /// Create a new parser object
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Token::lexer(source),
            ast: vec![],
        }
    }

    /// Start parsing Token Vector into Abstract Syntax Tree
    pub fn parse(&mut self) -> Vec<Expr> {
        while let Some(token) = self.lexer.next() {
            let expr  = match token {
                Token::Variable => self.variable(),
                tok => {
                    // TODO: Better error handling
                    println!("Parse error");
                    break;
                }
            };
           self.ast.push(expr.unwrap());
        }

        self.ast.clone()
    }

    /// Parse variable declaration
    ///
    /// `var [iden] = [literal];`
    fn variable(&mut self) -> Result<Expr, ParseError> {
        let iden = self.require(Token::Identifier)?;

        let init = match self.lexer.next() {
            Some(Token::Semicolon) => None,
            Some(Token::Assignment) => {
                let value = self.require(Token::Boolean)?; // TODO: Shouldn't be limited to boolean (pattern match?)
                self.require(Token::Semicolon)?;
                Some(value)
            }
            _ => return Err(ParseError::UnexpectedToken),
        };

        Ok(Expr::DeclareVariable { iden, init })
    }
}
