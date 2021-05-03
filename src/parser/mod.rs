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
    tdark: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser object
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: PeekableLexer::lexer(source),
            ast: Vec::new(),
            tdark: false,
        }
    }

    pub fn init(&mut self) -> Result<Vec<Item>, Error> {
        loop {
            let token = self.lexer.next();

            match token {
                Some(Token::Comment) => continue,
                Some(Token::TDark) => {
                    let mut block = self.tdark_block()?;
                    self.ast.append(&mut block);
                }
                None => return Ok(self.ast.clone()),
                _ => {
                    let item = self.parse_item(token)?;
                    self.ast.push(item);
                }
            }
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
            Token::Identifier(_)
            | Token::Aboolean(_)
            | Token::Boolean(_)
            | Token::Integer(_)
            | Token::String(_)
            | Token::Nul
            | Token::LeftParenthesis
            | Token::LogNot => self.parse_ops(token),

            // Control flow
            Token::If => self.if_cond(),
            Token::Loop => self.loop_block(),

            Token::HopBack => {
                self.require(Token::Semicolon)?;
                Ok(Stmt::HopBack.into())
            }
            Token::Break => {
                self.require(Token::Semicolon)?;
                Ok(Stmt::Break.into())
            }

            // Declarations
            Token::Variable => self.variable_declaration(),
            Token::Function => self.function_declaration(),
            Token::BfFunction => self.bff_declaration(),

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
        let mut args = vec![];
        loop {
            let next = self.lexer.next();
            match next {
                Some(Token::RightParenthesis) => break,
                Some(Token::Identifier(i)) => args.push(Iden(i)),
                _ => return Err(self.unexpected_token(None)),
            }
        }
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

    /// Parse If-stmt
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

    /// Parse loop
    pub fn loop_block(&mut self) -> ParseResult {
        self.require(Token::LeftBrace)?;
        let body = self.parse_body()?;

        Ok(Stmt::Loop { body }.into())
    }

    /// T-Dark block parsing
    pub fn tdark_block(&mut self) -> Result<Vec<Item>, Error> {
        self.require(Token::LeftBrace)?;
        self.tdark = true;
        let mut body = Vec::new();
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

            if token == Token::RightBrace {
                break;
            }
            body.push(self.parse_item(Some(token))?);
        }
        self.tdark = false;
        Ok(body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Expr::*;
    use Stmt::*;

    #[test]
    fn control_flow() {
        let code = r#"loop { var a = 3 + 2; if (a == 5) { break; } }"#;

        let expected: &[Item] = &[Item::Stmt(Loop {
            body: vec![
                VariableDeclaration {
                    iden: Iden("a".to_owned()),
                    init: Some(Box::new(
                        Add {
                            left: Box::new(Literal(Value::Int(3))),
                            right: Box::new(Literal(Value::Int(2))),
                        }
                        .into(),
                    )),
                }
                .into(),
                If {
                    cond: Box::new(
                        Eq {
                            left: Box::new(Iden("a".to_owned()).into()),
                            right: Box::new(Literal(Value::Int(5)).into()),
                        }
                        .into(),
                    ),
                    body: vec![Break.into()],
                }
                .into(),
            ],
        })];
        let ast = Parser::new(code).init().unwrap();

        assert_eq!(ast, expected)
    }

    #[test]
    fn tdark() {
        let code = r#"T-Dark { var lang = nul; lang print; }"#;
        let expected: &[Item] = &[
            VariableDeclaration {
                iden: Iden("script".to_owned()),
                init: Some(Box::new(Literal(Value::Nul).into())),
            }
            .into(),
            Print(Iden("script".to_owned()).into()).into(),
        ];

        let ast = Parser::new(code).init().unwrap();

        assert_eq!(ast, expected)
    }
}
