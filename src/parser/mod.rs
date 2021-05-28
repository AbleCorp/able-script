pub mod item;
mod ops;
mod utils;

use item::Item;
use item::SpannedIden;
use logos::Logos;

use crate::{
    error::Error,
    lexer::SpannedToken,
    parser::item::{Expr, ExprKind, Stmt, StmtKind},
    variables::Value,
};
use crate::{lexer::Token, parser::item::Iden};

pub type ParseResult = Result<Item, Error>;

/// Parser structure / state machine
pub struct Parser<'source> {
    lexer: std::iter::Peekable<logos::SpannedIter<'source, Token>>,
    ast: Vec<Item>,
    tdark: bool,
}

impl<'source> Parser<'source> {
    /// Create a new parser object
    pub fn new(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source).spanned().peekable(),
            ast: Vec::new(),
            tdark: false,
        }
    }

    pub fn init(&mut self) -> Result<Vec<Item>, Error> {
        loop {
            let token = self.lexer.peek().cloned();

            match token {
                Some((Token::Comment, _)) => {
                    self.lexer.next();
                    continue;
                }
                Some((Token::TDark, _)) => {
                    self.lexer.next();
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

    fn parse_item(&mut self, token: Option<SpannedToken>) -> ParseResult {
        if matches!(token, None) {
            return Err(Error::end_of_token_stream());
        }

        let token = token.unwrap();

        match token {
            (Token::Identifier(_), _)
            | (Token::Aboolean(_), _)
            | (Token::Boolean(_), _)
            | (Token::Integer(_), _)
            | (Token::String(_), _)
            | (Token::Nul, _)
            | (Token::LeftParenthesis, _)
            | (Token::Assignment, _)
            | (Token::LogNot, _) => self.parse_ops(token),

            // Control flow
            (Token::If, _) => self.if_cond(),
            (Token::Loop, _) => self.loop_block(),

            (Token::HopBack, span) => {
                self.lexer.next();
                self.require(Token::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::HopBack,
                    span,
                }
                .into())
            }
            (Token::Break, span) => {
                self.lexer.next();
                self.require(Token::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Break,
                    span,
                }
                .into())
            }

            // Declarations
            (Token::Variable, _) => self.variable_declaration(),
            (Token::Function, _) => self.function_declaration(),
            (Token::BfFunction, _) => self.bff_declaration(),

            // Prefix keywords
            // Melo - ban variable from next usage (runtime error)
            (Token::Melo, span) => {
                self.lexer.next();
                let iden = self.require_iden()?;
                let iden_end = iden.span.end;
                self.require(Token::Semicolon)?;
                Ok(Stmt {
                    kind: StmtKind::Melo(iden),
                    span: span.start..iden_end,
                }
                .into())
            }

            (_, span) => Err(Error::unexpected_token(span)),
        }
    }

    /// Parse variable declaration
    ///
    /// `var [iden] = [literal];`
    fn variable_declaration(&mut self) -> ParseResult {
        let (_, span) = self.lexer.next().unwrap();
        let iden = self.require_iden()?;

        let peek = self.lexer.peek();
        let (init, end_pos) = match peek {
            Some((Token::Semicolon, _)) => {
                self.lexer.next();
                (None, iden.span.end)
            }
            Some((Token::Assignment, _)) => {
                self.lexer.next();
                let next = self.lexer.next();
                let mut value = self.parse_expr(next)?;
                loop {
                    let peek = self.lexer.peek().cloned();
                    value = match peek {
                        Some((Token::Semicolon, _)) => break,
                        None => return Err(Error::end_of_token_stream()),
                        Some(t) => self.parse_operation(Some(t), value)?,
                    };
                }
                self.lexer.next();
                let val_end = value.span.end;
                (Some(value), val_end)
            }
            Some((_, span)) => return Err(Error::unexpected_token(span.clone())),
            None => return Err(Error::end_of_token_stream()),
        };

        Ok(Stmt {
            kind: StmtKind::VariableDeclaration { iden, init },
            span: span.start..end_pos,
        }
        .into())
    }

    /// Declare function
    ///
    /// `functio [iden] ([expr], [expr]) { ... }
    fn function_declaration(&mut self) -> ParseResult {
        let (_, span) = self.lexer.next().unwrap();
        let iden = self.require_iden()?;

        self.require(Token::LeftParenthesis)?;
        let mut args = vec![];
        loop {
            let next = self.lexer.next();
            match next {
                Some((Token::RightParenthesis, _)) => break,
                Some((Token::Identifier(i), span)) => args.push(SpannedIden::new(Iden(i), span)),
                Some((_, span)) => return Err(Error::unexpected_token(span)),
                None => return Err(Error::end_of_token_stream()),
            }

            match self.lexer.peek() {
                Some((Token::Comma, _)) => {
                    self.lexer.next();
                    continue;
                }
                Some((Token::RightParenthesis, _)) => {
                    self.lexer.next();
                    break;
                }
                Some((_, span)) => return Err(Error::unexpected_token(span.clone())),
                None => return Err(Error::end_of_token_stream()),
            }
        }

        self.require(Token::LeftBrace)?;

        // Parse function body
        let (body, end) = self.parse_body()?;

        Ok(Stmt {
            kind: StmtKind::FunctionDeclaration { iden, args, body },
            span: span.start..end,
        }
        .into())
    }

    /// Declare BF FFI Function
    ///
    /// `bff [iden] { ... }`
    fn bff_declaration(&mut self) -> ParseResult {
        let (_, span) = self.lexer.next().unwrap();
        let iden = self.require_iden()?;
        self.require(Token::LeftBrace)?;

        let mut body = String::new();
        loop {
            let token = {
                match self.lexer.next() {
                    Some(t) => t,
                    None => return Err(Error::end_of_token_stream()),
                }
            };

            body.push_str(match token {
                (Token::OpGt, _) => ">",
                (Token::OpLt, _) => "<",
                (Token::Addition, _) => "+",
                (Token::Subtract, _) => "-",
                (Token::FullStop, _) => ".",
                (Token::Comma, _) => ",",
                (Token::LeftBracket, _) => "[",
                (Token::RightBracket, _) => "]",
                (Token::RightBrace, end) => {
                    break Ok(Stmt {
                        kind: StmtKind::BfFDeclaration { iden, body },
                        span: span.start..end.end,
                    }
                    .into())
                }
                (_, span) => return Err(Error::unexpected_token(span)),
            });
        }
    }

    /// Parse If-stmt
    pub fn if_cond(&mut self) -> ParseResult {
        let (_, span) = self.lexer.next().unwrap(); // I was hungry :(
        let (_, cond_span) = self.require(Token::LeftParenthesis)?;
        let cond = self.parse_paren(cond_span.start)?;
        self.require(Token::LeftBrace)?;

        let (body, end_span) = self.parse_body()?;

        Ok(Stmt {
            kind: StmtKind::If { cond, body },
            span: span.start..end_span,
        }
        .into())
    }

    /// Parse loop
    pub fn loop_block(&mut self) -> ParseResult {
        let (_, span) = self.lexer.next().unwrap();
        self.require(Token::LeftBrace)?;
        let (body, end) = self.parse_body()?;

        Ok(Stmt {
            kind: StmtKind::Loop { body },
            span: span.start..end,
        }
        .into())
    }

    /// T-Dark block parsing
    pub fn tdark_block(&mut self) -> Result<Vec<Item>, Error> {
        self.require(Token::LeftBrace)?;
        self.tdark = true;
        let mut body = Vec::new();
        loop {
            let token = {
                match self.lexer.peek().cloned() {
                    Some(t) => t,
                    None => return Err(Error::end_of_token_stream()),
                }
            };

            if matches!(token, (Token::RightBrace, _)) {
                self.lexer.next();
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
    use ExprKind::*;
    use StmtKind::*;

    #[test]
    fn control_flow() {
        let code = r#"loop { var a = 3 + 2; if (a == 5) { break; } }"#;
        let expected = &[Item::Stmt(Stmt {
            kind: StmtKind::Loop {
                body: vec![
                    Item::Stmt(Stmt {
                        kind: VariableDeclaration {
                            iden: SpannedIden {
                                iden: Iden("a".to_owned()),
                                span: 11..12,
                            },
                            init: Some(Expr {
                                kind: Add {
                                    left: Box::new(Expr {
                                        kind: Literal(Value::Int(3)),
                                        span: 15..16,
                                    }),
                                    right: Box::new(Expr {
                                        kind: Literal(Value::Int(2)),
                                        span: 19..20,
                                    }),
                                },
                                span: 15..20,
                            }),
                        },
                        span: 7..20,
                    }),
                    Item::Stmt(Stmt {
                        kind: If {
                            cond: Expr {
                                kind: Eq {
                                    left: Box::new(Expr {
                                        kind: Identifier(Iden("a".to_owned())),
                                        span: 26..27,
                                    }),
                                    right: Box::new(Expr {
                                        kind: Literal(Value::Int(5)),
                                        span: 31..32,
                                    }),
                                },
                                span: 25..33,
                            },
                            body: vec![Item::Stmt(Stmt {
                                kind: Break,
                                span: 36..41,
                            })],
                        },
                        span: 22..44,
                    }),
                ],
            },
            span: 0..46,
        })];

        let ast = Parser::new(code).init().unwrap();

        assert_eq!(ast, expected)
    }

    #[test]
    fn tdark() {
        let code = r#"T-Dark { var lang = nul; lang print; }"#;

        let ast = Parser::new(code).init().unwrap();
        let expected = &[
            Item::Stmt(Stmt {
                kind: VariableDeclaration {
                    iden: SpannedIden {
                        iden: Iden("script".to_owned()),
                        span: 13..17,
                    },
                    init: Some(Expr {
                        kind: Literal(Value::Nul),
                        span: 20..23,
                    }),
                },
                span: 9..23,
            }),
            Item::Stmt(Stmt {
                kind: Print(Expr {
                    kind: Identifier(Iden("script".to_owned())),
                    span: 25..29,
                }),
                span: 25..35,
            }),
        ];
        assert_eq!(ast, expected)
    }
}
