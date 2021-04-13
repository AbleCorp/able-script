use std::ops::Range;

use logos::Logos;

use crate::tokens::{self, Token};
pub struct Scanner<'a> {
    source: &'a str,
    lexer: logos::Lexer<'a, Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            lexer: tokens::Token::lexer(source),
        }
    }

    pub fn scan(&mut self) {
        while let Some(tok) = self.lexer.next() {
            if matches!(tok, Token::Error) {
                self.throw_err(&self.lexer.span());
            } else {
                println!("Token: {:?}", tok);
            }
        }
    }

    fn throw_err(&self, location: &Range<usize>) {
        let part = &self.source[location.clone()];
        println!("Unknown keyword `{}` found on {:?}", part, location);
    }
}
