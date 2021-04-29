use logos::{Lexer, Logos, Span};

use crate::variables::Abool;

pub struct PeekableLexer<'source> {
    lexer: Lexer<'source, Token>,
    peeked: Option<Option<Token>>,
}

impl<'source> PeekableLexer<'source> {
    pub fn lexer(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source),
            peeked: None,
        }
    }

    /// Returns a reference to the next() value without advancing the iterator.
    #[inline]
    pub fn peek(&mut self) -> &Option<Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next());
        }
        self.peeked.as_ref().unwrap()
    }

    /// Get the range for the current token in `Source`.
    #[inline]
    pub fn span(&self) -> Span {
        self.lexer.span()
    }

    /// Get a string slice of the current token.
    #[inline]
    pub fn slice(&self) -> &'source str {
        self.lexer.slice()
    }

    /// Get a slice of remaining source, starting at the end of current token.
    #[inline]
    pub fn remainder(&self) -> &'source str {
        self.lexer.remainder()
    }
}

impl<'source> Iterator for PeekableLexer<'source> {
    type Item = Token;

    /// Advances the iterator and returns the next value.
    ///
    /// Returns [`None`] when iteration is finished.
    /// Individual iterator implementations may choose to resume iteration, and so calling `next()`
    /// again may or may not eventually start returning [`Some(Item)`] again at some point.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.lexer.next(),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("functio")]
    Function,

    /// Brain fuck FFI
    #[token("bff")]
    BfFunction,

    /// Variable bro
    #[token("var")]
    Variable,

    /// Prints the preceding things
    #[token("print")]
    Print,

    /// Ban the following variable from ever being used again
    #[token("melo")]
    Melo,

    #[token("T-Dark")]
    TDark,

    // Expressions
    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("loop")]
    Loop,

    // Literals
    /// True, False
    #[regex("true|false", get_bool)]
    Boolean(bool),

    /// Always, Sometimes, Never
    #[regex("always|sometimes|never", get_abool)]
    Aboolean(Abool),

    /// String
    #[regex("\"(\\.|[^\"])*\"", get_string)]
    String(String),

    /// Integer
    #[regex(r"[0-9]+", get_int)]
    Integer(i32),

    /// A C-complaint identifier
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", get_iden)]
    Identifier(String),

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(";")]
    Semicolon,

    #[token(".")]
    FullStop,

    #[token(",")]
    Comma,

    #[regex(r"#.*")]
    Comment,

    // Operators
    #[token("-")]
    Subtract,

    #[token("+")]
    Addition,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("=")]
    Assignment,

    // Logical operators
    #[token("<")]
    OpLt,

    #[token(">")]
    OpGt,

    #[token("==")]
    OpEq,

    #[token("!=")]
    OpNeq,

    /// Base52 based character ('a')
    #[token("'.*'")]
    Char,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

fn get_bool(lexer: &mut Lexer<Token>) -> Option<bool> {
    lexer.slice().parse().ok()
}

fn get_int(lexer: &mut Lexer<Token>) -> Option<i32> {
    lexer.slice().parse().ok()
}

fn get_string(lexer: &mut Lexer<Token>) -> String {
    lexer.slice().trim_matches('"').to_owned()
}

fn get_abool(lexer: &mut Lexer<Token>) -> Option<Abool> {
    match lexer.slice() {
        "always" => Some(Abool::Always),
        "sometimes" => Some(Abool::Sometimes),
        "never" => Some(Abool::Never),
        _ => None,
    }
}

fn get_iden(lexer: &mut Lexer<Token>) -> String {
    lexer.slice().to_owned()
}

#[cfg(test)]
mod tests {
    use super::Token;
    use super::Token::*;
    use logos::Logos;

    #[test]
    fn simple_fn() {
        let code = "functio test() { var a = 3; if a == 3 { a print } }";
        let expected = &[
            Function,
            Identifier("test".to_owned()),
            LeftParenthesis,
            RightParenthesis,
            LeftBrace,
            Variable,
            Identifier("a".to_owned()),
            Assignment,
            Integer(3),
            Semicolon,
            If,
            Identifier("a".to_owned()),
            OpEq,
            Integer(3),
            LeftBrace,
            Identifier("a".to_owned()),
            Print,
            RightBrace,
            RightBrace,
        ];
        let lexer = Token::lexer(code);
        let result: Vec<Token> = lexer.collect();
        assert_eq!(result, expected);
    }
}
