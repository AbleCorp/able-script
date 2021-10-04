use logos::{Lexer, Logos};

use crate::variables::Abool;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Symbols
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftCurly,

    #[token("}")]
    RightCurly,

    #[token(";")]
    Semicolon,

    #[token(".")]
    Dot,

    #[token(",")]
    Comma,

    // Operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    FwdSlash,

    #[token("=")]
    Equal,

    #[token("<=")]
    Arrow,

    // Logical operators
    #[token("<")]
    LessThan,

    #[token(">")]
    GreaterThan,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    NotEqual,

    #[regex("!|aint")] // also add aint as a not keyword
    Not,

    // Keywords
    #[token("functio")]
    Functio,

    /// Brain fuck FFI
    #[token("bff")]
    Bff,

    /// Variable bro
    #[token("var")]
    Var,

    /// Prints the preceding things
    #[token("print")]
    Print,

    /// Read input into preceding variable
    #[token("read")]
    Read,

    /// Ban the following variable from ever being used again
    #[token("melo")]
    Melo,

    #[token("T-Dark")]
    TDark,

    // Control flow keywords
    #[token("if")]
    If,

    #[token("loop")]
    Loop,

    #[token("break")]
    Break,

    /// HopBack hops on the back of loop - like `continue`
    #[token("hopback")]
    HopBack,

    /// Crash with random error (see discussion #17)
    #[token("rlyeh")]
    Rlyeh,

    #[token("rickroll")]
    Rickroll,

    // Literals
    /// True, False
    #[regex("true|false", get_bool)]
    Bool(bool),

    /// Always, Sometimes, Never
    #[regex("always|sometimes|never", get_abool)]
    Abool(Abool),

    /// Base52 based character ('a')
    #[token("'.*'")]
    Char,

    /// String
    #[regex("\"(\\.|[^\"])*\"", get_string)]
    String(String),

    /// Integer
    #[regex(r"-?[0-9]+", get_int)]
    Integer(i32),

    /// A C-complaint identifier
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", get_ident)]
    Identifier(String),

    #[regex(r"owo .*")]
    Comment,

    #[regex("nul")]
    Nul,

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

fn get_ident(lexer: &mut Lexer<Token>) -> String {
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
            Functio,
            Identifier("test".to_owned()),
            LeftParen,
            RightParen,
            LeftCurly,
            Var,
            Identifier("a".to_owned()),
            Equal,
            Integer(3),
            Semicolon,
            If,
            Identifier("a".to_owned()),
            EqualEqual,
            Integer(3),
            LeftCurly,
            Identifier("a".to_owned()),
            Print,
            RightCurly,
            RightCurly,
        ];
        let lexer = Token::lexer(code);
        let result: Vec<Token> = lexer.collect();
        assert_eq!(result, expected);
    }
}
