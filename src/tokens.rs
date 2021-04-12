use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    /// A C-complaint identifier
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,

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

    #[regex(r"#.*")]
    Comment,

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

    /// Base52 based character ('a')
    #[token("'.*'")]
    Char,

    #[token("functio")]
    Function,

    /// Brain fuck FFI
    #[token("bff")]
    BfFunction,

    /// Variable bro
    #[token("var")]
    Variable,

    /// True, False
    #[regex("true|false")]
    Boolean,

    /// Always, Sometimes, Never
    #[regex("always|sometimes|never")]
    Aboolean,

    /// String
    #[regex("\"(\\.|[^\"])*\"")]
    String,

    /// Integer
    #[regex(r"[0-9]+")]
    Integer,

    /// Prints the preceding things
    #[token("print")]
    Print,

    /// Ban the following variable from ever being used again
    #[token("melo")]
    Melo,

    #[token("T-Dark")]
    TDark,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

#[derive(Debug, PartialEq)]
pub enum Abool {
    Never = -1,
    Sometimes = 0,
    Always = 1,
}
