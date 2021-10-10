#![forbid(unsafe_code, clippy::unwrap_used)]

pub mod ast;
pub mod error;
pub mod interpret;
pub mod parser;

mod base_55;
mod brian;
mod consts;
mod lexer;
mod variables;
