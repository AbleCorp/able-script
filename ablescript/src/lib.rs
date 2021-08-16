#![forbid(unsafe_code, clippy::unwrap_used)]

pub mod ast;
pub mod interpret;
pub mod parser;

mod base_55;
mod brian;
mod consts;
mod error;
mod lexer;
mod variables;
