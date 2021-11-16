pub mod errors;
pub mod token;
pub mod token_kind;

use std::fmt::Display;

use token::TokenSpan;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T: std::fmt::Display> {
    pub span: TokenSpan,
    pub node: T,
}

impl<T: std::fmt::Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

#[derive(PartialEq)]
pub struct Filed<'input, T: std::fmt::Display> {
    pub filename: &'input str,
    pub contents: &'input str,
    pub node: T,
}
