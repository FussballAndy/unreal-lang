pub mod errors;
pub mod token;
mod types;

use token::TokenSpan;

pub use token::Token;
pub use types::ULCType;

use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: TokenSpan,
    pub node: T,
}

impl<T> Spanned<T> {
    pub fn new<S: Into<TokenSpan>>(span: S, node: T) -> Self {
        Self {
            span: span.into(),
            node,
        }
    }
}

impl<T: std::fmt::Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}
