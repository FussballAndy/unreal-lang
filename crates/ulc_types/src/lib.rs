pub mod errors;
pub mod token;
mod types;
pub use types::ULCType;

use std::fmt::Display;

use token::TokenSpan;

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
