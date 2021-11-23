pub mod errors;
pub mod token;
pub mod token_kind;

use std::fmt::Display;

use token::TokenSpan;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: TokenSpan,
    pub node: T,
}

impl<T> Spanned<T> {
    pub fn map<B, C: FnMut(T) -> anyhow::Result<B>>(self, mut val: C) -> anyhow::Result<Spanned<B>> {
        let Spanned {node, span} = self;
        Ok(Spanned {
            node: val(node)?,
            span,
        })
    }
}

impl<T: std::fmt::Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

#[derive(PartialEq)]
pub struct Filed<'input, T> {
    pub filename: &'input str,
    pub contents: &'input str,
    pub node: T,
}
