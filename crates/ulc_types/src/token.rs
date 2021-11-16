use std::ops::{Index, Range};

use super::token_kind::TokenKind;

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TokenSpan,
}

impl Token {
    #[inline(always)]
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
}

impl From<TokenSpan> for Range<usize> {
    fn from(span: TokenSpan) -> Self {
        span.start..span.end
    }
}

impl From<Range<usize>> for TokenSpan {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<TokenSpan> for str {
    type Output = str;

    fn index(&self, index: TokenSpan) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}
