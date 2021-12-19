use std::ops::{Index, Range};

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
