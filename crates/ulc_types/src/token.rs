use std::{
    hash::Hash,
    ops::{Index, Range},
};

use ariadne::{Color, Fmt};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
}

impl chumsky::Span for TokenSpan {
    type Context = ();

    type Offset = usize;

    fn new(_: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub struct TokenSpanWrap<Id: std::fmt::Debug + Hash + PartialEq + Eq + ToOwned>(
    pub Id,
    pub TokenSpan,
);

impl<Id: std::fmt::Debug + Hash + PartialEq + Eq + ToOwned> ariadne::Span for TokenSpanWrap<Id> {
    type SourceId = Id;

    fn source(&self) -> &Self::SourceId {
        &self.0
    }

    fn start(&self) -> usize {
        self.1.start
    }

    fn end(&self) -> usize {
        self.1.end
    }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Bool(bool),
    Num(i32),
    String(String),
    Operator(String),
    Ctrl(char),
    Ident(String),
    Type(&'static str),
    Func,
    Let,
    Const,
    If,
    Do,
    Then,
    Else,
    End,
    Import,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bool(b) => write!(f, "{}", b.fg(Color::Fixed(166))),
            Token::Num(i) => write!(f, "{}", i.fg(Color::Fixed(139))),
            Token::String(s) => write!(f, "{}", s.fg(Color::Fixed(28))),
            Token::Operator(o) => write!(f, "{}", o.fg(Color::Fixed(23))),
            Token::Ctrl(c) => write!(f, "{}", c.fg(Color::Fixed(247))),
            Token::Ident(i) => write!(f, "{}", i.fg(Color::Fixed(248))),
            Token::Type(t) => write!(f, "{}", t.fg(Color::Fixed(226))),
            Token::Func => write!(f, "{}", "function".fg(Color::Fixed(207))),
            Token::Let => write!(f, "{}", "let".fg(Color::Fixed(207))),
            Token::Const => write!(f, "{}", "const".fg(Color::Fixed(207))),
            Token::If => write!(f, "{}", "if".fg(Color::Fixed(207))),
            Token::Do => write!(f, "{}", "do".fg(Color::Fixed(207))),
            Token::Then => write!(f, "{}", "then".fg(Color::Fixed(207))),
            Token::Else => write!(f, "{}", "else".fg(Color::Fixed(207))),
            Token::End => write!(f, "{}", "end".fg(Color::Fixed(207))),
            Token::Import => write!(f, "{}", "import".fg(Color::Fixed(207))),
        }
    }
}
