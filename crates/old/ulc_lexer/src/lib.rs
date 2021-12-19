use logos::Logos;
use ulc_types::{
    token::{Token, TokenSpan},
    token_kind::{LogosTokens, TokenKind},
};

pub struct Lexer<'input> {
    content: &'input str,
    generated: logos::SpannedIter<'input, LogosTokens>,
    end_of_file: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(content: &'input str) -> Self {
        Self {
            content,
            generated: LogosTokens::lexer(content).spanned(),
            end_of_file: false,
        }
    }

    pub fn tokenise(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.generated.next() {
            Some((token, span)) => Some(Token {
                kind: token.into(),
                span: span.into(),
            }),
            None if self.end_of_file => None,
            None => {
                let len = self.content.len();
                self.end_of_file = true;
                Some(Token {
                    kind: TokenKind::EndOfFile,
                    span: TokenSpan {
                        start: len,
                        end: len,
                    },
                })
            }
        }
    }
}
