use std::iter::Peekable;

use ulc_ast::types::Type;
use ulc_checker::CheckerContext;
use ulc_lexer::Lexer;
use ulc_types::{
    errors::{SyntaxError, SyntaxResult},
    token::Token,
    token_kind::TokenKind,
    Spanned,
};

mod expression;
mod statement;

pub struct Parser<'input, 'b> {
    filename: &'input str,
    input: &'input str,
    checker: &'b mut CheckerContext<'input>,
    lexer: Peekable<Lexer<'input>>,
}

impl<'input, 'b> Parser<'input, 'b> {
    pub fn new(
        filename: &'input str,
        input: &'input str,
        checker: &'b mut CheckerContext<'input>,
    ) -> Self {
        Self {
            filename,
            input,
            checker,
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.lexer.next().ok_or_else(|| {
            SyntaxError::UnexpectedEndOfInput(Token {
                kind: TokenKind::EndOfFile,
                span: (self.input.len()..self.input.len()).into(),
            })
        })
    }

    #[inline(always)]
    pub fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }

    #[inline(always)]
    pub fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::EndOfFile)
    }

    #[inline(always)]
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    #[inline(always)]
    pub fn multi_at(&mut self, kinds: &'static [TokenKind]) -> bool {
        kinds.contains(&self.peek())
    }

    pub fn consume(&mut self, expected: TokenKind) -> SyntaxResult<()> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                token,
            })
        } else {
            Ok(())
        }
    }

    pub fn consume_next(&mut self, expected: TokenKind) -> SyntaxResult<Token> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                token,
            })
        } else {
            Ok(token)
        }
    }

    pub fn consume_type(&mut self) -> SyntaxResult<Type> {
        let type_token = self.consume_next(TokenKind::Type)?;
        let type_text = self.text(type_token);
        Type::try_from(Spanned {
            node: type_text.to_owned(),
            span: type_token.span,
        })
    }
}