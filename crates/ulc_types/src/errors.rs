use crate::token::TokenSpan;

use super::token::Token;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

use super::Spanned;

pub type ParseResult<T> = anyhow::Result<super::Spanned<T>, SyntaxError>;
pub type SyntaxResult<T> = anyhow::Result<T, SyntaxError>;

pub struct FiledError<'input> {
    pub filename: &'input str,
    pub contents: &'input str,
    pub error: SyntaxError,
}

impl<'input> FiledError<'input> {
    pub fn display(&self) {
        self.error.display(self.contents, self.filename);
    }
}

pub enum SyntaxError {
    UnexpectedToken {
        expected: String,
        token: Token,
    },
    InvalidLiteral(Token),
    UnexpectedEndOfInput(Token),
    InvalidToken(Token),
    InvalidIdent(Spanned<String>),
    InvalidType(Spanned<String>),
    InvalidIfExpression {
        span: TokenSpan,
        sp_msg: Spanned<String>,
    },

    End,
}

impl SyntaxError {
    pub fn display(&self, input: &str, file_id: &str) {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_id, input);
        let diagnostic = match self {
            Self::UnexpectedToken { expected, token } => Diagnostic::error()
                .with_message(format!(
                    "Unexpected token: Expected {}, got {}",
                    expected, token.kind,
                ))
                .with_labels(vec![Label::primary(file_id, token.span)
                    .with_message(format!("Expected {}, got {}", expected, token.kind))]),
            Self::InvalidLiteral(token) => Diagnostic::error()
                .with_message(format!("Invalid {}: '{}'", token.kind, token.text(input)))
                .with_labels(vec![Label::primary(file_id, token.span)
                    .with_message(format!("Invalid {}", token.kind))]),
            Self::UnexpectedEndOfInput(token) => Diagnostic::error()
                .with_message("Unexpected end of input".to_owned())
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message("Unexpected end of input")
                ]),
            Self::InvalidToken(token) => Diagnostic::error()
                .with_message(token.kind.to_string())
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message(token.kind.to_string())
                ]),
            Self::InvalidIdent(token) => Diagnostic::error()
                .with_message(format!("Used Ident '{}' doesn't exist!", token.node))
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message("Unknown ident")
                ]),
            Self::InvalidType(token) => Diagnostic::error()
                .with_message(format!("Invalid type '{}'!", token.node))
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message("Unknown type")
                ]),
            Self::InvalidIfExpression { span, sp_msg } => {
                Diagnostic::error().with_message("").with_labels(vec![
                    Label::primary(file_id, *span),
                    Label::secondary(file_id, sp_msg.span).with_message(sp_msg.node.clone()),
                ])
            }
            Self::End => unreachable!(),
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}
