use crate::{token::TokenSpan, types::ULCType};

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
    NotMutableVar(Spanned<String>),
    InvalidIfExpression {
        span: TokenSpan,
        sp_msg: Spanned<&'static str>,
    },
    AlreadyDeclaredVar(Spanned<String>),
    AlreadyDeclaredFunc(Spanned<String>, Spanned<String>),
    NotMatchingType {
        expected: Vec<ULCType>,
        got: ULCType,
        span: TokenSpan,
    },
    FuncCallArgAmount {
        func_sig: String,
        span: TokenSpan,
        m_or_l: bool,
    },
    NotSupported {
        span: TokenSpan,
        message: &'static str,
    },

    /// An error that appears if someone breaks the matrix
    /// and somehow manages to get a statement somewhere
    /// where it does not belong!
    MessedUpMatrix(TokenSpan),

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
                    Label::secondary(file_id, sp_msg.span).with_message(sp_msg.node),
                ])
            }
            Self::NotMutableVar(var) => Diagnostic::error()
                .with_message("Variable is not mutable!")
                .with_labels(vec![
                    Label::primary(file_id, var.span).with_message("Declared using const, not let")
                ]),
            Self::AlreadyDeclaredVar(var) => Diagnostic::error()
                .with_message("Variable already declared")
                .with_labels(vec![
                    Label::primary(file_id, var.span).with_message("Variable already declared!")
                ]),
            Self::AlreadyDeclaredFunc(alr_func, func) => Diagnostic::error()
                .with_message("Function already declared!")
                .with_labels(vec![
                    Label::primary(file_id, alr_func.span).with_message("Function declared here!"),
                    Label::secondary(file_id, func.span).with_message("Later again declared here!"),
                ]),
            Self::NotMatchingType {
                expected,
                got,
                span,
            } => Diagnostic::error()
                .with_message(format!(
                    "Types are not matching! Expected: [{}], Got: {}",
                    expected
                        .iter()
                        .map(|e| e.into())
                        .collect::<Vec<&str>>()
                        .join(", "),
                    got
                ))
                .with_labels(vec![Label::primary(file_id, *span).with_message(format!(
                    "Expression returns {}. Expected {}.",
                    got,
                    expected
                        .iter()
                        .map(|e| e.into())
                        .collect::<Vec<&str>>()
                        .join(", ")
                ))]),
            Self::FuncCallArgAmount {
                func_sig,
                span,
                m_or_l,
            } => Diagnostic::error()
                .with_message(format!(
                    "Too {} arguments provided! Function signature is {}",
                    if *m_or_l { "many" } else { "few" },
                    func_sig
                ))
                .with_labels(vec![Label::primary(file_id, *span)]),
            Self::MessedUpMatrix(sp) => Diagnostic::error()
                .with_message("Please stop rigging the matrix!")
                .with_labels(vec![Label::primary(file_id, *sp)]),
            Self::NotSupported { span, message } => Diagnostic::error()
                .with_message(*message)
                .with_labels(vec![Label::primary(file_id, *span)]),
            Self::End => unreachable!(),
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}
