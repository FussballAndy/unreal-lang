use crate::{
    token::{TokenSpan, TokenSpanWrap},
    types::ULCType,
    Token,
};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

use super::Spanned;

pub type ParseResult<T> = anyhow::Result<super::Spanned<T>, SyntaxError>;
pub type SyntaxResult<T> = anyhow::Result<T, SyntaxError>;

pub enum SyntaxError {
    /// chumsky related error
    /// Returned when chumsky finds a Token that should not be where it is
    UnexpectedInput {
        span: TokenSpan,
        expected: Vec<Option<Token>>,
        got: Option<Token>,
    },

    /// Returned when a ident is not found
    IdentNotFound(Spanned<String>),

    /// Returned when there is an assigment to a non-mutable var
    NotMutableVar {
        ident: String,
        declared: TokenSpan,
        used: TokenSpan,
    },

    /// Returned when an if-expr. is used as a value but not
    /// complete.
    InvalidIfExpression {
        span: TokenSpan,
        sp_msg: Spanned<&'static str>,
    },

    /// Returned when an ident is already defined
    AlreadyDeclaredIdent {
        ident: String,
        existing: TokenSpan,
        new: TokenSpan,
        /// True if the idents belong to functions
        /// False if the idents belong to variables
        is_function: bool,
    },

    /// Returned when two expressions do not have the same type
    NotMatchingType {
        expected: Spanned<Vec<ULCType>>,
        got: Spanned<ULCType>,
    },

    /// Returned when a function got called with either too
    /// many or too less arguments.
    FuncCallArgAmount {
        func_sig: String,
        span: TokenSpan,
        /// True if too many arguments are passed
        /// False if too few arguments are passed
        m_or_l: bool,
    },

    /// As of right now, string mutability is not supported,
    /// because I do not have any idea on how to do it.
    NotSupported {
        span: TokenSpan,
        message: &'static str,
    },
}

impl chumsky::Error<Token> for SyntaxError {
    type Span = std::ops::Range<usize>;

    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        Self::UnexpectedInput {
            span: span.into(),
            expected: expected.into_iter().collect(),
            got: found,
        }
    }

    fn with_label(self, (): Self::Label) -> Self {
        self
    }

    fn merge(mut self, other: Self) -> Self {
        if let (
            Self::UnexpectedInput { expected, .. },
            Self::UnexpectedInput {
                expected: expected_other,
                ..
            },
        ) = (&mut self, other)
        {
            for exp_other in expected_other {
                if !expected.contains(&exp_other) {
                    expected.push(exp_other);
                }
            }
        }
        self
    }
}

fn paint<T: std::fmt::Display>(item: T, color: u8) -> impl std::fmt::Display {
    item.fg(Color::Fixed(color))
}

impl SyntaxError {
    pub fn display(self, input: &str, file_id: &str) {
        let make_span = |sp| TokenSpanWrap(file_id, sp);
        let paint_optional_token = |tok| match tok {
            Some(tok) => format!("{}", tok),
            None => "end of file".fg(Color::Fixed(124)).to_string(),
        };
        let report = Report::build(ReportKind::Error, file_id, 0);
        let report = match self {
            SyntaxError::UnexpectedInput {
                span,
                expected,
                got,
            } => report.with_message("Unexpected input.").with_label(
                Label::new(make_span(span)).with_message(format!(
                    "Expected any of {} got {}",
                    expected
                        .into_iter()
                        .map(paint_optional_token)
                        .collect::<Vec<_>>()
                        .join(", "),
                    paint_optional_token(got)
                )),
            ),
            SyntaxError::IdentNotFound(idt) => report
                .with_message(format!("Ident {} was not found.", paint(idt.node, 248)))
                .with_label(Label::new(make_span(idt.span)).with_message("Ident used here!")),
            SyntaxError::NotMutableVar {
                used,
                declared,
                ident,
            } => report
                .with_message("Tried to mutate const variable.")
                .with_label(Label::new(make_span(declared)).with_message(format!(
                    "Variable {} declared as const here.",
                    paint(ident, 248)
                )))
                .with_label(
                    Label::new(make_span(used))
                        .with_message(format!("Later {} here.", paint("mutated", 207))),
                ),
            SyntaxError::InvalidIfExpression { span, sp_msg } => report
                .with_message(format!("Invalid {} expression.", paint("if", 207)))
                .with_label(Label::new(make_span(span)))
                .with_label(Label::new(make_span(sp_msg.span)).with_message(sp_msg.node)),
            SyntaxError::NotMatchingType { expected, got } => report
                .with_message(format!("Invalid {}", paint("type", 207)))
                .with_label(Label::new(make_span(expected.span)).with_message(format!(
                        "This expects {}.",
                        expected
                            .node
                            .into_iter()
                            .map(|ty| paint(ty, 226).to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )))
                .with_label(
                    Label::new(make_span(got.span))
                        .with_message(format!("This has {}", paint(got.node, 226))),
                ),
            SyntaxError::FuncCallArgAmount {
                func_sig,
                span,
                m_or_l,
            } => report
                .with_message(if m_or_l {
                    "Too many arguments provided!"
                } else {
                    "Too few arguments provided!"
                })
                .with_label(Label::new(make_span(span)).with_message(format!(
                    "This function's signature is {}!",
                    paint(func_sig, 240)
                ))),
            SyntaxError::NotSupported { span, message } => report
                .with_message("Use of unsupported feature!")
                .with_label(Label::new(make_span(span)).with_message(message)),
            SyntaxError::AlreadyDeclaredIdent {
                ident,
                existing,
                new,
                is_function,
            } => report
                .with_message(format!(
                    "{} {} is already declared!",
                    paint(if is_function { "Function" } else { "Variable" }, 207),
                    paint(&ident, 248)
                ))
                .with_label(
                    Label::new(make_span(existing))
                        .with_message(format!("Ident {} declared here.", paint(ident, 248))),
                )
                .with_label(
                    Label::new(make_span(new))
                        .with_message(format!("Later again declared {}!", paint("here", 197))),
                ),
        };
        report
            .finish()
            .eprint((file_id, Source::from(input)))
            .unwrap();
    }
}
