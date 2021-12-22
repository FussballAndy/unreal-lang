use chumsky::prelude::*;
use ulc_ast::Lit;
use ulc_types::{errors::SyntaxError, Token};

pub(super) fn parser_literal() -> impl Parser<Token, Lit, Error = SyntaxError> {
    filter_map(|span, tok| {
        Ok(match tok {
            Token::Bool(b) => Lit::Bool(b),
            Token::String(s) => Lit::String(s),
            Token::Num(i) => Lit::Int(i),
            _ => {
                return Err(SyntaxError::expected_input_found(
                    span,
                    Vec::new(),
                    Some(tok),
                ))
            }
        })
    })
}
