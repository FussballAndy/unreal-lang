use chumsky::prelude::*;
use ulc_ast::Lit;

use super::lexer::Token;

pub(super) fn parser_literal() -> impl Parser<Token, Lit, Error = Simple<Token>> {
    filter_map(|span, tok| {
        Ok(match tok {
            Token::Bool(b) => Lit::Bool(b),
            Token::String(s) => Lit::String(s),
            Token::Num(i) => Lit::Int(i),
            _ => return Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
    })
}
