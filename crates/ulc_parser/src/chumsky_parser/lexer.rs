use chumsky::prelude::*;
use ulc_types::Token;

type Span = std::ops::Range<usize>;

pub(super) fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int(10).map(|s: String| Token::Num(s.parse().unwrap()));

    let stri = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect()
        .map(Token::String);

    let oper = one_of("+-*/!=<>")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Operator);

    let ctrl = one_of("(),:;").map(Token::Ctrl);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "function" => Token::Func,
        "let" => Token::Let,
        "const" => Token::Const,
        "if" => Token::If,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "do" => Token::Do,
        "then" => Token::Then,
        "else" => Token::Else,
        "end" => Token::End,
        "Unit" => Token::Type("Unit"),
        "Int" => Token::Type("Int"),
        "String" => Token::Type("String"),
        "Bool" => Token::Type("Bool"),
        "and" => Token::Operator("and".to_owned()),
        "or" => Token::Operator("or".to_owned()),
        _ => Token::Ident(ident),
    });

    let token = num
        .or(stri)
        .or(oper)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span: Span| (tok, span))
        .padded()
        .repeated()
}
