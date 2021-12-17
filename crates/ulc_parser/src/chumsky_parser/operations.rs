use chumsky::prelude::*;
use ulc_ast::{BinaryOperation, BoxedExpression, Expression, UnaryOperation};
use ulc_types::Spanned;

use super::lexer::Token;

type BinOpFun = fn(BoxedExpression, BoxedExpression) -> BinaryOperation;
type UnOpFun = fn(BoxedExpression) -> UnaryOperation;

#[inline(always)]
fn parser_bin_operator() -> impl Parser<Token, BinOpFun, Error = Simple<Token>> {
    just(Token::Operator("+".to_owned()))
        .to(BinaryOperation::Add as BinOpFun)
        .or(just(Token::Operator("-".to_owned())).to(BinaryOperation::Sub as BinOpFun))
        .or(just(Token::Operator("*".to_owned())).to(BinaryOperation::Mul as BinOpFun))
        .or(just(Token::Operator("/".to_owned())).to(BinaryOperation::Div as BinOpFun))
        .or(just(Token::Operator(">".to_owned())).to(BinaryOperation::GT as BinOpFun))
        .or(just(Token::Operator("<".to_owned())).to(BinaryOperation::ST as BinOpFun))
        .or(just(Token::Operator("==".to_owned())).to(BinaryOperation::Eq as BinOpFun))
        .or(just(Token::Operator("!=".to_owned())).to(BinaryOperation::NEq as BinOpFun))
        .or(just(Token::Operator(">=".to_owned())).to(BinaryOperation::GTOE as BinOpFun))
        .or(just(Token::Operator("<=".to_owned())).to(BinaryOperation::STOE as BinOpFun))
}

#[inline(always)]
pub(super) fn parser_bin_operation(
    expr: Recursive<Token, Spanned<Expression>, Simple<Token>>,
) -> impl Parser<Token, BinaryOperation, Error = Simple<Token>> + '_ {
    expr.clone()
        .then(parser_bin_operator())
        .then(expr)
        .map(|((lhs, op_fun), rhs)| op_fun(Box::new(lhs), Box::new(rhs)))
}

#[inline(always)]
pub(super) fn parser_un_operation(
    expr: Recursive<Token, Spanned<Expression>, Simple<Token>>,
) -> impl Parser<Token, UnaryOperation, Error = Simple<Token>> + '_ {
    just(Token::Operator("-".to_owned()))
        .to(UnaryOperation::Neg as UnOpFun)
        .or(just(Token::Operator("!".to_owned())).to(UnaryOperation::Not as UnOpFun))
        .then(expr)
        .map(|(op_fun, expr)| op_fun(Box::new(expr)))
}
