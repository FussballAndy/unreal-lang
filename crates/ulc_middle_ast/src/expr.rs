use ulc_ast::Lit;
use ulc_types::{errors::SyntaxError, token_kind::TokenKind, ULCType};

use crate::stmt::MiddleAstStatement;

pub type BoxedExpression = Box<MiddleAstExpression>;

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstExpression {
    Literal(Lit),
    Ident(usize),
    FunctionCall {
        function: String,
        args: Vec<(MiddleAstExpression, ULCType)>,
        ret_ty: ULCType,
    },
    BinaryOperation(MiddleAstBinaryOperation),
    UnaryOperation(MiddleAstUnaryOperation),
    IfExpr {
        condition: BoxedExpression,
        true_case: Vec<MiddleAstStatement>,
        false_case: Vec<MiddleAstStatement>,
        ret_ty: Option<ULCType>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstBinaryOperation {
    Add(BoxedExpression, BoxedExpression),
    Sub(BoxedExpression, BoxedExpression),
    Mul(BoxedExpression, BoxedExpression),
    Div(BoxedExpression, BoxedExpression),

    NEq(BoxedExpression, BoxedExpression),
    Eq(BoxedExpression, BoxedExpression),
    GT(BoxedExpression, BoxedExpression),
    ST(BoxedExpression, BoxedExpression),
    GTOE(BoxedExpression, BoxedExpression),
    STOE(BoxedExpression, BoxedExpression),

    And(BoxedExpression, BoxedExpression),
    Or(BoxedExpression, BoxedExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstUnaryOperation {
    Minus(BoxedExpression),
    Invert(BoxedExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl TryFrom<TokenKind> for UnaryOperator {
    type Error = SyntaxError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Not => Ok(Self::Not),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Or,
    And,
    Equals,
    NotEquals,
    SmallerThan,
    GreaterThan,
    SmallerEquals,
    GreaterEquals,
    Add,
    Minus,
    Multiply,
    Divide,
}

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = SyntaxError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Or => Ok(Self::Or),
            TokenKind::And => Ok(Self::And),
            TokenKind::Equals => Ok(Self::Equals),
            TokenKind::NotEquals => Ok(Self::NotEquals),
            TokenKind::SmallerThan => Ok(Self::SmallerThan),
            TokenKind::GreaterThan => Ok(Self::GreaterThan),
            TokenKind::SmallerEquals => Ok(Self::SmallerEquals),
            TokenKind::GreaterEquals => Ok(Self::GreaterEquals),
            TokenKind::Add => Ok(Self::Add),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Multiply => Ok(Self::Multiply),
            TokenKind::Divide => Ok(Self::Divide),
            _ => unreachable!(),
        }
    }
}
