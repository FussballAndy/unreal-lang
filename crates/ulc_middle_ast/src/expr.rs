use ulc_ast::Lit;
use ulc_types::{errors::SyntaxError, token_kind::TokenKind, Spanned};

use crate::stmt::MiddleAstStatement;

pub type BoxedExpression = Box<Spanned<MiddleAstExpression>>;
pub type Expressions = Vec<Spanned<MiddleAstExpression>>;

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstExpression {
    Literal(Lit),
    Ident(u32),
    FunctionCall {
        function: String,
        args: Expressions,
    },
    BinaryOperation {
        op: BinaryOperator,
        lhs: BoxedExpression,
        rhs: BoxedExpression,
    },
    UnaryOperation {
        op: UnaryOperator,
        expr: BoxedExpression,
    },
    IfExpr {
        condition: BoxedExpression,
        true_case: Vec<Spanned<MiddleAstStatement>>,
        false_case: Option<Vec<Spanned<MiddleAstStatement>>>,
    },
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
