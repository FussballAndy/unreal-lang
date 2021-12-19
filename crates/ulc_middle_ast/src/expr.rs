use ulc_ast::Lit;
use ulc_types::ULCType;

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
