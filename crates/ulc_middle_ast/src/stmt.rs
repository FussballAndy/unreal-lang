use ulc_types::ULCType;

use crate::expr::BoxedExpression;

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstStatement {
    Const {
        name: usize,
        const_type: ULCType,
        expr: BoxedExpression,
    },
    Let {
        name: usize,
        let_type: ULCType,
        expr: BoxedExpression,
    },
    Assignment {
        name: usize,
        expr: BoxedExpression,
    },
    ReturnStatement {
        ty: ULCType,
        expression: BoxedExpression,
    },
    UnusedExpression(BoxedExpression),
}
