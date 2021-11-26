use ulc_types::ULCType;

use crate::expr::BoxedExpression;

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleAstStatement {
    Const {
        name: u32,
        const_type: ULCType,
        expr: BoxedExpression,
    },
    Let {
        name: u32,
        let_type: ULCType,
        expr: BoxedExpression,
    },
    Assignment {
        name: u32,
        expr: BoxedExpression,
    },
    ReturnStatement {
        ty: ULCType,
        expression: BoxedExpression,
    },
    UnusedExpression(BoxedExpression),
}
