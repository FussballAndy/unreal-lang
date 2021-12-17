use crate::BoxedExpression;

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperation {
    // Math
    /// lhs + rhs
    Add(BoxedExpression, BoxedExpression),
    /// lhs - rhs
    Sub(BoxedExpression, BoxedExpression),
    /// lhs * rhs
    Mul(BoxedExpression, BoxedExpression),
    /// lhs / rhs
    Div(BoxedExpression, BoxedExpression),

    // Comp
    /// lhs != rhs
    NEq(BoxedExpression, BoxedExpression),
    /// lhs == rhs
    Eq(BoxedExpression, BoxedExpression),
    /// lhs > rhs
    GT(BoxedExpression, BoxedExpression),
    /// lhs < rhs
    ST(BoxedExpression, BoxedExpression),
    /// lhs >= rhs
    GTOE(BoxedExpression, BoxedExpression),
    /// lhs <= rhs
    STOE(BoxedExpression, BoxedExpression),

    // Comb
    /// lhs && rhs
    And(BoxedExpression, BoxedExpression),
    /// lhs || rhs
    Or(BoxedExpression, BoxedExpression),
}

impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperation::Add(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            BinaryOperation::Sub(lhs, rhs) => write!(f, "{} - {}", lhs, rhs),
            BinaryOperation::Mul(lhs, rhs) => write!(f, "{} * {}", lhs, rhs),
            BinaryOperation::Div(lhs, rhs) => write!(f, "{} / {}", lhs, rhs),
            BinaryOperation::NEq(lhs, rhs) => write!(f, "{} != {}", lhs, rhs),
            BinaryOperation::Eq(lhs, rhs) => write!(f, "{} == {}", lhs, rhs),
            BinaryOperation::GT(lhs, rhs) => write!(f, "{} > {}", lhs, rhs),
            BinaryOperation::ST(lhs, rhs) => write!(f, "{} < {}", lhs, rhs),
            BinaryOperation::GTOE(lhs, rhs) => write!(f, "{} >= {}", lhs, rhs),
            BinaryOperation::STOE(lhs, rhs) => write!(f, "{} <= {}", lhs, rhs),
            BinaryOperation::And(lhs, rhs) => write!(f, "{} and {}", lhs, rhs),
            BinaryOperation::Or(lhs, rhs) => write!(f, "{} or {}", lhs, rhs),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperation {
    /// -num
    Neg(BoxedExpression),
    /// !bool
    Not(BoxedExpression),
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Neg(expr) => write!(f, "-{}", expr),
            UnaryOperation::Not(expr) => write!(f, "!{}", expr),
        }
    }
}
