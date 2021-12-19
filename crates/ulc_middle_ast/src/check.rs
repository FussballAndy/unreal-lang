use ulc_ast::{Expression, Statement};
use ulc_types::{
    errors::{ParseResult, SyntaxError},
    Spanned,
};

pub fn validate_used_if_expression(expr: Spanned<Expression>) -> ParseResult<Expression> {
    if let Expression::IfExpr {
        true_case,
        false_case,
        ..
    } = &expr.node
    {
        if let Some(a) = true_case.last() {
            if let Statement::ReturnStatement { .. } = a.node {
                if let Some(fl_c) = false_case {
                    if let Some(d) = fl_c.last() {
                        if let Statement::ReturnStatement { .. } = d.node {
                            Ok(expr)
                        } else {
                            Err(SyntaxError::InvalidIfExpression {
                                span: expr.span,
                                sp_msg: Spanned {
                                    span: d.span,
                                    node:
                                        "Expected return statement! Perhaps remove the semicolon.",
                                },
                            })
                        }
                    } else {
                        Err(SyntaxError::InvalidIfExpression {
                            span: expr.span,
                            sp_msg: Spanned {
                                span: expr.span,
                                node: "False case is missing return statement!",
                            },
                        })
                    }
                } else {
                    Err(SyntaxError::InvalidIfExpression {
                        span: expr.span,
                        sp_msg: Spanned {
                            span: expr.span,
                            node: "Missing false case!",
                        },
                    })
                }
            } else {
                Err(SyntaxError::InvalidIfExpression {
                    span: expr.span,
                    sp_msg: Spanned {
                        span: a.span,
                        node: "Expected return statement! Perhaps remove the semicolon.",
                    },
                })
            }
        } else {
            Err(SyntaxError::InvalidIfExpression {
                span: expr.span,
                sp_msg: Spanned {
                    span: expr.span,
                    node: "True case is missing return statement!",
                },
            })
        }
    } else {
        Ok(expr)
    }
}
