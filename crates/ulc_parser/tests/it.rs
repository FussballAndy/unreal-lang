use ulc_ast::{types::ULCType, Expression, Function, Lit, Statement};
use ulc_parser::Parser;
use ulc_types::{token_kind::TokenKind, Spanned};

#[test]
fn global_function() {
    let test = r#"
        function main do
            let a: Int = 5;
            println(a);
        end
    "#;
    let mut parser = Parser::new(test);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: "main".to_owned(),
                    params: Vec::new(),
                    return_type: ULCType::Unit,
                    body: vec![
                        Spanned {
                            span: (42..52).into(),
                            node: Statement::Let {
                                name: "a".to_owned(),
                                let_type: ULCType::Int,
                                expr: Box::new(Spanned {
                                    span: (51..52).into(),
                                    node: Expression::Literal(Lit::Int(5)),
                                })
                            }
                        },
                        Spanned {
                            span: (66..76).into(),
                            node: Statement::UnusedExpression(Box::new(Spanned {
                                span: (66..76).into(),
                                node: Expression::FunctionCall {
                                    function: "println".to_owned(),
                                    args: vec![Spanned {
                                        span: (74..75).into(),
                                        node: Expression::Ident("a".to_owned()),
                                    }]
                                }
                            }))
                        }
                    ]
                })
            )
        }
        Err(err) => {
            err.display(test, "test.ul");
            assert!(false)
        }
    }
}

#[test]
fn block_return() {
    let test = r#"
        function main do
            5
        end
    "#;
    let mut parser = Parser::new(test);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: "main".to_owned(),
                    return_type: ULCType::Unit,
                    params: Vec::new(),
                    body: vec![Spanned {
                        span: (38..39).into(),
                        node: Statement::ReturnStatement {
                            expression: Box::new(Spanned {
                                span: (38..39).into(),
                                node: Expression::Literal(Lit::Int(5)),
                            })
                        }
                    }]
                })
            )
        }
        Err(err) => {
            err.display(test, "test.ul");
            assert!(false)
        }
    }
}

#[test]
fn if_expr() {
    let test = r#"
        function main do
            const a: Int = if 5 == 5 then 5 else 5 end;
        end
    "#;
    let mut parser = Parser::new(test);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: "main".to_owned(),
                    params: Vec::new(),
                    return_type: ULCType::Unit,
                    body: vec![Spanned {
                        span: (44..80).into(),
                        node: Statement::Const {
                            name: "a".to_owned(),
                            const_type: ULCType::Int,
                            expr: Box::new(Spanned {
                                span: (53..80).into(),
                                node: Expression::IfExpr {
                                    condition: Box::new(Spanned {
                                        span: (56..62).into(),
                                        node: Expression::BinaryOperation {
                                            op: TokenKind::Equals,
                                            lhs: Box::new(Spanned {
                                                span: (56..57).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            }),
                                            rhs: Box::new(Spanned {
                                                span: (61..62).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            }),
                                        },
                                    }),
                                    true_case: vec![Spanned {
                                        span: (68..69).into(),
                                        node: Statement::ReturnStatement {
                                            expression: Box::new(Spanned {
                                                span: (68..69).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            })
                                        },
                                    }],
                                    false_case: Some(vec![Spanned {
                                        span: (75..76).into(),
                                        node: Statement::ReturnStatement {
                                            expression: Box::new(Spanned {
                                                span: (75..76).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            })
                                        },
                                    },])
                                }
                            })
                        },
                    }]
                })
            )
        }
        Err(err) => {
            err.display(test, "test.ul");
            assert!(false)
        }
    }
}
