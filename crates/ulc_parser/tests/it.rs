use ulc_ast::{BinaryOperation, Expression, Function, Lit, Statement};
use ulc_parser::Parser;
use ulc_types::{Spanned, ULCType};

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
                    ident: Spanned::new(18..22, "main".to_owned()),
                    params: Vec::new(),
                    return_type: ULCType::Unit,
                    body: vec![
                        Spanned {
                            span: (42..52).into(),
                            node: Statement::Let {
                                name: Spanned::new(42..43, "a".to_owned()),
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
                                    function: Spanned::new(66..73, "println".to_owned()),
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
                    ident: Spanned::new(18..22, "main".to_owned()),
                    params: Vec::new(),
                    return_type: ULCType::Unit,
                    body: vec![Spanned {
                        span: (44..80).into(),
                        node: Statement::Const {
                            name: Spanned::new(44..45, "a".to_owned()),
                            const_type: ULCType::Int,
                            expr: Box::new(Spanned {
                                span: (53..80).into(),
                                node: Expression::IfExpr {
                                    condition: Box::new(Spanned {
                                        span: (56..62).into(),
                                        node: Expression::BinaryOperation(BinaryOperation::Eq(
                                            Box::new(Spanned {
                                                span: (56..57).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            }),
                                            Box::new(Spanned {
                                                span: (61..62).into(),
                                                node: Expression::Literal(Lit::Int(5)),
                                            }),
                                        )),
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

#[test]
fn bigger_if_expr() {
    let test = r#"
        function main(argv: Int, argc: Int) do
            if true then
                puts("Fuck!");
            else
                puts("Fuck!");
            end;
            puts("Hello World!")
        end
    "#;
    let mut parser = Parser::new(test);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: Spanned::new(18..22, "main".to_owned()),
                    params: vec![
                        ("argv".to_owned(), ULCType::Int),
                        ("argc".to_owned(), ULCType::Int)
                    ],
                    return_type: ULCType::Unit,
                    body: vec![
                        Spanned {
                            span: (60..167).into(),
                            node: Statement::UnusedExpression(Box::new(Spanned {
                                span: (60..167).into(),
                                node: Expression::IfExpr {
                                    condition: Box::new(Spanned {
                                        span: (63..67).into(),
                                        node: Expression::Literal(Lit::Bool(true)),
                                    }),
                                    true_case: vec![Spanned {
                                        span: (89..102).into(),
                                        node: Statement::UnusedExpression(Box::new(Spanned {
                                            span: (89..102).into(),
                                            node: Expression::FunctionCall {
                                                function: Spanned::new(89..93, "puts".to_owned()),
                                                args: vec![Spanned {
                                                    span: (94..101).into(),
                                                    node: Expression::Literal(Lit::String(
                                                        "Fuck!".to_owned()
                                                    ))
                                                }]
                                            }
                                        }))
                                    }],
                                    false_case: Some(vec![Spanned {
                                        span: (137..150).into(),
                                        node: Statement::UnusedExpression(Box::new(Spanned {
                                            span: (137..150).into(),
                                            node: Expression::FunctionCall {
                                                function: Spanned::new(137..141, "puts".to_owned()),
                                                args: vec![Spanned {
                                                    span: (142..149).into(),
                                                    node: Expression::Literal(Lit::String(
                                                        "Fuck!".to_owned()
                                                    ))
                                                }]
                                            }
                                        }))
                                    }])
                                }
                            }))
                        },
                        Spanned {
                            span: (181..201).into(),
                            node: Statement::UnusedExpression(Box::new(Spanned {
                                span: (181..201).into(),
                                node: Expression::FunctionCall {
                                    function: Spanned::new(181..185, "puts".to_owned()),
                                    args: vec![Spanned {
                                        span: (186..200).into(),
                                        node: Expression::Literal(Lit::String(
                                            "Hello World!".to_owned()
                                        ))
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
