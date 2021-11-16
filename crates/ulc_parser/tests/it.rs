use ulc_ast::{types::Type, Expression, Function, Lit, Statement};
use ulc_checker::CheckerContext;
use ulc_parser::Parser;
use ulc_types::Spanned;

#[test]
fn global_function() {
    let test = r#"
        function main do
            let a: Int = 5;
            println(a);
        end
    "#;
    let mut checker = CheckerContext::default();
    let mut parser = Parser::new("test.ul", test, &mut checker);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: "main".to_owned(),
                    params: Vec::new(),
                    return_type: Type::Unit,
                    body: vec![
                        Spanned {
                            span: (42..52).into(),
                            node: Statement::Let {
                                name: "a".to_owned(),
                                let_type: Type::Int,
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
fn global_const() {
    let test = r#"const a: Int = 5;"#;
    let mut checker = CheckerContext::default();
    let mut parser = Parser::new("test.ul", test, &mut checker);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::Const {
                    name: "a".to_owned(),
                    const_type: Type::Int,
                    expr: Box::new(Spanned {
                        span: (15..16).into(),
                        node: Expression::Literal(Lit::Int(5)),
                    }),
                }
            );
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
    let mut checker = CheckerContext::default();
    let mut parser = Parser::new("test.ul", test, &mut checker);
    match parser.parse_global_statement() {
        Ok(stmt) => {
            assert_eq!(
                stmt.node,
                Statement::FunctionDefinition(Function {
                    ident: "main".to_owned(),
                    return_type: Type::Unit,
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
