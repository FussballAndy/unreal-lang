use ulc_ast::{Expression, Function, Lit, Statement, TopLevelStatement};
use ulc_parser::{chumsky_parser, ChumskyParserRes};
use ulc_types::{Spanned, ULCType};

#[test]
fn global_func() {
    let test = r#"
        function main do
            let a: Int = 5;
            println(a);
        end
    "#;
    let ChumskyParserRes {
        parsed_funcs,
        lexer_errors,
        parser_errors,
    } = chumsky_parser(test.trim());
    match parsed_funcs {
        Some(func) => {
            assert_eq!(
                func[0].node,
                TopLevelStatement::FunctionDefinition(Function {
                    ident: Spanned::new(9..13, "main".to_owned()),
                    params: Vec::new(),
                    return_type: Spanned::new(16..19, ULCType::Unit),
                    body: vec![
                        Spanned {
                            span: (29..43).into(),
                            node: Statement::Let {
                                name: Spanned::new(33..34, "a".to_owned()),
                                let_type: ULCType::Int,
                                expr: Box::new(Spanned {
                                    span: (42..43).into(),
                                    node: Expression::Literal(Lit::Int(5)),
                                })
                            }
                        },
                        Spanned {
                            span: (57..67).into(),
                            node: Statement::UnusedExpression(Box::new(Spanned {
                                span: (57..67).into(),
                                node: Expression::FunctionCall {
                                    module: None,
                                    function: Spanned::new(57..64, "println".to_owned()),
                                    args: vec![Spanned {
                                        span: (65..66).into(),
                                        node: Expression::Ident("a".to_owned()),
                                    }]
                                }
                            }))
                        }
                    ]
                })
            )
        }
        None => {
            lexer_errors
                .into_iter()
                .for_each(|e| println!("Lexer error: {}", e));
            parser_errors
                .into_iter()
                .for_each(|e| e.display(test, "test.ul"));
            assert!(false)
        }
    }
}
