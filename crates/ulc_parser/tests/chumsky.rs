#![recursion_limit = "32"]

use ulc_ast::{Expression, Function, Lit, Statement};
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
    println!("Running parser");
    let ChumskyParserRes {
        parsed_funcs,
        lexer_errors,
        parser_errors,
    } = chumsky_parser(test.trim());
    println!("Finished parser");
    match parsed_funcs {
        Some(func) => {
            assert_eq!(
                func[0].node,
                Function {
                    ident: Spanned::new(9..13, "main".to_owned()),
                    params: Vec::new(),
                    return_type: ULCType::Unit,
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
                                    function: Spanned::new(57..63, "println".to_owned()),
                                    args: vec![Spanned {
                                        span: (64..65).into(),
                                        node: Expression::Ident("a".to_owned()),
                                    }]
                                }
                            }))
                        }
                    ]
                }
            )
        }
        None => {
            lexer_errors
                .into_iter()
                .for_each(|e| println!("Lexer error: {}", e));
            parser_errors
                .into_iter()
                .for_each(|e| println!("Parser error: {}", e));
            assert!(false)
        }
    }
}
