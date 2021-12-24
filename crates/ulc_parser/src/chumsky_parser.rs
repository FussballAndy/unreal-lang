use chumsky::{prelude::*, Error, Stream};
use ulc_ast::{
    BinaryOperation, BoxedExpression, Expression, Function, Statement, TopLevelStatement,
    UnaryOperation,
};
use ulc_types::{errors::SyntaxError, Spanned, Token, ULCType};

mod lexer;
mod literal;

type BinOpFun = fn(BoxedExpression, BoxedExpression) -> BinaryOperation;
type UnOpFun = fn(BoxedExpression) -> UnaryOperation;

#[inline(always)]
fn parser_raw_expr(
    expr: Recursive<'static, Token, Spanned<Expression>, SyntaxError>,
    stmt_list: Recursive<'static, Token, Vec<Spanned<Statement>>, SyntaxError>,
) -> impl Parser<Token, Spanned<Expression>, Error = SyntaxError> + Clone {
    recursive(|_| {
        let expr_ident = spanned_ident()
            .then(
                just(Token::Ctrl(':'))
                    .repeated()
                    .exactly(2)
                    .ignore_then(spanned_ident())
                    .or_not()
                    .then(
                        expr.clone()
                            .separated_by(just(Token::Ctrl(',')))
                            .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
                    )
                    .or_not(),
            )
            .map_with_span(|(idt, func_call), span| {
                if let Some((from_mod, args)) = func_call {
                    if let Some(fn_in_mod) = from_mod {
                        Spanned {
                            node: Expression::FunctionCall {
                                args,
                                module: Some(idt),
                                function: fn_in_mod,
                            },
                            span: span.into(),
                        }
                    } else {
                        Spanned {
                            node: Expression::FunctionCall {
                                args,
                                module: None,
                                function: idt,
                            },
                            span: span.into(),
                        }
                    }
                } else {
                    Spanned {
                        node: Expression::Ident(idt.node),
                        span: span.into(),
                    }
                }
            });

        let expr_lit = literal::parser_literal().map_with_span(|lit, span| Spanned {
            node: Expression::Literal(lit),
            span: span.into(),
        });

        let expr_if = just(Token::If)
            .ignore_then(expr)
            .then_ignore(just(Token::Then))
            .then(stmt_list.clone())
            .then(just(Token::Else).ignore_then(stmt_list).or_not())
            .then_ignore(just(Token::End))
            .map_with_span(|((cond, true_case), false_case), span| Spanned {
                node: Expression::IfExpr {
                    condition: Box::new(cond),
                    true_case,
                    false_case,
                },
                span: span.into(),
            });

        expr_if.or(expr_lit).or(expr_ident)
    })
}

#[inline(always)]
fn parser_expr(
    stmt_list: Recursive<'static, Token, Vec<Spanned<Statement>>, SyntaxError>,
) -> impl Parser<Token, Spanned<Expression>, Error = SyntaxError> + Clone {
    recursive(|expr| {
        let raw_expr = parser_raw_expr(expr, stmt_list);

        let unary = just(Token::Operator("-".to_owned()))
            .to(UnaryOperation::Neg as UnOpFun)
            .or(just(Token::Operator("!".to_owned())).to(UnaryOperation::Not as UnOpFun))
            .repeated()
            .then(raw_expr.clone())
            .foldr(|op_fun: UnOpFun, exp: Spanned<Expression>| {
                let span = exp.span;
                Spanned {
                    node: Expression::UnaryOperation(op_fun(Box::new(exp))),
                    span,
                }
            });

        let prd_op = just(Token::Operator("*".to_owned()))
            .to(BinaryOperation::Mul as BinOpFun)
            .or(just(Token::Operator("/".to_owned())).to(BinaryOperation::Div as BinOpFun));
        let product = raw_expr
            .clone()
            .then(prd_op.then(raw_expr).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            });

        let pr_or_un = product.or(unary);

        let sum_op = just(Token::Operator("+".to_owned()))
            .to(BinaryOperation::Add as BinOpFun)
            .or(just(Token::Operator("-".to_owned())).to(BinaryOperation::Sub as BinOpFun))
            .or(just(Token::Operator("and".to_owned())).to(BinaryOperation::And as BinOpFun))
            .or(just(Token::Operator("or".to_owned())).to(BinaryOperation::Or as BinOpFun));
        let sum = pr_or_un
            .clone()
            .then(sum_op.then(pr_or_un).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            });

        // compare
        let cmp_ops = just(Token::Operator("==".to_owned()))
            .to(BinaryOperation::Eq as BinOpFun)
            .or(just(Token::Operator("!=".to_owned())).to(BinaryOperation::NEq as BinOpFun))
            .or(just(Token::Operator(">".to_owned())).to(BinaryOperation::GT as BinOpFun))
            .or(just(Token::Operator("<".to_owned())).to(BinaryOperation::ST as BinOpFun))
            .or(just(Token::Operator(">=".to_owned())).to(BinaryOperation::GTOE as BinOpFun))
            .or(just(Token::Operator("<=".to_owned())).to(BinaryOperation::STOE as BinOpFun));
        sum.clone()
            .then(cmp_ops.then(sum).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            })
    })
}

fn parser_stmt_list() -> impl Parser<Token, Vec<Spanned<Statement>>, Error = SyntaxError> {
    recursive(|stmt_list| {
        // marked as recursive cuz I don't know any other way to get `.clone()` to work.
        let stmt = recursive(|_| {
            let expr = parser_expr(stmt_list);

            let stmt_let = just(Token::Let)
                .ignore_then(spanned_ident())
                .then(just(Token::Ctrl(':')).ignore_then(parser_type()))
                .then_ignore(just(Token::Operator("=".to_owned())))
                .then(expr.clone())
                .map_with_span(|((name, let_type), val), span| Spanned {
                    node: Statement::Let {
                        name,
                        let_type,
                        expr: Box::new(val),
                    },
                    span: span.into(),
                });

            let stmt_const = just(Token::Const)
                .ignore_then(spanned_ident())
                .then(just(Token::Ctrl(':')).ignore_then(parser_type()))
                .then_ignore(just(Token::Operator("=".to_owned())))
                .then(expr.clone())
                .map_with_span(|((name, const_type), val), span| Spanned {
                    node: Statement::Const {
                        name,
                        const_type,
                        expr: Box::new(val),
                    },
                    span: span.into(),
                });

            let stmt_ass = spanned_ident()
                .then_ignore(just(Token::Operator("=".to_owned())))
                .then(expr.clone())
                .map_with_span(|(name, val), span| Spanned {
                    node: Statement::Assignment {
                        name,
                        expr: Box::new(val),
                    },
                    span: span.into(),
                });

            stmt_let
                .or(stmt_const)
                .or(stmt_ass)
                .or(expr.map_with_span(|exp, span| Spanned {
                    node: Statement::UnusedExpression(Box::new(exp)),
                    span: span.into(),
                }))
        });

        stmt.clone()
            .then(just(Token::Ctrl(';')).ignore_then(stmt.or_not()).repeated())
            .map(|(left, mut right)| {
                let mut result = Vec::new();
                if right.is_empty() {
                    result.push(match left {
                        Spanned {
                            node: Statement::UnusedExpression(expr),
                            span,
                        } => Spanned {
                            node: Statement::ReturnStatement { expression: expr },
                            span,
                        },
                        a => a,
                    })
                } else {
                    result.push(left);
                    let last = right.pop();
                    result.append(&mut right.into_iter().flatten().collect());

                    if let Some(Some(st)) = last {
                        let stm = match st.node {
                            Statement::UnusedExpression(expr) => {
                                Statement::ReturnStatement { expression: expr }
                            }
                            o => o,
                        };
                        result.push(Spanned {
                            node: stm,
                            span: st.span,
                        })
                    }
                }
                result
            })
    })
}

pub fn top_level_statement(
) -> impl Parser<Token, Vec<Spanned<TopLevelStatement>>, Error = SyntaxError> {
    let func = just(Token::Func)
        .ignore_then(spanned_ident())
        .then(
            parser_ident_and_type()
                .separated_by(just(Token::Ctrl(',')))
                .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                .or_not()
                .map(|idts| idts.unwrap_or_else(Vec::new)),
        )
        .then(
            just(Token::Ctrl(':'))
                .ignore_then(parser_type())
                .or_not()
                .map_with_span(|ty, span| Spanned {
                    node: ty.unwrap_or(ULCType::Unit),
                    span: span.into(),
                }),
        )
        .then(parser_stmt_list().delimited_by(Token::Do, Token::End))
        .map_with_span(|(((name_idt, params), return_type), stmts), span| Spanned {
            node: TopLevelStatement::FunctionDefinition(Function {
                ident: name_idt,
                params,
                body: stmts,
                return_type,
            }),
            span: span.into(),
        });

    let import = just(Token::Import)
        .ignore_then(spanned_ident())
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|file, span| Spanned {
            node: TopLevelStatement::Import(file),
            span: span.into(),
        });

    func.or(import).repeated().then_ignore(end())
}

#[inline(always)]
fn parser_type() -> impl Parser<Token, ULCType, Error = SyntaxError> + Clone {
    just(Token::Type("Int"))
        .to(ULCType::Int)
        .or(just(Token::Type("Bool")).to(ULCType::Bool))
        .or(just(Token::Type("String")).to(ULCType::String))
}

fn spanned_ident() -> impl Parser<Token, Spanned<String>, Error = SyntaxError> {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(idt) => Ok(idt),
        _ => Err(SyntaxError::expected_input_found(
            span,
            Vec::new(),
            Some(tok),
        )),
    });
    ident.map_with_span(|idt, span| Spanned {
        node: idt,
        span: span.into(),
    })
}

fn parser_ident_and_type() -> impl Parser<Token, Spanned<(String, ULCType)>, Error = SyntaxError> {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(idt) => Ok(idt),
        _ => Err(SyntaxError::expected_input_found(
            span,
            Vec::new(),
            Some(tok),
        )),
    });

    ident
        .then(just(Token::Ctrl(':')).ignore_then(parser_type()))
        .map_with_span(|stuff, span| Spanned {
            node: stuff,
            span: span.into(),
        })
}

pub struct ChumskyParserRes {
    pub parsed_funcs: Option<Vec<Spanned<TopLevelStatement>>>,
    pub lexer_errors: Vec<Simple<char>>,
    pub parser_errors: Vec<SyntaxError>,
}

pub fn chumsky_parser(src: &str) -> ChumskyParserRes {
    let (tokens, errs) = lexer::lexer().parse_recovery(src);
    if let Some(tokens) = tokens {
        let len = src.chars().count();
        // Moving parser to seperate thread because of bigger stack size
        let (res, err) = top_level_statement()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
        ChumskyParserRes {
            parsed_funcs: res,
            lexer_errors: Vec::new(),
            parser_errors: err,
        }
    } else {
        ChumskyParserRes {
            parsed_funcs: None,
            lexer_errors: errs,
            parser_errors: Vec::new(),
        }
    }
}
