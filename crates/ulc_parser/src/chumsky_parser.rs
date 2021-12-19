use chumsky::{prelude::*, Error, Stream};
use ulc_ast::{BinaryOperation, BoxedExpression, Expression, Function, Statement, UnaryOperation};
use ulc_types::{Spanned, ULCType};

mod lexer;
pub use lexer::Token;
mod literal;

type Span = std::ops::Range<usize>;

type BinOpFun = fn(BoxedExpression, BoxedExpression) -> BinaryOperation;
type UnOpFun = fn(BoxedExpression) -> UnaryOperation;

#[inline(always)]
fn parser_raw_expr(
    expr: Recursive<'static, Token, Spanned<Expression>, Simple<Token>>,
    stmt_list: Recursive<'static, Token, Vec<Spanned<Statement>>, Simple<Token>>,
) -> impl Parser<Token, Spanned<Expression>, Error = Simple<Token>> + Clone {
    recursive(|raw_expr| {
        let expr_ident = spanned_ident()
            .then(
                expr.clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                    .or_not(),
            )
            .map_with_span(|(idt, exprs), span: Span| {
                if let Some(args) = exprs {
                    Spanned {
                        node: Expression::FunctionCall {
                            args,
                            function: idt,
                        },
                        span: span.into(),
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

        let expr_un_op = just(Token::Operator("-".to_owned()))
            .to(UnaryOperation::Neg as UnOpFun)
            .or(just(Token::Operator("!".to_owned())).to(UnaryOperation::Not as UnOpFun))
            .repeated()
            .then(raw_expr)
            .foldr(|op_fun: UnOpFun, exp: Spanned<Expression>| {
                let span = exp.span;
                Spanned {
                    node: Expression::UnaryOperation(op_fun(Box::new(exp))),
                    span,
                }
            });

        expr_if.or(expr_lit).or(expr_un_op).or(expr_ident)
    })
}

#[inline(always)]
fn parser_expr(
    stmt_list: Recursive<'static, Token, Vec<Spanned<Statement>>, Simple<Token>>,
) -> impl Parser<Token, Spanned<Expression>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = parser_raw_expr(expr, stmt_list);

        let prd_op = just(Token::Operator("*".to_owned()))
            .to(BinaryOperation::Mul as BinOpFun)
            .or(just(Token::Operator("/".to_owned())).to(BinaryOperation::Div as BinOpFun));
        let product = raw_expr
            .clone()
            .then(prd_op.then(raw_expr.clone()).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            });

        let sum_op = just(Token::Operator("+".to_owned()))
            .to(BinaryOperation::Add as BinOpFun)
            .or(just(Token::Operator("-".to_owned())).to(BinaryOperation::Sub as BinOpFun))
            .or(just(Token::Operator("and".to_owned())).to(BinaryOperation::And as BinOpFun))
            .or(just(Token::Operator("or".to_owned())).to(BinaryOperation::Or as BinOpFun));
        let sum = product
            .clone()
            .then(sum_op.then(product).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            });

        let cmp_ops = just(Token::Operator("==".to_owned()))
            .to(BinaryOperation::Eq as BinOpFun)
            .or(just(Token::Operator("!=".to_owned())).to(BinaryOperation::NEq as BinOpFun))
            .or(just(Token::Operator(">".to_owned())).to(BinaryOperation::GT as BinOpFun))
            .or(just(Token::Operator("<".to_owned())).to(BinaryOperation::ST as BinOpFun))
            .or(just(Token::Operator(">=".to_owned())).to(BinaryOperation::GTOE as BinOpFun))
            .or(just(Token::Operator("<=".to_owned())).to(BinaryOperation::STOE as BinOpFun));
        let compare = sum
            .clone()
            .then(cmp_ops.then(sum).repeated())
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.span.start..rhs.span.end).into();
                Spanned {
                    node: Expression::BinaryOperation(op(Box::new(lhs), Box::new(rhs))),
                    span,
                }
            });

        compare.or(raw_expr)
    })
}

fn parser_stmt_list() -> impl Parser<Token, Vec<Spanned<Statement>>, Error = Simple<Token>> {
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
            .map(|(left, right)| {
                let mut result = vec![left];
                for a in right.into_iter().flatten() {
                    result.push(a);
                }
                if let Some(last) = result.pop() {
                    if let Statement::UnusedExpression(expr) = last.node {
                        result.push(Spanned {
                            node: Statement::ReturnStatement { expression: expr },
                            span: last.span,
                        });
                    } else {
                        result.push(last);
                    }
                }
                result
            })
    })
}

pub fn parser_func() -> impl Parser<Token, Vec<Spanned<Function>>, Error = Simple<Token>> {
    just(Token::Func)
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
                .map(|ty| ty.unwrap_or(ULCType::Unit)),
        )
        .then(parser_stmt_list().delimited_by(Token::Do, Token::End))
        .map_with_span(|(((name_idt, params), return_type), stmts), span| Spanned {
            node: Function {
                ident: name_idt,
                params,
                body: stmts,
                return_type,
            },
            span: span.into(),
        })
        .repeated()
        .then_ignore(end())
}

#[inline(always)]
fn parser_type() -> impl Parser<Token, ULCType, Error = Simple<Token>> + Clone {
    just(Token::Type("Int"))
        .to(ULCType::Int)
        .or(just(Token::Type("Bool")).to(ULCType::Bool))
        .or(just(Token::Type("String")).to(ULCType::String))
}

fn spanned_ident() -> impl Parser<Token, Spanned<String>, Error = Simple<Token>> {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(idt) => Ok(idt),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    ident.map_with_span(|idt, span: Span| Spanned {
        node: idt,
        span: span.into(),
    })
}

fn parser_ident_and_type() -> impl Parser<Token, (String, ULCType), Error = Simple<Token>> {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(idt) => Ok(idt),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    ident.then(just(Token::Ctrl(':')).ignore_then(parser_type()))
}

pub struct ChumskyParserRes {
    pub parsed_funcs: Option<Vec<Spanned<Function>>>,
    pub lexer_errors: Vec<Simple<char>>,
    pub parser_errors: Vec<Simple<Token>>,
}

pub fn chumsky_parser(src: &str) -> ChumskyParserRes {
    println!("Lexer...");
    let (tokens, errs) = lexer::lexer().parse_recovery(src);
    println!("Done");
    if let Some(tokens) = tokens {
        let len = src.chars().count();
        println!("Parser...");
        // Moving parser to seperate thread because of bigger stack size
        let (res, err) =
            parser_func().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
        println!("Done");
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
