use chumsky::{prelude::*, Error, Stream};
use ulc_ast::{Expression, Function, Statement};
use ulc_types::{Spanned, ULCType};

mod lexer;
pub use lexer::Token;
mod literal;
mod operations;

type Span = std::ops::Range<usize>;

pub fn parser_expr(
    stmt_list: Recursive<Token, Vec<Spanned<Statement>>, Simple<Token>>,
) -> impl Parser<Token, Spanned<Expression>, Error = Simple<Token>> + Clone + '_ {
    recursive(|expr| {
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

        let expr_bin_op =
            operations::parser_bin_operation(expr.clone()).map_with_span(|bin_op, span| Spanned {
                node: Expression::BinaryOperation(bin_op),
                span: span.into(),
            });

        let expr_un_op =
            operations::parser_un_operation(expr.clone()).map_with_span(|bin_op, span| Spanned {
                node: Expression::UnaryOperation(bin_op),
                span: span.into(),
            });

        let expr_operations = expr_bin_op.or(expr_un_op);

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

        expr_ident.or(expr_lit).or(expr_operations).or(expr_if)
    })
}

#[inline(always)]
fn parser_type() -> impl Parser<Token, ULCType, Error = Simple<Token>> + Clone {
    just(Token::Type("Int"))
        .to(ULCType::Int)
        .or(just(Token::Type("Bool")).to(ULCType::Bool))
        .or(just(Token::Type("String")).to(ULCType::String))
}

fn parser_stmt(
    stmt_list: Recursive<Token, Vec<Spanned<Statement>>, Simple<Token>>,
) -> impl Parser<Token, Spanned<Statement>, Error = Simple<Token>> + '_ {
    let parser_exp = parser_expr(stmt_list);

    let stmt_let = just(Token::Let)
        .ignore_then(spanned_ident())
        .then_ignore(just(Token::Ctrl(':')))
        .then(parser_type())
        .then_ignore(just(Token::Operator("=".to_owned())))
        .then(parser_exp.clone())
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
        .then_ignore(just(Token::Ctrl(':')))
        .then(parser_type())
        .then_ignore(just(Token::Operator("=".to_owned())))
        .then(parser_exp.clone())
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
        .then(parser_exp.clone())
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
        .or(parser_exp.map_with_span(|expr, span| Spanned {
            node: Statement::UnusedExpression(Box::new(expr)),
            span: span.into(),
        }))
}
fn parser_ident_and_type() -> impl Parser<Token, (String, ULCType), Error = Simple<Token>> {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(idt) => Ok(idt),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    ident
        .then_ignore(just(Token::Ctrl(':')))
        .then(parser_type())
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

fn parser_stmt_list() -> impl Parser<Token, Vec<Spanned<Statement>>, Error = Simple<Token>> {
    recursive(|stmt_list| {
        parser_stmt(stmt_list.clone())
            .then_ignore(just(Token::Ctrl(';')))
            .repeated()
            .then(parser_stmt(stmt_list).or_not())
            .map(|(mut stmts, st)| {
                if let Some(sta) = st {
                    stmts.push(sta);
                }
                stmts
            })
    })
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
