use ulc_ast::{types::ULCType, BlockStatements, Expression, Function, Statement};
use ulc_types::{
    errors::{ParseResult, SyntaxError},
    token_kind::TokenKind,
};

use super::{Parser, Spanned};

impl Parser<'_> {
    pub fn parse_global_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Function => self.parse_function_def(),

            _ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "function".to_owned(),
                    token,
                })
            }
        }
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Let => self.parse_let(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Ident => {
                let ident_token = self.consume_next(TokenKind::Ident)?;
                let ident_text = self.text(ident_token);

                match self.peek() {
                    TokenKind::Assign => {
                        self.consume(TokenKind::Assign)?;
                        let expr = crate::validate_used_if_expression(self.expression()?)?;
                        Ok(Spanned {
                            span: (ident_token.span.start..expr.span.end).into(),
                            node: Statement::Assignment {
                                name: ident_text.to_owned(),
                                expr: Box::new(expr),
                            },
                        })
                    }
                    TokenKind::LeftParen => {
                        let expr = self
                            .parse_function_call(ident_text.to_owned(), ident_token.span.start)?;
                        Ok(Spanned {
                            span: expr.span,
                            node: Statement::UnusedExpression(Box::new(expr)),
                        })
                    }
                    _ => {
                        let expr = self.parse_expression_with_start(
                            0,
                            Spanned {
                                span: ident_token.span,
                                node: Expression::Ident(ident_text.to_owned()),
                            },
                        )?;
                        Ok(Spanned {
                            span: expr.span,
                            node: Statement::UnusedExpression(Box::new(expr)),
                        })
                    }
                }
            }

            _ => {
                let expr = self.boxed_expression()?;
                Ok(Spanned {
                    span: expr.span,
                    node: Statement::UnusedExpression(expr),
                })
            }
        }
    }

    fn parse_const(&mut self) -> ParseResult<Statement> {
        self.next_token()?;
        let ident_name = self.consume_next(TokenKind::Ident)?;

        let ident_text = self.text(ident_name);
        self.consume(TokenKind::Colon)?;
        let const_type = self.consume_type()?;

        self.consume(TokenKind::Assign)?;
        let expr = crate::validate_used_if_expression(self.expression()?)?;

        Ok(Spanned {
            span: (ident_name.span.start..expr.span.end).into(),
            node: Statement::Const {
                name: ident_text.to_owned(),
                const_type,
                expr: Box::new(expr),
            },
        })
    }

    fn parse_let(&mut self) -> ParseResult<Statement> {
        self.next_token()?;
        let ident_name = self.consume_next(TokenKind::Ident)?;

        let ident_text = self.text(ident_name);
        self.consume(TokenKind::Colon)?;
        let let_type = self.consume_type()?;
        self.consume(TokenKind::Assign)?;
        let expr = crate::validate_used_if_expression(self.expression()?)?;

        Ok(Spanned {
            span: (ident_name.span.start..expr.span.end).into(),
            node: Statement::Let {
                name: ident_text.to_owned(),
                let_type,
                expr: Box::new(expr),
            },
        })
    }

    pub fn parse_function_def(&mut self) -> ParseResult<Statement> {
        self.next_token()?;
        let ident = self.consume_next(TokenKind::Ident)?;

        let text = self.text(ident);
        let mut params = Vec::new();
        if self.at(TokenKind::LeftParen) {
            self.consume(TokenKind::LeftParen)?;
            while !self.at(TokenKind::RightParen) {
                if !params.is_empty() && self.at(TokenKind::Comma) {
                    self.consume(TokenKind::Comma)?;
                }
                let ident = self.next_token()?;
                let ident_text = self.text(ident);
                self.consume(TokenKind::Colon)?;
                let param_type = self.consume_type()?;
                params.push((ident_text.to_owned(), param_type));
            }
            self.consume(TokenKind::RightParen)?;
        }

        let return_type = if self.at(TokenKind::Colon) {
            self.consume(TokenKind::Colon)?;
            self.consume_type()?
        } else {
            ULCType::Unit
        };
        let body = self.parse_function_block_statements()?;
        Ok(Spanned {
            span: (ident.span.start..body.span.end).into(),
            node: Statement::FunctionDefinition(Function {
                ident: text.to_owned(),
                return_type,
                params,
                body: body.node.0,
            }),
        })
    }

    fn parse_function_block_statements(&mut self) -> ParseResult<BlockStatements> {
        let token = self.consume_next(TokenKind::Do)?;
        let mut statements = Vec::new();
        while !self.at(TokenKind::End) {
            let stmt = self.parse_statement()?;
            if !self.at(TokenKind::Semicolon) {
                if let Statement::UnusedExpression(e) = stmt.node {
                    statements.push(Spanned {
                        node: Statement::ReturnStatement { expression: e },
                        span: stmt.span,
                    });
                } else {
                    statements.push(stmt);
                }
                break;
            } else {
                statements.push(stmt);
            }
            self.consume(TokenKind::Semicolon)?;
        }
        let end = self.consume_next(TokenKind::End)?;
        Ok(Spanned {
            node: BlockStatements(statements),
            span: (token.span.start..end.span.end).into(),
        })
    }
}
