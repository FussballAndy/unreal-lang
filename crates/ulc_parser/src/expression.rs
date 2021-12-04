use ulc_ast::{BoxedExpression, Expression, Lit, Statement};
use ulc_types::{
    errors::{ParseResult, SyntaxError},
    token_kind::TokenKind,
};

use super::{Parser, Spanned, SyntaxResult};

trait Operator {
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Minus => ((), 51),
            TokenKind::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            TokenKind::Or => (1, 2),
            TokenKind::And => (3, 4),
            TokenKind::Equals | TokenKind::NotEquals => (5, 6),
            TokenKind::SmallerThan
            | TokenKind::GreaterThan
            | TokenKind::SmallerEquals
            | TokenKind::GreaterEquals => (7, 8),
            TokenKind::Add | TokenKind::Minus => (9, 10),
            TokenKind::Multiply | TokenKind::Divide => (11, 12),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        None
    }
}

impl Parser<'_> {
    fn parse_expression(&mut self, binding_power: u8) -> ParseResult<Expression> {
        let lhs = match self.peek() {
            lit @ TokenKind::Unit
            | lit @ TokenKind::True
            | lit @ TokenKind::False
            | lit @ TokenKind::IntLit
            | lit @ TokenKind::StringLit => self.parse_lit(lit)?,

            TokenKind::Ident => self.parse_ident()?,
            TokenKind::If => self.parse_if_expression()?,

            _ => {
                let token = self.next_token()?;
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_owned(),
                    token,
                });
            }
        };
        self.parse_expression_with_start(binding_power, lhs)
    }

    pub fn parse_expression_with_start(
        &mut self,
        binding_power: u8,
        start: Spanned<Expression>,
    ) -> ParseResult<Expression> {
        let mut left_hand_side = start;

        loop {
            let op = match self.peek() {
                op @ TokenKind::Add
                | op @ TokenKind::Minus
                | op @ TokenKind::Multiply
                | op @ TokenKind::Divide
                | op @ TokenKind::Equals
                | op @ TokenKind::NotEquals
                | op @ TokenKind::GreaterThan
                | op @ TokenKind::SmallerThan
                | op @ TokenKind::GreaterEquals
                | op @ TokenKind::SmallerEquals
                | op @ TokenKind::And
                | op @ TokenKind::Or
                | op @ TokenKind::Not => op,

                TokenKind::Then
                | TokenKind::RightParen
                | TokenKind::Else
                | TokenKind::End
                | TokenKind::Comma
                | TokenKind::Semicolon
                | TokenKind::EndOfFile => break,

                _ => {
                    let token = self.next_token()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "operator or expression terminator".to_owned(),
                        token,
                    });
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                let op_token = self.consume_next(op)?;

                // no recursive call here, because we have already
                // parsed our operand `lhs`
                left_hand_side = Spanned {
                    span: (left_hand_side.span.start..op_token.span.end).into(),
                    node: Expression::UnaryOperation {
                        op,
                        expr: Box::new(left_hand_side),
                    },
                };
                // parsed an operator --> go round the loop again
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op)?;

                let right_hand_side = crate::validate_used_if_expression(
                    self.parse_expression(right_binding_power)?,
                )?;
                left_hand_side = Spanned {
                    span: (left_hand_side.span.start..right_hand_side.span.end).into(),
                    node: Expression::BinaryOperation {
                        op,
                        lhs: Box::new(left_hand_side),
                        rhs: Box::new(right_hand_side),
                    },
                };
                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(left_hand_side)
    }

    fn parse_lit(&mut self, lit: TokenKind) -> ParseResult<Expression> {
        let token = self.next_token()?;
        let text = self.text(token);

        Ok(Spanned {
            span: token.span,
            node: Expression::Literal(match lit {
                TokenKind::True => Lit::Bool(true),
                TokenKind::False => Lit::Bool(false),
                TokenKind::IntLit => Lit::Int(
                    // stdlib ftw
                    text.parse()
                        .map_err(|_| SyntaxError::InvalidLiteral(token))?,
                ),
                TokenKind::StringLit => Lit::String(
                    // Trim the quotes
                    text[1..(text.len() - 1)].to_string() + "\0",
                ),
                _ => unreachable!(),
            }),
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Expression> {
        let token = self.next_token()?;
        let text = self.text(token);

        if self.at(TokenKind::LeftParen) {
            self.parse_function_call(
                Spanned {
                    node: text.to_owned(),
                    span: token.span,
                },
                token.span.start,
            )
        } else {
            Ok(Spanned {
                span: token.span,
                node: Expression::Ident(text.to_owned()),
            })
        }
    }

    fn parse_if_expression(&mut self) -> ParseResult<Expression> {
        let token = self.next_token()?;
        let condition = self.boxed_expression()?;
        self.consume(TokenKind::Then)?;
        let mut true_case = Vec::new();
        while !self.multi_at(&[TokenKind::End, TokenKind::Else]) {
            let stmt = self.parse_statement()?;
            if !self.at(TokenKind::Semicolon) {
                if let Statement::UnusedExpression(e) = stmt.node {
                    true_case.push(Spanned {
                        node: Statement::ReturnStatement { expression: e },
                        span: stmt.span,
                    });
                } else {
                    true_case.push(stmt);
                }
                break;
            } else {
                true_case.push(stmt);
            }
            self.consume(TokenKind::Semicolon)?;
        }
        if self.at(TokenKind::End) {
            let end = self.consume_next(TokenKind::End)?;
            Ok(Spanned {
                span: (token.span.start..end.span.end).into(),
                node: Expression::IfExpr {
                    condition,
                    true_case,
                    false_case: None,
                },
            })
        } else {
            self.consume(TokenKind::Else)?;
            let mut false_case = Vec::new();
            while !self.at(TokenKind::End) {
                let stmt = self.parse_statement()?;
                if !self.at(TokenKind::Semicolon) {
                    if let Statement::UnusedExpression(e) = stmt.node {
                        false_case.push(Spanned {
                            node: Statement::ReturnStatement { expression: e },
                            span: stmt.span,
                        });
                    } else {
                        false_case.push(stmt);
                    }
                    break;
                } else {
                    false_case.push(stmt);
                }
                self.consume(TokenKind::Semicolon)?;
            }
            let end = self.consume_next(TokenKind::End)?;
            Ok(Spanned {
                span: (token.span.start..end.span.end).into(),
                node: Expression::IfExpr {
                    condition,
                    true_case,
                    false_case: Some(false_case),
                },
            })
        }
    }

    pub fn parse_function_call(
        &mut self,
        lhs: Spanned<String>,
        span_start: usize,
    ) -> ParseResult<Expression> {
        self.consume(TokenKind::LeftParen)?;
        let mut args = Vec::new();
        while !self.at(TokenKind::RightParen) {
            if !args.is_empty() && self.at(TokenKind::Comma) {
                self.consume(TokenKind::Comma)?;
            }
            let expr = crate::validate_used_if_expression(self.expression()?)?;
            args.push(expr);
        }
        let end = self.consume_next(TokenKind::RightParen)?;
        Ok(Spanned {
            span: (span_start..end.span.end).into(),
            node: Expression::FunctionCall {
                function: lhs,
                args,
            },
        })
    }

    #[inline(always)]
    pub fn expression(&mut self) -> ParseResult<Expression> {
        self.parse_expression(0)
    }

    #[inline(always)]
    pub(crate) fn boxed_expression(&mut self) -> SyntaxResult<BoxedExpression> {
        self.parse_expression(0).map(Box::new)
    }
}
