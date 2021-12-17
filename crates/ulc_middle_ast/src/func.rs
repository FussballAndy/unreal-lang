use std::collections::HashMap;

use ulc_ast::{BinaryOperation, Expression, Function, Statement, UnaryOperation};
use ulc_types::{
    errors::{SyntaxError, SyntaxResult},
    Spanned, ULCType,
};

use crate::{
    expr::{MiddleAstBinaryOperation, MiddleAstExpression, MiddleAstUnaryOperation},
    stmt::MiddleAstStatement,
    FuncData,
};

#[derive(Copy, Clone)]
struct VarData {
    id: usize,
    mutable: bool,
    ty: ULCType,
}

pub struct MiddleAstFunction {
    pub ident: String,
    pub return_type: ULCType,
    pub params: Vec<(usize, ULCType)>,
    pub body: Vec<MiddleAstStatement>,
}

impl MiddleAstFunction {
    pub(crate) fn new(
        known_funcs: &HashMap<String, FuncData>,
        fun: Function,
    ) -> SyntaxResult<Self> {
        let Function {
            ident,
            return_type,
            params,
            body,
        } = fun;
        let mut vars: HashMap<String, VarData> = HashMap::new();
        let mut a = Vec::new();
        let mut cur_var = 0;
        for (par_name, par_type) in params {
            vars.insert(
                par_name,
                VarData {
                    id: cur_var,
                    mutable: false,
                    ty: par_type,
                },
            );
            a.push((cur_var, par_type));
            cur_var += 1;
        }
        let mut middle_body = Vec::new();
        for stmt in body {
            let mut mat = MiddleAstTranslator {
                return_type,
                known_funcs,
                vars: &mut vars,
                cur_var: &mut cur_var,
            };
            middle_body.push(mat.translate_stmt(stmt)?);
        }
        Ok(Self {
            ident: ident.node,
            params: a,
            return_type,
            body: middle_body,
        })
    }
}

struct MiddleAstTranslator<'a> {
    return_type: ULCType,
    known_funcs: &'a HashMap<String, FuncData>,
    vars: &'a mut HashMap<String, VarData>,
    cur_var: &'a mut usize,
}

impl<'a> MiddleAstTranslator<'a> {
    fn translate_stmt(&mut self, stmt: Spanned<Statement>) -> SyntaxResult<MiddleAstStatement> {
        match stmt.node {
            Statement::Assignment { name, expr } => {
                if let Some(&var_data) = self.vars.get(&name.node) {
                    if var_data.mutable {
                        let ex_span = expr.span;
                        let (middle_expr, ty) = self.translate_expr(*expr)?;
                        if var_data.ty == ty {
                            Ok(MiddleAstStatement::Assignment {
                                name: var_data.id,
                                expr: Box::new(middle_expr),
                            })
                        } else {
                            Err(SyntaxError::NotMatchingType {
                                expected: vec![var_data.ty],
                                got: ty,
                                span: ex_span,
                            })
                        }
                    } else {
                        Err(SyntaxError::NotMutableVar(name))
                    }
                } else {
                    Err(SyntaxError::InvalidIdent(name))
                }
            }
            Statement::Const {
                name,
                const_type,
                expr,
            } => {
                if self.vars.get(&name.node).is_some() {
                    Err(SyntaxError::AlreadyDeclaredVar(name))
                } else {
                    self.vars.insert(
                        name.node,
                        VarData {
                            id: *self.cur_var,
                            mutable: false,
                            ty: const_type,
                        },
                    );
                    let ex_span = expr.span;
                    let (middle_expr, ty) = self.translate_expr(*expr)?;
                    if const_type == ty {
                        let a = MiddleAstStatement::Const {
                            name: *self.cur_var,
                            const_type,
                            expr: Box::new(middle_expr),
                        };
                        *self.cur_var += 1;
                        Ok(a)
                    } else {
                        Err(SyntaxError::NotMatchingType {
                            expected: vec![const_type],
                            got: ty,
                            span: ex_span,
                        })
                    }
                }
            }
            Statement::Let {
                name,
                let_type,
                expr,
            } => {
                if self.vars.get(&name.node).is_some() {
                    Err(SyntaxError::AlreadyDeclaredVar(name))
                } else {
                    let ex_span = expr.span;
                    let (middle_expr, ty) = self.translate_expr(*expr)?;

                    if let_type == ty {
                        if let_type == ULCType::String {
                            return Err(SyntaxError::NotSupported {
                                span: stmt.span,
                                message: "Strings are only supported with consts for now!",
                            });
                        }

                        self.vars.insert(
                            name.node,
                            VarData {
                                id: *self.cur_var,
                                mutable: true,
                                ty: let_type,
                            },
                        );

                        let a = MiddleAstStatement::Let {
                            name: *self.cur_var,
                            let_type,
                            expr: Box::new(middle_expr),
                        };

                        *self.cur_var += 1;
                        Ok(a)
                    } else {
                        Err(SyntaxError::NotMatchingType {
                            expected: vec![let_type],
                            got: ty,
                            span: ex_span,
                        })
                    }
                }
            }
            Statement::ReturnStatement { expression } => {
                let (middle_expr, ty) = self.translate_expr(*expression)?;
                if self.return_type != ty {
                    return Err(SyntaxError::NotMatchingType {
                        expected: vec![self.return_type],
                        got: ty,
                        span: stmt.span,
                    });
                }
                Ok(MiddleAstStatement::ReturnStatement {
                    ty,
                    expression: Box::new(middle_expr),
                })
            }
            Statement::UnusedExpression(expr) => {
                let (middle_expr, _ty) = self.translate_expr(*expr)?;
                Ok(MiddleAstStatement::UnusedExpression(Box::new(middle_expr)))
            }
            Statement::FunctionDefinition(_) => unreachable!(),
        }
    }

    fn translate_expr(
        &mut self,
        expr: Spanned<Expression>,
    ) -> SyntaxResult<(MiddleAstExpression, ULCType)> {
        match expr.node {
            Expression::Literal(lit) => {
                let ty = match &lit {
                    ulc_ast::Lit::Unit => ULCType::Unit,
                    ulc_ast::Lit::Bool(_) => ULCType::Bool,
                    ulc_ast::Lit::Int(_) => ULCType::Int,
                    ulc_ast::Lit::String(_) => ULCType::String,
                };
                Ok((MiddleAstExpression::Literal(lit), ty))
            }
            Expression::Ident(idt) => {
                if let Some(var_data) = self.vars.get(&idt) {
                    Ok((MiddleAstExpression::Ident(var_data.id), var_data.ty))
                } else {
                    Err(SyntaxError::InvalidIdent(Spanned::new(expr.span, idt)))
                }
            }
            Expression::FunctionCall { function, args } => {
                if let Some(fu) = self.known_funcs.get(&function.node) {
                    let mut args_b = Vec::new();
                    let mut par_types_iter = fu.param_tys.iter();
                    for arg in args {
                        let ar_span = arg.span;
                        let (middle_expr, ty) = self.translate_expr(arg)?;
                        if let Some(next) = par_types_iter.next() {
                            if next != &ty {
                                return Err(SyntaxError::NotMatchingType {
                                    expected: vec![*next],
                                    got: ty,
                                    span: ar_span,
                                });
                            }
                        } else {
                            return Err(SyntaxError::FuncCallArgAmount {
                                func_sig: format!(
                                    "{}({})",
                                    fu.ident.node,
                                    fu.param_tys
                                        .iter()
                                        .map(|s| s.into())
                                        .collect::<Vec<&str>>()
                                        .join(", ")
                                ),
                                span: expr.span,
                                m_or_l: true,
                            });
                        }
                        args_b.push((middle_expr, ty));
                    }
                    if par_types_iter.next() != None {
                        return Err(SyntaxError::FuncCallArgAmount {
                            func_sig: format!(
                                "{}({})",
                                fu.ident.node,
                                fu.param_tys
                                    .iter()
                                    .map(|s| s.into())
                                    .collect::<Vec<&str>>()
                                    .join(", ")
                            ),
                            span: expr.span,
                            m_or_l: false,
                        });
                    }
                    Ok((
                        MiddleAstExpression::FunctionCall {
                            function: function.node,
                            args: args_b,
                            ret_ty: fu.ret_ty,
                        },
                        fu.ret_ty,
                    ))
                } else {
                    Err(SyntaxError::InvalidIdent(function))
                }
            }
            Expression::BinaryOperation(oper) => self.translate_binary_operation(oper),
            Expression::UnaryOperation(oper) => self.translate_unary_operation(oper),
            Expression::IfExpr {
                condition,
                true_case,
                false_case,
            } => {
                let cond_span = condition.span;
                let (middle_cond, cond_ty) = self.translate_expr(*condition)?;

                if cond_ty != ULCType::Bool {
                    return Err(SyntaxError::NotMatchingType {
                        expected: vec![ULCType::Bool],
                        got: cond_ty,
                        span: cond_span,
                    });
                }

                let cond = Box::new(middle_cond);

                let mut if_ty = ULCType::Unit;

                let mut true_c = Vec::new();

                for tru_c in true_case {
                    true_c.push(Spanned::new(tru_c.span, self.translate_stmt(tru_c)?));
                }

                if let Some(Spanned {
                    node: MiddleAstStatement::ReturnStatement { ty, .. },
                    ..
                }) = true_c.last()
                {
                    if_ty = *ty;
                }

                let false_c = if let Some(false_ca) = false_case {
                    let mut fl_cases = Vec::new();
                    for fal_c in false_ca {
                        fl_cases.push(Spanned::new(fal_c.span, self.translate_stmt(fal_c)?));
                    }
                    if let Some(Spanned {
                        node: MiddleAstStatement::ReturnStatement { ty, .. },
                        span,
                    }) = fl_cases.last()
                    {
                        if &if_ty != ty {
                            return Err(SyntaxError::NotMatchingType {
                                expected: vec![if_ty],
                                got: *ty,
                                span: *span,
                            });
                        }
                    }
                    fl_cases.iter().map(|s| s.node.clone()).collect()
                } else {
                    Vec::new()
                };

                Ok((
                    MiddleAstExpression::IfExpr {
                        condition: cond,
                        true_case: true_c.iter().map(|s| s.node.clone()).collect(),
                        false_case: false_c,
                        ret_ty: if if_ty == ULCType::Unit {
                            None
                        } else {
                            Some(if_ty)
                        },
                    },
                    if_ty,
                ))
            }
        }
    }

    fn translate_unary_operation(
        &mut self,
        oper: UnaryOperation,
    ) -> SyntaxResult<(MiddleAstExpression, ULCType)> {
        match oper {
            UnaryOperation::Neg(expr) => {
                let ex_span = expr.span;
                let (middle_expr, ty) = self.translate_expr(*expr)?;
                if ty != ULCType::Int {
                    return Err(SyntaxError::NotMatchingType {
                        expected: vec![ULCType::Int],
                        got: ty,
                        span: ex_span,
                    });
                }
                Ok((
                    MiddleAstExpression::UnaryOperation(MiddleAstUnaryOperation::Minus(Box::new(
                        middle_expr,
                    ))),
                    ULCType::Int,
                ))
            }
            UnaryOperation::Not(expr) => {
                let ex_span = expr.span;
                let (middle_expr, ty) = self.translate_expr(*expr)?;
                if ty != ULCType::Bool {
                    return Err(SyntaxError::NotMatchingType {
                        expected: vec![ULCType::Bool],
                        got: ty,
                        span: ex_span,
                    });
                }
                Ok((
                    MiddleAstExpression::UnaryOperation(MiddleAstUnaryOperation::Invert(Box::new(
                        middle_expr,
                    ))),
                    ULCType::Bool,
                ))
            }
        }
    }

    fn translate_binary_operation(
        &mut self,
        oper: BinaryOperation,
    ) -> SyntaxResult<(MiddleAstExpression, ULCType)> {
        let (oper_fun, (lhs, rhs), expect, returns) = match oper {
            BinaryOperation::Add(lhs, rhs) => (
                MiddleAstBinaryOperation::Add as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Int,
            ),
            BinaryOperation::Sub(lhs, rhs) => (
                MiddleAstBinaryOperation::Sub as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Int,
            ),
            BinaryOperation::Mul(lhs, rhs) => (
                MiddleAstBinaryOperation::Mul as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Int,
            ),
            BinaryOperation::Div(lhs, rhs) => (
                MiddleAstBinaryOperation::Div as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Int,
            ),

            BinaryOperation::NEq(lhs, rhs) => (
                MiddleAstBinaryOperation::NEq as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),
            BinaryOperation::Eq(lhs, rhs) => (
                MiddleAstBinaryOperation::Eq as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),
            BinaryOperation::GT(lhs, rhs) => (
                MiddleAstBinaryOperation::GT as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),
            BinaryOperation::ST(lhs, rhs) => (
                MiddleAstBinaryOperation::ST as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),
            BinaryOperation::GTOE(lhs, rhs) => (
                MiddleAstBinaryOperation::GTOE as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),
            BinaryOperation::STOE(lhs, rhs) => (
                MiddleAstBinaryOperation::STOE as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Int,
                ULCType::Bool,
            ),

            BinaryOperation::And(lhs, rhs) => (
                MiddleAstBinaryOperation::And as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Bool,
                ULCType::Bool,
            ),
            BinaryOperation::Or(lhs, rhs) => (
                MiddleAstBinaryOperation::Or as fn(_, _) -> _,
                (lhs, rhs),
                ULCType::Bool,
                ULCType::Bool,
            ),
        };
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let (lhs_node, lhs_ty) = self.translate_expr(*lhs)?;
        let (rhs_node, rhs_ty) = self.translate_expr(*rhs)?;

        if lhs_ty != expect {
            return Err(SyntaxError::NotMatchingType {
                expected: vec![expect],
                got: lhs_ty,
                span: lhs_span,
            });
        }

        if rhs_ty != expect {
            return Err(SyntaxError::NotMatchingType {
                expected: vec![expect],
                got: rhs_ty,
                span: rhs_span,
            });
        }

        Ok((
            MiddleAstExpression::BinaryOperation(oper_fun(Box::new(lhs_node), Box::new(rhs_node))),
            returns,
        ))
    }
}
