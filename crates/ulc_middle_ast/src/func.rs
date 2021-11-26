use std::collections::HashMap;

use ulc_ast::{Expression, Function, Statement};
use ulc_types::{
    errors::{SyntaxError, SyntaxResult},
    Spanned, ULCType,
};

use crate::{FuncData, expr::{BinaryOperator, MiddleAstExpression, UnaryOperator}, stmt::MiddleAstStatement};

#[derive(Copy, Clone)]
struct VarData {
    id: u32,
    mutable: bool,
    ty: ULCType,
}

pub struct MiddleAstFunction {
    ident: String,
    return_type: ULCType,
    params: Vec<(u32, ULCType)>,
    body: Vec<Spanned<MiddleAstStatement>>,
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
        if known_funcs.contains_key(&ident.node) {
            return Err(SyntaxError::AlreadyDeclaredFunc(
                known_funcs.get(&ident.node).unwrap().ident.clone(),
                ident,
            ));
        }
        let mut vars: HashMap<String, VarData> = HashMap::new();
        let mut a = Vec::new();
        let mut cur_var = 0;
        for (par_name, par_type) in params {
            vars.insert(
                par_name,
                VarData {
                    id: cur_var,
                    mutable: false,
                    ty: par_type.clone(),
                },
            );
            a.push((cur_var, par_type));
            cur_var += 1;
        }
        let mut middle_body = Vec::new();
        for stmt in body {
            middle_body.push(Spanned::new(
                stmt.span,
                translate_stmt(known_funcs, &mut vars, &mut cur_var, stmt)?,
            ));
        }
        Ok(Self {
            ident: ident.node,
            params: a,
            return_type,
            body: middle_body,
        })
    }
}

fn translate_stmt(
    known_funcs: &HashMap<String, FuncData>,
    vars: &mut HashMap<String, VarData>,
    cur_var: &mut u32,
    stmt: Spanned<Statement>,
) -> SyntaxResult<MiddleAstStatement> {
    match stmt.node {
        Statement::Assignment { name, expr } => {
            if let Some(&var_data) = vars.get(&name.node) {
                if var_data.mutable {
                    let ex_span = expr.span;
                    let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, *expr)?;
                    if var_data.ty == ty {
                        Ok(MiddleAstStatement::Assignment {
                            name: var_data.id,
                            expr: Box::new(Spanned::new(ex_span, middle_expr)),
                        })
                    } else {
                        Err(SyntaxError::NotMatchingType {
                            expected: var_data.ty,
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
            if let Some(_) = vars.get(&name.node) {
                Err(SyntaxError::AlreadyDeclaredVar(name))
            } else {
                vars.insert(
                    name.node,
                    VarData {
                        id: *cur_var,
                        mutable: false,
                        ty: const_type,
                    },
                );
                let ex_span = expr.span;
                let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, *expr)?;
                if const_type == ty {
                    let a = MiddleAstStatement::Const {
                        name: *cur_var,
                        const_type,
                        expr: Box::new(Spanned::new(ex_span, middle_expr)),
                    };
                    *cur_var += 1;
                    Ok(a)
                } else {
                    Err(SyntaxError::NotMatchingType {
                        expected: const_type,
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
            if let Some(_) = vars.get(&name.node) {
                Err(SyntaxError::AlreadyDeclaredVar(name))
            } else {
                vars.insert(
                    name.node,
                    VarData {
                        id: *cur_var,
                        mutable: true,
                        ty: let_type,
                    },
                );
                let ex_span = expr.span;
                let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, *expr)?;
                if let_type == ty {
                    let a = MiddleAstStatement::Let {
                        name: *cur_var,
                        let_type,
                        expr: Box::new(Spanned::new(ex_span, middle_expr)),
                    };
                    *cur_var += 1;
                    Ok(a)
                } else {
                    Err(SyntaxError::NotMatchingType {
                        expected: let_type,
                        got: ty,
                        span: ex_span,
                    })
                }
            }
        }
        Statement::ReturnStatement { expression } => {
            let ex_span = expression.span;
            let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, *expression)?;
            Ok(MiddleAstStatement::ReturnStatement {
                ty,
                expression: Box::new(Spanned::new(ex_span, middle_expr)),
            })
        }
        Statement::UnusedExpression(expr) => {
            let ex_span = expr.span;
            let (middle_expr, _ty) = translate_expr(known_funcs, vars, cur_var, *expr)?;
            Ok(MiddleAstStatement::UnusedExpression(Box::new(
                Spanned::new(ex_span, middle_expr),
            )))
        }
        Statement::FunctionDefinition(_) => unreachable!(),
    }
}

fn translate_expr(
    known_funcs: &HashMap<String, FuncData>,
    vars: &mut HashMap<String, VarData>,
    cur_var: &mut u32,
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
            if let Some(var_data) = vars.get(&idt) {
                Ok((MiddleAstExpression::Ident(var_data.id), var_data.ty))
            } else {
                Err(SyntaxError::InvalidIdent(Spanned::new(expr.span, idt)))
            }
        }
        Expression::FunctionCall { function, args } => {
            if let Some(fu) = known_funcs.get(&function.node) {
                let mut args_b = Vec::new();
                let mut par_types_iter = fu.param_tys.iter();
                for arg in args {
                    let ar_span = arg.span;
                    let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, arg)?;
                    if let Some(next) = par_types_iter.next() {
                        if next != &ty {
                            return Err(SyntaxError::NotMatchingType {
                                expected: *next,
                                got: ty,
                                span: ar_span,
                            });
                        }
                    } else {
                        return Err(SyntaxError::FuncCallArgAmount {
                            func_sig: format!("{}({})", fu.ident.node, fu.param_tys.iter().map(|s| s.into()).collect::<Vec<&str>>().join(", ")),
                            span: expr.span,
                            m_or_l: true,
                        });
                    }
                    args_b.push(Spanned::new(
                        ar_span,
                        middle_expr,
                    ));
                }
                if par_types_iter.next() != None {
                    return Err(SyntaxError::FuncCallArgAmount {
                        func_sig: format!("{}({})", fu.ident.node, fu.param_tys.iter().map(|s| s.into()).collect::<Vec<&str>>().join(", ")),
                        span: expr.span,
                        m_or_l: false,
                    });
                }
                Ok((
                    MiddleAstExpression::FunctionCall {
                        function: function.node,
                        args: args_b,
                    },
                    fu.ret_ty,
                ))
            } else {
                Err(SyntaxError::InvalidIdent(function))
            }
        }
        Expression::BinaryOperation { op, lhs, rhs } => {
            let oper: BinaryOperator = op.try_into()?;
            let lhs_span = lhs.span;
            let rhs_span = rhs.span;
            let (lhs_node, lhs_ty) = translate_expr(known_funcs, vars, cur_var, *lhs)?;
            let (rhs_node, rhs_ty) = translate_expr(known_funcs, vars, cur_var, *rhs)?;
            if lhs_ty != ULCType::Int {
                return Err(SyntaxError::NotMatchingType {
                    expected: ULCType::Int,
                    got: lhs_ty,
                    span: lhs_span,
                });
            }
            if rhs_ty != ULCType::Int {
                return Err(SyntaxError::NotMatchingType {
                    expected: ULCType::Int,
                    got: rhs_ty,
                    span: rhs_span,
                });
            }
            Ok((
                MiddleAstExpression::BinaryOperation {
                    op: oper,
                    lhs: Box::new(Spanned::new(lhs_span, lhs_node)),
                    rhs: Box::new(Spanned::new(rhs_span, rhs_node)),
                },
                ULCType::Int,
            ))
        }
        Expression::UnaryOperation { op, expr } => {
            let oper: UnaryOperator = op.try_into()?;
            let ex_span = expr.span;
            let (middle_expr, ty) = translate_expr(known_funcs, vars, cur_var, *expr)?;
            if oper == UnaryOperator::Minus {
                if ty != ULCType::Int {
                    return Err(SyntaxError::NotMatchingType {
                        expected: ULCType::Int,
                        got: ty,
                        span: ex_span,
                    });
                }
            } else {
                if ty != ULCType::Bool {
                    return Err(SyntaxError::NotMatchingType {
                        expected: ULCType::Bool,
                        got: ty,
                        span: ex_span,
                    });
                }
            }
            Ok((
                MiddleAstExpression::UnaryOperation {
                    op: oper,
                    expr: Box::new(Spanned::new(ex_span, middle_expr)),
                },
                ty,
            ))
        }
        Expression::IfExpr {
            condition,
            true_case,
            false_case,
        } => {
            let cond_span = condition.span;
            let (middle_cond, cond_ty) = translate_expr(known_funcs, vars, cur_var, *condition)?;

            if cond_ty != ULCType::Bool {
                return Err(SyntaxError::NotMatchingType {
                    expected: ULCType::Bool,
                    got: cond_ty,
                    span: cond_span,
                });
            }

            let cond = Box::new(Spanned::new(
                cond_span,
                middle_cond,
            ));

            let mut if_ty = ULCType::Unit;

            let mut true_c = Vec::new();

            for tru_c in true_case {
                true_c.push(Spanned::new(
                    tru_c.span,
                    translate_stmt(known_funcs, vars, cur_var, tru_c)?,
                ));
            }

            if let Some(Spanned {node, span: _s}) = true_c.last() {
                if let MiddleAstStatement::ReturnStatement { expression: _e, ty } = node {
                    if_ty = ty.clone();
                }

            }

            let false_c = if let Some(false_ca) = false_case {
                let mut fl_cases = Vec::new();
                for fal_c in false_ca {
                    fl_cases.push(Spanned::new(
                        fal_c.span,
                        translate_stmt(known_funcs, vars, cur_var, fal_c)?,
                    ));
                }
                if let Some(Spanned {node, span: _s}) = fl_cases.last() {
                    if let MiddleAstStatement::ReturnStatement { expression, ty } = node {
                        if &if_ty != ty {
                            return Err(SyntaxError::NotMatchingType {
                                expected: if_ty,
                                got: ty.clone(),
                                span: expression.span,
                            })
                        }
                    }
                }
                Some(fl_cases)
            } else {
                None
            };

            Ok((MiddleAstExpression::IfExpr {
                condition: cond,
                true_case: true_c,
                false_case: false_c,
            }, if_ty))
        }
    }
}
