use std::{borrow::Borrow, collections::HashMap};

use ulc_ast::{types::ULCType, Expression, Function, Statement};
use ulc_types::Spanned;

use crate::{expr::{BinaryOperator, MiddleAstExpression}, stmt::MiddleAstStatement};

pub struct MiddleAstFunction {
    ident: String,
    return_type: ULCType,
    params: Vec<(u32, ULCType)>,
    body: Vec<Spanned<MiddleAstStatement>>,
}

impl MiddleAstFunction {
    pub fn new(fun: Function) -> anyhow::Result<Self> {
        let Function {
            ident,
            return_type,
            params,
            body,
        } = fun;
        let mut vars: HashMap<String, (u32, bool)> = HashMap::new();
        let mut a = Vec::new();
        let mut cur_var = 0;
        for (par_name, par_type) in params {
            vars.insert(par_name, (cur_var, false));
            a.push((cur_var, par_type));
            cur_var += 1;
        }
        let mut middle_body = Vec::new();
        for stmt in body {
            middle_body.push(stmt.map(|val| translate_stmt(&mut vars, &mut cur_var, val))?)
        }
        Ok(Self {
            ident,
            params: a,
            return_type,
            body: middle_body,
        })
    }
}

fn translate_stmt(
    vars: &mut HashMap<String, (u32, bool)>,
    cur_var: &mut u32,
    stmt: Statement,
) -> anyhow::Result<MiddleAstStatement> {
    match stmt {
        Statement::Assignment { name, expr } => {
            if let Some((var_id, mutable)) = vars.get(&name) {
                if *mutable {
                    Ok(MiddleAstStatement::Assignment {
                        name: *var_id,
                        expr: Box::new(expr.map(|exp| 
                            translate_expr(vars, cur_var, exp)
                        )?),
                    })
                } else {
                    anyhow::bail!("Variable not mutable");
                }
            } else {
                anyhow::bail!("Ident not found!")
            }
        }
        Statement::Const {
            name,
            const_type,
            expr,
        } => {
            if let Some(_) = vars.get(&name) {
                anyhow::bail!("Variable already defined!")
            } else {
                vars.insert(name, (*cur_var, false));
                let a = MiddleAstStatement::Const {
                    name: *cur_var,
                    const_type,
                    expr: Box::new(expr.map(|exp| translate_expr(vars, cur_var, exp))?),
                };
                *cur_var += 1;
                Ok(a)
            }
        }
        Statement::Let {
            name,
            let_type,
            expr,
        } => {
            if let Some(_) = vars.get(&name) {
                anyhow::bail!("Variable already defined!")
            } else {
                vars.insert(name, (*cur_var, true));
                let a = MiddleAstStatement::Let {
                    name: *cur_var,
                    let_type,
                    expr: Box::new(expr.map(|exp| translate_expr(vars, cur_var, exp))?),
                };
                *cur_var += 1;
                Ok(a)
            }
        }
        Statement::ReturnStatement { expression } => {
            Ok(MiddleAstStatement::ReturnStatement {
                expression: Box::new(expression.map(|exp| translate_expr(vars, cur_var, exp))?)
            })
        },
        Statement::UnusedExpression(expr) => {
            Ok(MiddleAstStatement::UnusedExpression(Box::new(expr.map(|exp| translate_expr(vars, cur_var, exp))?)))
        },
        Statement::FunctionDefinition(_) => unreachable!(),
    }
}

fn translate_expr(
    vars: &mut HashMap<String, (u32, bool)>,
    cur_var: &mut u32,
    expr: Expression,
) -> anyhow::Result<MiddleAstExpression> {
    match expr {
        Expression::Literal(lit) => Ok(MiddleAstExpression::Literal(lit)),
        Expression::Ident(idt) => {
            if let Some((id, _)) = vars.get(&idt) {
                Ok(MiddleAstExpression::Ident(*id))
            } else {
                anyhow::bail!("Unknwon ident!")
            }
        },
        Expression::FunctionCall { function, args } => {
            let mut args_b = Vec::new();
            for arg in args {
                args_b.push(arg.map(|ar| translate_expr(vars, cur_var, ar))?)
            }
            Ok(MiddleAstExpression::FunctionCall {
                function,
                args: args_b,
            })
        },
        Expression::BinaryOperation { op, lhs, rhs } => {
            let oper: BinaryOperator = op.try_into()?;
            
        },
        Expression::UnaryOperation { op, expr } => todo!(),
        Expression::IfExpr { condition, true_case, false_case } => todo!(),
    }
}
