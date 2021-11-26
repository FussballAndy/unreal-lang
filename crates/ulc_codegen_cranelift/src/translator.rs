use std::collections::HashMap;

use cranelift::codegen::ir::types;
use cranelift::prelude::*;
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use ulc_ast::{Expression, Function, Lit, Statement};
use ulc_types::ULCType;

fn convert_type(ty: ULCType, tcf: isa::TargetFrontendConfig) -> Type {
    match ty {
        ULCType::Bool => types::B1,
        ULCType::Int => types::I32,
        ULCType::Unit => types::INVALID,
        ULCType::String => tcf.pointer_type(),
    }
}

pub(crate) fn translate_func(
    module: &mut ObjectModule,
    ctx: &mut codegen::Context,
    bctx: &mut FunctionBuilderContext,
    func: Function,
) -> anyhow::Result<()> {
    for param in func.params {
        ctx.func
            .signature
            .params
            .push(AbiParam::new(convert_type(param.1, module.target_config())));
    }
    ctx.func.signature.returns.push(AbiParam::new(convert_type(
        func.return_type,
        module.target_config(),
    )));
    let mut idents = Vec::new();
    for stmt in &func.body {
        walk_lets_stmt(&mut idents, &stmt.node);
    }
    let mut stack_slots = HashMap::new();
    for (idt, ty) in idents {
        stack_slots.insert(
            idt,
            ctx.func.create_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                convert_type(ty, module.target_config()).bytes(),
            )),
        );
    }
    let mut builder = FunctionBuilder::new(&mut ctx.func, bctx);
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let mut trans = FunctionTranslator {
        builder,
        variables: HashMap::new(),
        module,
    };

    for stmt in func.body {
        trans.translate_stmt(stmt.node);
    }

    Ok(())
}

fn walk_lets_stmt(let_vec: &mut Vec<(String, ULCType)>, root: &Statement) {
    match root {
        Statement::Let {
            name,
            expr,
            let_type,
        } => {
            let_vec.push((name.node.clone(), let_type.clone()));
            walk_lets_expr(let_vec, &expr.node);
        }
        Statement::Assignment { expr, .. } => {
            walk_lets_expr(let_vec, &expr.node);
        }
        Statement::Const { expr, .. } => walk_lets_expr(let_vec, &expr.node),
        Statement::ReturnStatement { expression } => {
            walk_lets_expr(let_vec, &expression.node);
        }
        Statement::UnusedExpression(expr) => walk_lets_expr(let_vec, &expr.node),
        _ => unreachable!(),
    }
}
fn walk_lets_expr(let_vec: &mut Vec<(String, ULCType)>, root: &Expression) {
    match root {
        Expression::BinaryOperation { lhs, rhs, .. } => {
            walk_lets_expr(let_vec, &lhs.node);
            walk_lets_expr(let_vec, &rhs.node);
        }
        Expression::UnaryOperation { expr, .. } => {
            walk_lets_expr(let_vec, &expr.node);
        }
        Expression::FunctionCall { args, .. } => {
            for a in args {
                walk_lets_expr(let_vec, &a.node);
            }
        }
        Expression::IfExpr {
            condition,
            true_case,
            false_case,
        } => {
            walk_lets_expr(let_vec, &condition.node);
            for tr_cs in true_case {
                walk_lets_stmt(let_vec, &tr_cs.node);
            }
            if let Some(false_case) = false_case {
                for fl_cs in false_case {
                    walk_lets_stmt(let_vec, &fl_cs.node);
                }
            }
        }
        _ => {}
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut ObjectModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_stmt(&mut self, stmt: Statement) -> Value {
        match stmt {
            Statement::Let {
                name,
                let_type,
                expr,
            } => match let_type {
                ULCType::Bool => match expr.node {
                    Expression::Literal(Lit::Bool(b)) => {
                        let vr = Variable::new(self.variables.len());
                        self.builder
                            .declare_var(vr, convert_type(let_type, self.module.target_config()));
                        let tmp = self.builder.ins().bconst(types::B1, b);
                        self.builder.def_var(vr, tmp)
                    }
                    e => {
                        self.translate_expr(e);
                    }
                },
                ULCType::Int => {}
                ULCType::String => {}
                ULCType::Unit => {}
            },
            _ => unreachable!(),
        }
        unreachable!()
    }

    fn translate_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Literal(_) => todo!(),
            Expression::Ident(_) => todo!(),
            Expression::FunctionCall { function, args } => todo!(),
            Expression::BinaryOperation { op, lhs, rhs } => todo!(),
            Expression::UnaryOperation { op, expr } => todo!(),
            Expression::IfExpr {
                condition,
                true_case,
                false_case,
            } => todo!(),
        }
    }
}
