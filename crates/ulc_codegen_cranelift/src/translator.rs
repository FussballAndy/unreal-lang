use std::collections::HashMap;

use crate::convert_type;
use cranelift::codegen::binemit::{NullStackMapSink, NullTrapSink};
use cranelift::codegen::ir::{types, StackSlot};
use cranelift::codegen::verify_function;
use cranelift::prelude::*;
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use ulc_ast::Lit;
use ulc_middle_ast::{
    MiddleAstBinaryOperation, MiddleAstExpression, MiddleAstFunction, MiddleAstStatement,
    MiddleAstUnaryOperation,
};
use ulc_types::ULCType;

pub(crate) fn translate_func(
    module: &mut ObjectModule,
    ctx: &mut codegen::Context,
    data_ctx: &mut DataContext,
    bctx: &mut FunctionBuilderContext,
    buffer: &mut String,
    func: MiddleAstFunction,
    func_id: FuncId,
) -> anyhow::Result<()> {
    intern_translate_func(module, ctx, data_ctx, bctx, func)?;

    verify_function(&ctx.func, module.isa())?;

    codegen::write_function(buffer, &ctx.func)?;

    module.define_function(func_id, ctx, &mut NullTrapSink {}, &mut NullStackMapSink {})?;

    module.clear_context(ctx);

    Ok(())
}

fn intern_translate_func(
    module: &mut ObjectModule,
    ctx: &mut codegen::Context,
    data_ctx: &mut DataContext,
    bctx: &mut FunctionBuilderContext,
    mut func: MiddleAstFunction,
) -> anyhow::Result<()> {
    let mut builder = FunctionBuilder::new(&mut ctx.func, bctx);
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let mut const_variables = HashMap::new();

    for (i, &param) in func.params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = Variable::new(param.0);
        builder.declare_var(var, convert_type(param.1, module.target_config(), false));
        builder.def_var(var, val);
        const_variables.insert(param.0, var);
    }

    let mut trans = FunctionTranslator {
        builder,
        module,
        data_ctx,

        cur_ident: None,
        cur_unused_ident: 0,

        const_variables,
        stack_slots: HashMap::new(),
    };

    let last = func.body.pop();

    for stmt in func.body {
        trans.translate_stmt(stmt)?;
    }

    if let Some(last_stmt) = last {
        let is_ret = matches!(&last_stmt, MiddleAstStatement::ReturnStatement { .. });
        trans.translate_stmt(last_stmt)?;
        if !is_ret {
            trans.builder.ins().return_(&[]);
        }
    }

    trans.builder.finalize();
    Ok(())
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
    data_ctx: &'a mut DataContext,

    cur_ident: Option<usize>,
    cur_unused_ident: usize,

    const_variables: HashMap<usize, Variable>,
    stack_slots: HashMap<usize, (StackSlot, ULCType)>,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_stmt(&mut self, stmt: MiddleAstStatement) -> anyhow::Result<()> {
        match stmt {
            MiddleAstStatement::Let {
                name,
                let_type,
                expr,
            } => {
                let ir_ty = convert_type(let_type, self.module.target_config(), false);
                let stack_slot = self.builder.create_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    ir_ty.bytes(),
                ));
                self.stack_slots.insert(name, (stack_slot, let_type));
                let val = self.translate_expr(*expr)?;
                self.builder.ins().stack_store(val, stack_slot, 0);
            }
            MiddleAstStatement::Assignment { name, expr } => {
                let val = self.translate_expr(*expr)?;
                let slot = self.stack_slots[&name].0;
                self.builder.ins().stack_store(val, slot, 0);
            }
            MiddleAstStatement::Const {
                name,
                expr,
                const_type,
            } => {
                self.cur_ident = Some(name);
                let val = self.translate_expr(*expr)?;
                self.cur_ident = None;

                let variable = Variable::new(name);

                self.builder.declare_var(
                    variable,
                    convert_type(const_type, self.module.target_config(), false),
                );
                self.builder.def_var(variable, val);

                self.const_variables.insert(name, variable);
            }
            MiddleAstStatement::ReturnStatement { expression, .. } => {
                let val = self.translate_expr(*expression)?;
                self.builder.ins().return_(&[val]);
            }
            MiddleAstStatement::UnusedExpression(expr) => {
                self.translate_expr(*expr)?;
            }
        }
        Ok(())
    }

    fn translate_expr(&mut self, expr: MiddleAstExpression) -> anyhow::Result<Value> {
        Ok(match expr {
            MiddleAstExpression::Literal(lit) => match lit {
                Lit::Unit => self.builder.ins().iconst(types::I8, 0),
                Lit::Bool(b) => self.builder.ins().bconst(types::B1, b),
                Lit::Int(i) => self.builder.ins().iconst(types::I32, i as i64),
                Lit::String(s) => {
                    let string_lit_id = if let Some(cur_i) = self.cur_ident {
                        cur_i.to_string()
                    } else {
                        let cui = self.cur_unused_ident;
                        self.cur_unused_ident += 1;
                        "unused_".to_owned() + &cui.to_string()
                    };
                    let name = &format!("string_lit_{}", string_lit_id);
                    let id = self.create_data(name, s.into_bytes())?;
                    let local_id = self.module.declare_data_in_func(id, self.builder.func);
                    self.builder
                        .ins()
                        .symbol_value(self.module.target_config().pointer_type(), local_id)
                }
            },
            MiddleAstExpression::BinaryOperation(op) => match op {
                MiddleAstBinaryOperation::Add(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().iadd(lhs, rhs)
                }
                MiddleAstBinaryOperation::Sub(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().isub(lhs, rhs)
                }
                MiddleAstBinaryOperation::Mul(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().imul(lhs, rhs)
                }
                MiddleAstBinaryOperation::Div(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().udiv(lhs, rhs)
                }
                MiddleAstBinaryOperation::NEq(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
                }
                MiddleAstBinaryOperation::Eq(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().icmp(IntCC::Equal, lhs, rhs)
                }
                MiddleAstBinaryOperation::GT(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                }
                MiddleAstBinaryOperation::ST(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                }
                MiddleAstBinaryOperation::GTOE(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                }
                MiddleAstBinaryOperation::STOE(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                }

                MiddleAstBinaryOperation::And(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().band(lhs, rhs)
                }
                MiddleAstBinaryOperation::Or(lhs, rhs) => {
                    let lhs = self.translate_expr(*lhs)?;
                    let rhs = self.translate_expr(*rhs)?;
                    self.builder.ins().bor(lhs, rhs)
                }
            },
            MiddleAstExpression::FunctionCall {
                function,
                args,
                ret_ty,
            } => {
                let mut sig = self.module.make_signature();

                for arg in &args {
                    sig.params.push(AbiParam::new(convert_type(
                        arg.1,
                        self.module.target_config(),
                        false,
                    )));
                }

                if ret_ty != ULCType::Unit {
                    sig.returns.push(AbiParam::new(convert_type(
                        ret_ty,
                        self.module.target_config(),
                        false,
                    )));
                }

                let callee = self
                    .module
                    .declare_function(&function, Linkage::Import, &sig)?;
                let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.translate_expr(arg.0)?);
                }

                let call = self.builder.ins().call(local_callee, &arg_values);
                let inst_res = self.builder.inst_results(call);
                if !inst_res.is_empty() {
                    inst_res[0]
                } else {
                    self.builder.ins().iconst(types::I8, 0)
                }
            }
            MiddleAstExpression::Ident(name) => {
                if self.const_variables.contains_key(&name) {
                    let var = self
                        .const_variables
                        .get(&name)
                        .ok_or_else(|| anyhow::anyhow!("Somehow the const does not exist!"))?;
                    self.builder.use_var(*var)
                } else {
                    let stack_slot = self.stack_slots.get(&name).ok_or_else(|| {
                        anyhow::anyhow!("Somehow the stack slot and const does not exist.")
                    })?;
                    self.builder.ins().stack_load(
                        convert_type(stack_slot.1, self.module.target_config(), false),
                        stack_slot.0,
                        0,
                    )
                }
            }
            MiddleAstExpression::IfExpr {
                condition,
                mut true_case,
                mut false_case,
                ret_ty,
            } => {
                let condition_value = self.translate_expr(*condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                if let Some(return_type) = ret_ty {
                    self.builder.append_block_param(
                        merge_block,
                        convert_type(return_type, self.module.target_config(), false),
                    );
                    self.builder.ins().brz(condition_value, else_block, &[]);

                    self.builder.ins().jump(then_block, &[]);

                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    let true_last_stmt = true_case.pop().unwrap();
                    for stmt in true_case {
                        self.translate_stmt(stmt)?;
                    }
                    let true_return =
                        if let MiddleAstStatement::ReturnStatement { expression, .. } =
                            true_last_stmt
                        {
                            self.translate_expr(*expression)?
                        } else {
                            unreachable!()
                        };

                    self.builder.ins().jump(merge_block, &[true_return]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    let false_last_stmt = false_case.pop().unwrap();
                    for stmt in false_case {
                        self.translate_stmt(stmt)?;
                    }
                    let false_return =
                        if let MiddleAstStatement::ReturnStatement { expression, .. } =
                            false_last_stmt
                        {
                            self.translate_expr(*expression)?
                        } else {
                            unreachable!()
                        };

                    self.builder.ins().jump(merge_block, &[false_return]);

                    self.builder.switch_to_block(merge_block);

                    self.builder.seal_block(merge_block);

                    self.builder.block_params(merge_block)[0]
                } else {
                    self.builder.ins().brz(condition_value, else_block, &[]);

                    self.builder.ins().jump(then_block, &[]);
                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    for stmt in true_case {
                        self.translate_stmt(stmt)?;
                    }
                    self.builder.ins().jump(merge_block, &[]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    for stmt in false_case {
                        self.translate_stmt(stmt)?;
                    }

                    self.builder.ins().jump(merge_block, &[]);

                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);

                    self.builder.ins().iconst(types::I8, 0)
                }
            }
            MiddleAstExpression::UnaryOperation(un_op) => match un_op {
                MiddleAstUnaryOperation::Minus(expr) => {
                    let val = self.translate_expr(*expr)?;
                    self.builder.ins().ineg(val)
                }
                MiddleAstUnaryOperation::Invert(expr) => {
                    let val = self.translate_expr(*expr)?;
                    self.builder.ins().bnot(val)
                }
            },
        })
    }

    fn create_data(&mut self, name: &str, contents: Vec<u8>) -> anyhow::Result<DataId> {
        self.data_ctx.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)?;
        self.module.define_data(id, self.data_ctx)?;
        self.data_ctx.clear();

        Ok(id)
    }
}
