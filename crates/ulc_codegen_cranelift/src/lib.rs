use std::path::Path;

use anyhow::Context;
use cranelift::codegen;
use cranelift::prelude::*;
use cranelift_module::DataContext;
use cranelift_module::Module;
use cranelift_object::ObjectBuilder;
use cranelift_object::ObjectModule;
use target_lexicon::Triple;
use ulc_ast::Statement;
use ulc_types::Spanned;

mod translator;

pub struct CraneliftCodegonBackend {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: ObjectModule,
}

impl CraneliftCodegonBackend {
    pub fn new(name: &str) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        let isa_builder = isa::lookup(Triple::host()).unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));
        let builder = ObjectBuilder::new(
            isa,
            name.to_owned(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }

    pub fn compile(&mut self, stmts: Vec<Spanned<Statement>>) -> anyhow::Result<()> {
        for stmt in stmts {
            self.translate_top(stmt.node)?;
        }
        Ok(())
    }

    fn translate_top(&mut self, stmt: Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::FunctionDefinition(func) => {
                translator::translate_func(
                    &mut self.module,
                    &mut self.ctx,
                    &mut self.builder_context,
                    func,
                )?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn finish(self, res_file: &Path) -> anyhow::Result<()> {
        let product = self.module.finish();
        let obj = product.object.write().unwrap();
        std::fs::write(res_file, obj).context("Failed to write object file!")
    }
}
