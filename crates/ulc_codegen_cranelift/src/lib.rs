use std::collections::HashMap;
use std::path::Path;

use anyhow::Context;
use cranelift::codegen;
use cranelift::codegen::ir::Function;
use cranelift::prelude::*;
use cranelift_module::DataContext;
use cranelift_module::FuncId;
use cranelift_module::Linkage;
use cranelift_module::Module;
use cranelift_object::ObjectBuilder;
use cranelift_object::ObjectModule;
use target_lexicon::BinaryFormat;
use target_lexicon::Triple;
use ulc_middle_ast::MiddleAstFunction;
use ulc_middle_ast::MiddleAstRoot;

mod translator;
mod utils;
use ulc_types::ULCType;
pub(crate) use utils::convert_type;

pub struct CraneliftCodegonBackend {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: ObjectModule,
    buffer: String,
}

impl CraneliftCodegonBackend {
    pub fn new(name: &str) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        flag_builder.set("enable_verifier", "true").unwrap();

        let target_triple = Triple::host();
        let tls_model = match target_triple.binary_format {
            BinaryFormat::Elf => "elf_gd",
            BinaryFormat::Macho => "macho",
            BinaryFormat::Coff => "coff",
            _ => "none",
        };

        flag_builder.set("tls_model", tls_model).unwrap();

        let isa_builder = cranelift_native::builder_with_options(true).unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));
        let builder = ObjectBuilder::new(
            isa,
            name[..(name.len() - 3)].to_owned() + ".o",
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            buffer: Default::default(),
        }
    }

    pub fn compile(&mut self, root: MiddleAstRoot) -> anyhow::Result<()> {
        let mut func_ids = HashMap::new();
        for stmt in &root.root_functions {
            let sig = self.create_signature(stmt);
            let id = self
                .module
                .declare_function(&stmt.ident, Linkage::Export, &sig)?;
            let func = Function::with_name_signature(ExternalName::testcase(&stmt.ident), sig);
            func_ids.insert(stmt.ident.clone(), (id, func));
        }
        for stmt in root.root_functions {
            let (func_id, ir_func) = func_ids.remove(&stmt.ident).unwrap();
            self.translate_func(stmt, func_id, ir_func)?;
        }
        Ok(())
    }

    fn create_signature(&mut self, func: &MiddleAstFunction) -> Signature {
        let mut sig = self.module.make_signature();
        for param in &func.params {
            sig.params.push(AbiParam::new(convert_type(
                param.1,
                self.module.target_config(),
                false,
            )));
        }
        if func.return_type.node != ULCType::Unit {
            sig.returns.push(AbiParam::new(convert_type(
                func.return_type.node,
                self.module.target_config(),
                false,
            )));
        }

        sig
    }

    fn translate_func(
        &mut self,
        func: MiddleAstFunction,
        func_id: FuncId,
        cl_fun: Function,
    ) -> anyhow::Result<()> {
        self.ctx.func = cl_fun;
        translator::translate_func(
            &mut self.module,
            &mut self.ctx,
            &mut self.data_ctx,
            &mut self.builder_context,
            &mut self.buffer,
            func,
            func_id,
        )
    }

    pub fn finish(self, ir_file: &Path, object_file: &Path) -> anyhow::Result<()> {
        std::fs::write(ir_file, self.buffer)?;
        let product = self.module.finish();
        let obj = product.object.write().unwrap();
        std::fs::write(object_file, obj).context("Failed to write object file!")
    }
}
