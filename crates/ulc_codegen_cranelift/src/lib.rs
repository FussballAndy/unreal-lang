use std::path::Path;

use anyhow::Context;
use cranelift::prelude::*;
use cranelift::codegen;
use cranelift_module::DataContext;
use cranelift_module::Module;
use cranelift_object::ObjectBuilder;
use cranelift_object::ObjectModule;
use target_lexicon;
use target_lexicon::Triple;


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
            cranelift_module::default_libcall_names()
        ).unwrap();
        let module = ObjectModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module
        }
    }

    pub fn finish(self, res_file: &Path) -> anyhow::Result<()> {
        let product = self.module.finish();
        let obj = product.object.write().unwrap();
        std::fs::write(res_file, obj).context("Failed to write object file!")
    }

}