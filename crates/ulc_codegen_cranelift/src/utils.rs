use cranelift::prelude::*;
use ulc_types::ULCType;

pub fn convert_type(ty: ULCType, tcf: isa::TargetFrontendConfig) -> Type {
    match ty {
        ULCType::Bool => types::B1,
        ULCType::Int => types::I32,
        ULCType::Unit => types::INVALID,
        ULCType::String => tcf.pointer_type(),
    }
}
