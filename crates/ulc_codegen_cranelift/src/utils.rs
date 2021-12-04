use cranelift::prelude::*;
use ulc_types::ULCType;

pub fn convert_type(ty: ULCType, tcf: isa::TargetFrontendConfig, inv: bool) -> Type {
    match ty {
        ULCType::Bool => types::B1,
        ULCType::Int => types::I32,
        ULCType::Unit if inv => types::INVALID,
        ULCType::Unit => types::I8,
        ULCType::String => tcf.pointer_type(),
    }
}
