use std::ops::Deref;
use ulc_ast::types::Type;

pub struct LLVMType(pub String);

impl From<Type> for LLVMType {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Int => Self(String::from("i32")),
            Type::String => Self(String::from("i8*")),
            Type::Unit => Self(String::from("void")),
            Type::Bool => Self(String::from("i1")),
        }
    }
}

impl Deref for LLVMType {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
