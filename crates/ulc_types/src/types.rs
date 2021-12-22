use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ULCType {
    Int,
    String,
    Unit,
    Bool,
}

impl Display for ULCType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int => "Int",
                Self::String => "String",
                Self::Unit => "()",
                Self::Bool => "Bool",
            }
        )
    }
}

impl From<&ULCType> for &str {
    fn from(ty: &ULCType) -> Self {
        match ty {
            ULCType::Bool => "Bool",
            ULCType::Int => "Int",
            ULCType::Unit => "()",
            ULCType::String => "String",
        }
    }
}
