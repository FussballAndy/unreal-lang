use std::fmt::Display;

use crate::{errors::SyntaxError, Spanned};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ULCType {
    Int,
    String,
    Unit,
    Bool,
}

impl TryFrom<Spanned<String>> for ULCType {
    type Error = SyntaxError;

    fn try_from(value: Spanned<String>) -> Result<Self, Self::Error> {
        match value.node.as_str() {
            "Int" => Ok(Self::Int),
            "String" => Ok(Self::String),
            "Unit" => Ok(Self::Unit),
            "Bool" => Ok(Self::Bool),
            _ => Err(SyntaxError::InvalidType(value)),
        }
    }
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
            ULCType::String => "String"
        }
    }
}
