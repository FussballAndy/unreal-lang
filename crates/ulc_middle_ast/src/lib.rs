mod check;
mod expr;
mod func;
mod stmt;

use std::collections::HashMap;

use convert_case::{Case, Casing};
use ulc_ast::Function;
use ulc_types::{errors::SyntaxError, Spanned, ULCType};

pub use expr::{MiddleAstBinaryOperation, MiddleAstExpression, MiddleAstUnaryOperation};
pub use func::MiddleAstFunction;
pub use stmt::MiddleAstStatement;

pub struct MiddleAstRoot {
    pub root_functions: Vec<MiddleAstFunction>,
}

pub(crate) struct FuncData {
    ident: Spanned<String>,
    ret_ty: ULCType,
    param_tys: Vec<ULCType>,
}

impl Default for MiddleAstRoot {
    fn default() -> Self {
        Self::new()
    }
}

impl MiddleAstRoot {
    pub fn new() -> Self {
        Self {
            root_functions: Vec::new(),
        }
    }

    pub fn append_all_funcs(
        &mut self,
        funs: Vec<Spanned<Function>>,
    ) -> Result<(), MiddleAstFunctionError> {
        let mut names = HashMap::new();
        names.insert(
            "puts".to_owned(),
            FuncData {
                ident: Spanned::new(0..0, "puts".to_owned()),
                ret_ty: ULCType::Unit,
                param_tys: vec![ULCType::String],
            },
        );
        let mut funcs = Vec::new();
        for Spanned { span, node: func } in funs {
            let idt = Spanned::new(func.ident.span, func.ident.node.to_case(Case::Camel));
            if names.contains_key(&idt.node) {
                return Err(MiddleAstFunctionError(SyntaxError::AlreadyDeclaredFunc(
                    names.get(&idt.node).unwrap().ident.clone(),
                    idt,
                )));
            }
            names.insert(
                idt.node.clone(),
                FuncData {
                    ident: idt,
                    ret_ty: func.return_type,
                    param_tys: func.params.iter().map(|a| a.1).collect(),
                },
            );

            funcs.push(Spanned { node: func, span });
        }

        for func in funcs {
            self.append_func(&names, func)?;
        }

        Ok(())
    }

    fn append_func(
        &mut self,
        known_funcs: &HashMap<String, FuncData>,
        func: Spanned<Function>,
    ) -> Result<(), MiddleAstFunctionError> {
        let Spanned { node, .. } = func;
        self.root_functions
            .push(MiddleAstFunction::new(known_funcs, node).map_err(MiddleAstFunctionError)?);
        Ok(())
    }
}

pub struct MiddleAstFunctionError(pub SyntaxError);
