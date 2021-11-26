mod expr;
mod func;
mod stmt;

use std::collections::HashMap;

use func::MiddleAstFunction;
use ulc_ast::{Function, Statement};
use ulc_types::{errors::SyntaxError, Spanned, ULCType};

pub struct MiddleAstRoot {
    root_functions: Vec<Spanned<MiddleAstFunction>>,
}

pub(crate) struct FuncData {
    ident: Spanned<String>,
    ret_ty: ULCType,
    param_tys: Vec<ULCType>,
} 

impl MiddleAstRoot {
    pub fn new() -> Self {
        Self {
            root_functions: Vec::new(),
        }
    }

    pub fn append_all_funcs(
        &mut self,
        stmts: Vec<Spanned<Statement>>,
    ) -> Result<(), MiddleAstFunctionError> {
        let mut names = HashMap::new();
        let mut funcs = Vec::new();
        for st in stmts {
            if let Statement::FunctionDefinition(func) = st.node {
                names.insert(
                    func.ident.node.clone(),
                    FuncData {
                        ident: func.ident.clone(),
                        ret_ty: func.return_type,
                        param_tys: func.params.iter().map(|a| a.1.clone()).collect()
                    }
                );
                funcs.push(Spanned {
                    node: func,
                    span: st.span,
                });
            } else {
                return Err(MiddleAstFunctionError::NotAFunction(st));
            }
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
        let Spanned { span, node } = func;
        Ok(self.root_functions.push(Spanned {
            span,
            node: func::MiddleAstFunction::new(known_funcs, node)
                .map_err(|err| MiddleAstFunctionError::Syntax(err))?,
        }))
    }
}

pub enum MiddleAstFunctionError {
    Syntax(SyntaxError),
    NotAFunction(Spanned<Statement>),
}
