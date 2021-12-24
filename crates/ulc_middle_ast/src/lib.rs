mod check;
mod expr;
mod func;
mod stmt;

use std::collections::HashMap;

use convert_case::{Case, Casing};
use ulc_ast::{Function, TopLevelStatement};
use ulc_types::{errors::SyntaxError, token::TokenSpan, Spanned, ULCType};

pub use expr::{MiddleAstBinaryOperation, MiddleAstExpression, MiddleAstUnaryOperation};
pub use func::MiddleAstFunction;
pub use stmt::MiddleAstStatement;

pub struct MiddleAstRoot {
    pub root_functions: Vec<MiddleAstFunction>,

    function_names: HashMap<String, FuncData>,
}

pub struct FuncData {
    pub ident: (String, Option<TokenSpan>),
    pub ret_ty: ULCType,
    pub param_tys: Vec<(ULCType, Option<TokenSpan>)>,
}

impl Default for MiddleAstRoot {
    fn default() -> Self {
        Self::new()
    }
}

impl MiddleAstRoot {
    pub fn new() -> Self {
        let function_names = vec![(
            "puts".to_owned(),
            FuncData {
                ident: ("puts".to_owned(), None),
                ret_ty: ULCType::Unit,
                param_tys: vec![(ULCType::String, None)],
            },
        )]
        .into_iter()
        .collect();
        Self {
            root_functions: Vec::new(),
            function_names,
        }
    }

    pub fn append_all_tls(
        &mut self,
        stmts: Vec<Spanned<TopLevelStatement>>,
    ) -> Result<(Vec<Spanned<Function>>, Vec<String>), MiddleAstFunctionError> {
        let mut funcs = Vec::new();
        let mut imports = HashMap::new();
        for Spanned { span, node: tls } in stmts {
            match tls {
                TopLevelStatement::FunctionDefinition(func) => {
                    let idt = Spanned::new(func.ident.span, func.ident.node.to_case(Case::Camel));
                    if let Some(func_data) = self.function_names.get(&idt.node) {
                        return Err(MiddleAstFunctionError(SyntaxError::AlreadyDeclaredIdent {
                            existing: func_data.ident.1,
                            ident: idt.node,
                            new: idt.span,
                            is_function: true,
                        }));
                    }
                    self.function_names.insert(
                        idt.node.clone(),
                        FuncData {
                            ident: (idt.node, Some(idt.span)),
                            ret_ty: func.return_type.node,
                            param_tys: func
                                .params
                                .iter()
                                .map(|a| (a.node.1, Some(a.span)))
                                .collect(),
                        },
                    );

                    funcs.push(Spanned { node: func, span });
                }
                TopLevelStatement::Import(imp) => {
                    if let Some(sp) = imports.get(&imp.node) {
                        return Err(MiddleAstFunctionError(SyntaxError::AlreadyUsedImport {
                            file: imp.node,
                            existing: *sp,
                            new: imp.span,
                        }));
                    } else {
                        imports.insert(imp.node, imp.span);
                    }
                }
            }
        }

        Ok((funcs, imports.into_iter().map(|x| x.0).collect()))
    }

    pub fn translate(
        &mut self,
        outlines: &HashMap<String, Vec<FuncData>>,
        funcs: Vec<Spanned<Function>>,
    ) -> Result<(), MiddleAstFunctionError> {
        for func in funcs {
            self.append_func(outlines, func)?;
        }
        Ok(())
    }

    fn append_func(
        &mut self,
        outlines: &HashMap<String, Vec<FuncData>>,
        func: Spanned<Function>,
    ) -> Result<(), MiddleAstFunctionError> {
        let Spanned { node, .. } = func;
        self.root_functions.push(
            MiddleAstFunction::new(&self.function_names, outlines, node)
                .map_err(MiddleAstFunctionError)?,
        );
        Ok(())
    }
}

pub struct MiddleAstFunctionError(pub SyntaxError);
