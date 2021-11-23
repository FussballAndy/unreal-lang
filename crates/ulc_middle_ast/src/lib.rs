mod expr;
mod func;
mod stmt;

use func::MiddleAstFunction;
use ulc_ast::Statement;
use ulc_types::Spanned;

pub struct MiddleAstRoot {
    root_functions: Vec<Spanned<MiddleAstFunction>>,
}

impl MiddleAstRoot {
    pub fn new() -> Self {
        Self {
            root_functions: Vec::new(),
        }
    }

    pub fn append_func(&mut self, stmt: Spanned<Statement>) -> anyhow::Result<()> {
        let Spanned { span, node } = stmt; 
        if let Statement::FunctionDefinition(func) = node {
            self.root_functions.push(Spanned {
                span,
                node: func::MiddleAstFunction::new(func)?
            });
            Ok(())
        } else {
            anyhow::bail!("Not a func!")
        }   
    }
}
