use std::fmt;

use crate::{BlockStatements, TopLevelStatement};

use super::{Expression, Function, Lit, Statement};

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(s) => s.fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::IfExpr {
                condition,
                true_case,
                false_case,
            } => write!(
                f,
                "(if :cond {} :then ({}) :else ({}))",
                condition,
                join(true_case),
                match false_case {
                    Some(fa_c) => join(fa_c),
                    None => "".to_owned(),
                }
            ),
            Self::FunctionCall {
                function,
                args,
                module,
            } => write!(
                f,
                "({}::{} {})",
                match module {
                    Some(moda) => &moda.node,
                    _ => "",
                },
                function,
                join(args)
            ),
            Self::BinaryOperation(oper) => write!(f, "({})", oper),
            Self::UnaryOperation(oper) => write!(f, "({})", oper),
        }
    }
}

impl fmt::Display for BlockStatements {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(block ({}))", join(&self.0))
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionDefinition(fun) => write!(f, "(define {} {})", fun.ident, fun),
            Self::Const {
                name,
                const_type,
                expr,
            } => {
                write!(f, "(const {}: {} = {})", name, const_type, expr)
            }
            Self::Let {
                name,
                let_type,
                expr,
            } => {
                write!(f, "(let {}: {} = {})", name, let_type, expr)
            }
            Self::Assignment { name, expr } => write!(f, "({} = {})", name, expr),
            Self::UnusedExpression(e) => e.fmt(f),
            Self::ReturnStatement { expression } => write!(f, "return ({})", expression),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, r#""{}""#, s),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            ":params ({}) :body {}",
            self.params
                .iter()
                .map(|para| format!("{}: {}", para.node.0, para.node.1))
                .collect::<Vec<_>>()
                .join(", "),
            join(&self.body)
        )
    }
}

impl fmt::Display for TopLevelStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TopLevelStatement::FunctionDefinition(func) => {
                write!(f, "(define {} {})", func.ident, func)
            }
            TopLevelStatement::Import(file) => write!(f, "(import {})", file),
        }
    }
}

#[inline(always)]
fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}
