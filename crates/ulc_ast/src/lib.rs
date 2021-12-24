use ulc_types::Spanned;

use ulc_types::ULCType;

mod impls;
mod oper;

pub use oper::{BinaryOperation, UnaryOperation};

pub type BoxedExpression = Box<Spanned<Expression>>;
pub type Expressions = Vec<Spanned<Expression>>;

pub struct BlockStatements(pub Vec<Spanned<Statement>>);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Literal(Lit),
    Ident(String),
    FunctionCall {
        module: Option<Spanned<String>>,
        function: Spanned<String>,
        args: Expressions,
    },
    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
    IfExpr {
        condition: BoxedExpression,
        true_case: Vec<Spanned<Statement>>,
        false_case: Option<Vec<Spanned<Statement>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Unit,
    Bool(bool),
    Int(i32),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub ident: Spanned<String>,
    pub return_type: Spanned<ULCType>,
    pub params: Vec<Spanned<(String, ULCType)>>,
    pub body: Vec<Spanned<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    FunctionDefinition(Function),
    Const {
        name: Spanned<String>,
        const_type: ULCType,
        expr: BoxedExpression,
    },
    Let {
        name: Spanned<String>,
        let_type: ULCType,
        expr: BoxedExpression,
    },
    Assignment {
        name: Spanned<String>,
        expr: BoxedExpression,
    },
    ReturnStatement {
        expression: BoxedExpression,
    },
    UnusedExpression(BoxedExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TopLevelStatement {
    FunctionDefinition(Function),
    Import(Spanned<String>),
}
