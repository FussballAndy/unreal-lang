use ulc_types::{token_kind::TokenKind, Spanned};

use ulc_types::ULCType;

mod impls;

pub type BoxedExpression = Box<Spanned<Expression>>;
pub type Expressions = Vec<Spanned<Expression>>;

pub struct BlockStatements(pub Vec<Spanned<Statement>>);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Literal(Lit),
    Ident(String),
    FunctionCall {
        function: Spanned<String>,
        args: Expressions,
    },
    BinaryOperation {
        op: TokenKind,
        lhs: BoxedExpression,
        rhs: BoxedExpression,
    },
    UnaryOperation {
        op: TokenKind,
        expr: BoxedExpression,
    },
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
    pub return_type: ULCType,
    pub params: Vec<(String, ULCType)>,
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
