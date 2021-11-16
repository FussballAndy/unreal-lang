use std::fmt::Display;

#[derive(Debug, Clone, logos::Logos)]
pub enum LogosTokens {
    #[token("const")]
    Const,
    #[token("let")]
    Let,
    #[token("=")]
    Assign,

    #[token("function")]
    Function,
    #[token("do")]
    Do,

    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,

    #[token("end")]
    End,

    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    SmallerThan,
    #[token(">=")]
    GreaterEquals,
    #[token("<=")]
    SmallerEquals,

    #[token("+")]
    Add,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("!")]
    Not,

    #[token("()")]
    Unit,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex("-?[0-9]+", priority = 2)]
    IntLit,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    StringLit,

    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[regex(r#"[a-z0-9]+(?:_[a-z0-9]+)*"#)]
    Ident,
    #[regex(r#"[A-Z][a-z]*"#)]
    Type,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Functions
    Function,
    Do,

    // If Else Blocks
    If,
    Then,
    Else,

    // General block
    End,

    // Operations
    Add,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Not,

    // Variables
    Const,
    Let,
    Assign,

    // Compare
    Equals,
    NotEquals,
    GreaterThan,
    SmallerThan,
    GreaterEquals,
    SmallerEquals,

    // Literals
    Unit,
    IntLit,
    StringLit,
    True,
    False,

    // General
    Semicolon,
    Comma,
    Colon,
    LeftParen,
    RightParen,

    // Types
    Ident,
    Type,

    // Other
    Error,
    EndOfFile,
}

impl From<LogosTokens> for TokenKind {
    fn from(tok: LogosTokens) -> Self {
        match tok {
            LogosTokens::Assign => Self::Assign,
            LogosTokens::Do => Self::Do,
            LogosTokens::End => Self::End,
            LogosTokens::Else => Self::Else,
            LogosTokens::Equals => Self::Equals,
            LogosTokens::Error => Self::Error,
            LogosTokens::False => Self::False,
            LogosTokens::Function => Self::Function,
            LogosTokens::Ident => Self::Ident,
            LogosTokens::If => Self::If,
            LogosTokens::IntLit => Self::IntLit,
            LogosTokens::Let => Self::Let,
            LogosTokens::StringLit => Self::StringLit,
            LogosTokens::True => Self::True,
            LogosTokens::Type => Self::Type,
            LogosTokens::Unit => Self::Unit,
            LogosTokens::Then => Self::Then,
            LogosTokens::NotEquals => Self::NotEquals,
            LogosTokens::GreaterThan => Self::GreaterThan,
            LogosTokens::SmallerThan => Self::SmallerThan,
            LogosTokens::GreaterEquals => Self::GreaterEquals,
            LogosTokens::SmallerEquals => Self::SmallerEquals,
            LogosTokens::Add => Self::Add,
            LogosTokens::Minus => Self::Minus,
            LogosTokens::Multiply => Self::Multiply,
            LogosTokens::Divide => Self::Divide,
            LogosTokens::And => Self::And,
            LogosTokens::Or => Self::Or,
            LogosTokens::Not => Self::Not,
            LogosTokens::Semicolon => Self::Semicolon,
            LogosTokens::Const => Self::Const,
            LogosTokens::LeftParen => Self::LeftParen,
            LogosTokens::RightParen => Self::RightParen,
            LogosTokens::Colon => Self::Colon,
            LogosTokens::Comma => Self::Comma,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Function => "function",
                Self::Let => "let",
                Self::Do => "do",
                Self::End => "end",
                Self::Equals => "==",
                Self::Assign => "=",
                Self::IntLit => "integer",
                Self::StringLit => "string",
                Self::True => "true",
                Self::False => "false",
                Self::Ident => "ident",
                Self::Type => "type",
                Self::Error => "invalid token",
                Self::EndOfFile => "EOF",
                Self::If => "if",
                Self::Else => "else",
                Self::Unit => "unit",
                Self::Then => "then",
                Self::Add => "+",
                Self::Minus => "-",
                Self::Multiply => "*",
                Self::Divide => "/",
                Self::NotEquals => "!=",
                Self::GreaterThan => ">",
                Self::SmallerThan => "<",
                Self::GreaterEquals => ">=",
                Self::SmallerEquals => "<=",
                Self::And => "and",
                Self::Or => "or",
                Self::Not => "!",
                Self::Semicolon => ";",
                Self::Const => "const",
                Self::LeftParen => "(",
                Self::RightParen => ")",
                Self::Colon => ":",
                Self::Comma => ",",
            }
        )
    }
}
