#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Parenthesis(Parenthesis),
    Operator(Operator),
    String(String),
    Boolean(Boolean),
    Number(Number),
    Null,
    Keyword(Keyword),
    Modifier(Modifier),
    PrimitiveType(PrimitiveType),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Parenthesis {
    Open(char),
    Close(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Variadic,
    Path,
    Type,
    SkinnyArrowRight,
    ThickArrowRight,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Boolean {
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Integer(String),
    FloatingPoint(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
    If,
    Lambda,
    Module,
    Function,
    Constant,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    Public,
    Private,
    Lazy,
    Static,
    Internal,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimitiveType {
    Boolean,
    Integer,
    Float,
    String,
    Any,
    Unit,
}
