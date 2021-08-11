use derive_more::Constructor;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Lambda(Box<LambdaType>),
    Symbol(String),
}

impl Display for Type {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "unit"),
            Type::Boolean => write!(f, "boolean"),
            Type::Integer => write!(f, "integer"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Lambda(ref lambda) => lambda.fmt(f),
            Type::Symbol(ref symbol) => write!(f, "{}", symbol),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Constructor)]
pub struct LambdaType {
    pub parameters_types: Vec<Type>,
    pub return_type: Type,
}

impl Display for LambdaType {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = self
            .parameters_types
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "({} -> {})", parameters, self.return_type)
    }
}
