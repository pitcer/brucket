use crate::evaluator::environment::Environment;
use crate::evaluator::internal::InternalFunction;
use brucket_ast::function::ApplicationStrategy;
use brucket_ast::lambda::Parameter;
use brucket_ast::Node;
use derive_more::Constructor;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Null,
    Numeric(Numeric),
    Textual(String),
    Boolean(bool),
    Pair(Box<Value>, Box<Value>),
    Closure(Closure),
    FunctionClosure(ApplicationStrategy, Closure),
    InternalFunctionClosure(InternalFunctionClosure),
    Thunk(Node, Environment),
    Module(bool, String, Environment),
}

impl Display for Value {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Unit => write!(f, "()"),
            Value::Null => write!(f, "null"),
            Value::Numeric(ref numeric) => numeric.fmt(f),
            Value::Textual(ref text) => write!(f, "\"{}\"", text),
            Value::Boolean(ref boolean) => write!(f, "{}", boolean),
            Value::Pair(ref left, ref right) => write!(f, "({}, {})", left, right),
            Value::Closure(_) => write!(f, "(<Closure>)"),
            Value::FunctionClosure(_, _) | Value::InternalFunctionClosure(_) => {
                write!(f, "(<Closure>)")
            }
            Value::Thunk(_, _) => write!(f, "(<Thunk>)"),
            Value::Module(_, _, _) => write!(f, "(<Module>)"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Numeric {
    Integer(i32),
    FloatingPoint(f64),
}

impl Display for Numeric {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Numeric::Integer(ref number) => write!(f, "{}", number),
            Numeric::FloatingPoint(ref number) => write!(f, "{}", number),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Constructor)]
pub struct InternalFunctionClosure {
    pub application_strategy: ApplicationStrategy,
    pub parameters: Vec<Parameter>,
    pub function: InternalFunction,
    pub environment: Environment,
}

#[derive(Debug, PartialEq, Clone, Constructor)]
pub struct Closure {
    pub parameters: Vec<Parameter>,
    pub body: Node,
    pub environment: Environment,
}
