use crate::ast_type::Type;
use crate::{Node, NodeId};
use derive_more::Constructor;

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Lambda {
    pub node_id: NodeId,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Box<Node>,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Parameter {
    pub name: String,
    pub parameter_type: Type,
    pub arity: Arity,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Unary,
    Variadic,
}
