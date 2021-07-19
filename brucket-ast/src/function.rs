use crate::ast_type::Type;
use crate::lambda::{Lambda, Parameter};
use crate::{NodeId, Visibility};
use derive_more::{Constructor, IsVariant};

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Function {
    pub node_id: NodeId,
    pub visibility: Visibility,
    pub application_strategy: ApplicationStrategy,
    pub name: String,
    pub body: Lambda,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct InternalFunction {
    pub node_id: NodeId,
    pub visibility: Visibility,
    pub application_strategy: ApplicationStrategy,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, IsVariant)]
pub enum ApplicationStrategy {
    Eager,
    Lazy,
}
