#![forbid(unsafe_code)]
#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
    clippy::clone_on_ref_ptr,
    clippy::dbg_macro,
    clippy::exit,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::get_unwrap,
    clippy::if_then_some_else_none,
    clippy::indexing_slicing,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::map_err_ignore,
    clippy::multiple_inherent_impl,
    clippy::panic,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::rc_buffer,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::shadow_reuse,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_to_string,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    clippy::unwrap_in_result,
    clippy::use_debug,
    clippy::verbose_file_reads,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::cargo_common_metadata
)]

use crate::function::Function;
use crate::lambda::Lambda;
use ast_type::Type;
use constant_value::ConstantValue;
use derive_more::{Constructor, Display, IsVariant};
use function::InternalFunction;
use path::Path;
use std::fmt::Debug;

pub mod ast_type;
pub mod constant_value;
pub mod function;
pub mod lambda;
pub mod path;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    ConstantValue(ConstantValue),
    Identifier(Identifier),
    Application(Application),
    Let(Let),
    If(If),
    Lambda(Lambda),
    Function(Function),
    InternalFunction(InternalFunction),
    Constant(Constant),
    Module(Module),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Constructor, Display)]
pub struct NodeId(pub u64);

pub trait NodeIdHolder {
    fn node_id(&self) -> NodeId;
}

impl NodeIdHolder for Node {
    fn node_id(&self) -> NodeId {
        match self {
            Node::ConstantValue(value) => value.node_id,
            Node::Identifier(identifier) => identifier.node_id,
            Node::Application(application) => application.node_id,
            Node::Let(let_node) => let_node.node_id,
            Node::If(if_node) => if_node.node_id,
            Node::Lambda(lambda) => lambda.node_id,
            Node::Function(function) => function.node_id,
            Node::InternalFunction(function) => function.node_id,
            Node::Constant(constant) => constant.node_id,
            Node::Module(module) => module.node_id,
        }
    }
}

macro_rules! impl_node_id_holder {
    ($node_type:ty) => {
        impl NodeIdHolder for $node_type {
            fn node_id(&self) -> NodeId {
                self.node_id
            }
        }
    };
}

impl_node_id_holder!(ConstantValue);
impl_node_id_holder!(Identifier);
impl_node_id_holder!(Application);
impl_node_id_holder!(Let);
impl_node_id_holder!(If);
impl_node_id_holder!(Lambda);
impl_node_id_holder!(Function);
impl_node_id_holder!(InternalFunction);
impl_node_id_holder!(Constant);
impl_node_id_holder!(Module);

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Identifier {
    pub node_id: NodeId,
    pub path: Path,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Application {
    pub node_id: NodeId,
    pub identifier: Box<Node>,
    pub arguments: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Let {
    pub node_id: NodeId,
    pub name: String,
    pub value_type: Type,
    pub value: Box<Node>,
    pub then: Box<Node>,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct If {
    pub node_id: NodeId,
    pub condition: Box<Node>,
    pub if_true: Box<Node>,
    pub if_false: Box<Node>,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Constant {
    pub node_id: NodeId,
    pub visibility: Visibility,
    pub name: String,
    pub value: Box<Node>,
}

#[derive(Debug, Clone, PartialEq, IsVariant)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct Module {
    pub node_id: NodeId,
    pub is_static: bool,
    pub identifier: String,
    pub functions: Vec<Function>,
    pub internal_functions: Vec<InternalFunction>,
    pub constants: Vec<Constant>,
}
