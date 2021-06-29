/*
 * MIT License
 *
 * Copyright (c) 2020 Piotr Dobiech
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use std::fmt::{Debug, Display, Formatter};

use ast_type::Type;
use constant_value::ConstantValue;
use function::InternalFunction;
use path::Path;

use crate::ast::function::Function;
use crate::ast::lambda::Lambda;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u64);

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub node_id: NodeId,
    pub path: Path,
}

impl Identifier {
    pub fn new(node_id: NodeId, path: Path) -> Self {
        Identifier { node_id, path }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Application {
    pub node_id: NodeId,
    pub identifier: Box<Node>,
    pub arguments: Vec<Node>,
}

impl Application {
    pub fn new(node_id: NodeId, identifier: Box<Node>, arguments: Vec<Node>) -> Self {
        Application {
            node_id,
            identifier,
            arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub node_id: NodeId,
    pub name: String,
    pub value_type: Type,
    pub value: Box<Node>,
    pub then: Box<Node>,
}

impl Let {
    pub fn new(
        node_id: NodeId,
        name: String,
        value_type: Type,
        value: Box<Node>,
        then: Box<Node>,
    ) -> Self {
        Let {
            node_id,
            name,
            value_type,
            value,
            then,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub node_id: NodeId,
    pub condition: Box<Node>,
    pub if_true: Box<Node>,
    pub if_false: Box<Node>,
}

impl If {
    pub fn new(
        node_id: NodeId,
        condition: Box<Node>,
        if_true: Box<Node>,
        if_false: Box<Node>,
    ) -> Self {
        If {
            node_id,
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    pub node_id: NodeId,
    pub visibility: Visibility,
    pub name: String,
    pub value: Box<Node>,
}

impl Constant {
    pub fn new(node_id: NodeId, visibility: Visibility, name: String, value: Box<Node>) -> Self {
        Constant {
            node_id,
            visibility,
            name,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }

    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub node_id: NodeId,
    pub is_static: bool,
    pub identifier: String,
    pub functions: Vec<Function>,
    pub internal_functions: Vec<InternalFunction>,
    pub constants: Vec<Constant>,
}

impl Module {
    pub fn new(
        node_id: NodeId,
        is_static: bool,
        identifier: String,
        functions: Vec<Function>,
        internal_functions: Vec<InternalFunction>,
        constants: Vec<Constant>,
    ) -> Self {
        Module {
            node_id,
            is_static,
            identifier,
            functions,
            internal_functions,
            constants,
        }
    }
}
