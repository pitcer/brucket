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

use crate::ast::ast_type::Type;
use crate::ast::{Node, NodeId};

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub node_id: NodeId,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Box<Node>,
}

impl Lambda {
    pub fn new(
        node_id: NodeId,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Box<Node>,
    ) -> Self {
        Lambda {
            node_id,
            parameters,
            return_type,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub parameter_type: Type,
    pub arity: Arity,
}

impl Parameter {
    pub fn new(name: String, parameter_type: Type, arity: Arity) -> Self {
        Self {
            name,
            parameter_type,
            arity,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Unary,
    Variadic,
}
