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
    Lambda(LambdaType),
    Symbol(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "unit"),
            Type::Boolean => write!(f, "boolean"),
            Type::Integer => write!(f, "integer"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Lambda(lambda) => lambda.fmt(f),
            Type::Symbol(symbol) => write!(f, "{}", symbol),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Constructor)]
pub struct LambdaType {
    pub parameters_types: Vec<Type>,
    pub return_type: Box<Type>,
}

impl Display for LambdaType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = self
            .parameters_types
            .iter()
            .map(|parameter| parameter.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "({} -> {})", parameters, self.return_type)
    }
}
