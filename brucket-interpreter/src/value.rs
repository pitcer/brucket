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
    Thunk(Box<Node>, Environment),
    Module(bool, String, Environment),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Null => write!(f, "null"),
            Value::Numeric(numeric) => numeric.fmt(f),
            Value::Textual(text) => write!(f, "\"{}\"", text),
            Value::Boolean(boolean) => write!(f, "{}", boolean),
            Value::Pair(left, right) => write!(f, "({}, {})", left, right),
            Value::Closure(_) => write!(f, "(<Closure>)"),
            Value::FunctionClosure(_, _) => write!(f, "(<Closure>)"),
            Value::InternalFunctionClosure(_) => write!(f, "(<Closure>)"),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Integer(number) => write!(f, "{}", number),
            Numeric::FloatingPoint(number) => write!(f, "{}", number),
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
    pub body: Box<Node>,
    pub environment: Environment,
}
