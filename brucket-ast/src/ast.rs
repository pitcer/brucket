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

use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    ConstantValue(ConstantValue),
    Identifier(Path),
    Application(Application<Expression>),
    Let(Let<Expression>),
    If(If<Expression>),
    Lambda(Lambda<Expression>),
    Module(Module<Expression>),
    Function(Function<Expression>),
    InternalFunction(InternalFunction),
    Constant(Constant<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let<T> {
    pub name: String,
    pub value: Box<T>,
    pub then: Box<T>,
}

impl<T> Let<T> {
    pub fn new(name: String, value: Box<T>, then: Box<T>) -> Self {
        Self { name, value, then }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<T> {
    pub condition: Box<T>,
    pub if_true: Box<T>,
    pub if_false: Box<T>,
}

impl<T> If<T> {
    pub fn new(condition: Box<T>, if_true: Box<T>, if_false: Box<T>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<T> {
    pub visibility: Visibility,
    pub application_strategy: ApplicationStrategy,
    pub name: String,
    pub body: Lambda<T>,
}

impl<T> Function<T> {
    pub fn new(
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
        name: String,
        body: Lambda<T>,
    ) -> Self {
        Self {
            visibility,
            application_strategy,
            name,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalFunction {
    pub visibility: Visibility,
    pub application_strategy: ApplicationStrategy,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

impl InternalFunction {
    pub fn new(
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
    ) -> Self {
        Self {
            visibility,
            application_strategy,
            name,
            parameters,
            return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant<T> {
    pub visibility: Visibility,
    pub name: String,
    pub value: Box<T>,
}

impl<T> Constant<T> {
    pub fn new(visibility: Visibility, name: String, value: Box<T>) -> Self {
        Self {
            visibility,
            name,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Application<T> {
    pub identifier: Box<T>,
    pub arguments: Vec<T>,
}

impl<T> Application<T> {
    pub fn new(identifier: Box<T>, arguments: Vec<T>) -> Self {
        Self {
            identifier,
            arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    Simple(String),
    Complex(ComplexPath),
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Simple(name) => write!(f, "{}", name),
            Path::Complex(path) => std::fmt::Display::fmt(path, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComplexPath {
    pub identifier: String,
    pub path: Vec<String>,
}

impl Display for ComplexPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.path.join("::"), self.identifier)
    }
}

impl ComplexPath {
    pub fn new(identifier: String, path: Vec<String>) -> Self {
        Self { identifier, path }
    }

    pub fn identifier(&self) -> &String {
        &self.identifier
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda<T> {
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Box<T>,
    pub used_identifiers: HashSet<String>,
}

impl<T> Lambda<T> {
    pub fn new(
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Box<T>,
        used_identifiers: HashSet<String>,
    ) -> Self {
        Self {
            parameters,
            return_type,
            body,
            used_identifiers,
        }
    }

    pub fn parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    pub fn used_identifiers(&self) -> &HashSet<String> {
        &self.used_identifiers
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module<T> {
    is_static: bool,
    identifier: String,
    functions: Vec<Function<T>>,
    internal_functions: Vec<InternalFunction>,
    constants: Vec<Constant<T>>,
}

impl<T> Module<T> {
    pub fn new(
        is_static: bool,
        identifier: String,
        functions: Vec<Function<T>>,
        internal_functions: Vec<InternalFunction>,
        constants: Vec<Constant<T>>,
    ) -> Self {
        Self {
            is_static,
            identifier,
            functions,
            internal_functions,
            constants,
        }
    }

    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn identifier(&self) -> &String {
        &self.identifier
    }

    pub fn functions(&self) -> &Vec<Function<T>> {
        &self.functions
    }

    pub fn internal_functions(&self) -> &Vec<InternalFunction> {
        &self.internal_functions
    }

    pub fn constants(&self) -> &Vec<Constant<T>> {
        &self.constants
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Unit,
    Null,
    Numeric(Number),
    Boolean(Boolean),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Integer(String),
    FloatingPoint(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Boolean {
    True,
    False,
}

impl Boolean {
    pub fn to_bool(&self) -> bool {
        match self {
            Boolean::True => true,
            Boolean::False => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub parameter_type: Type,
    pub arity: Arity,
}

#[derive(Debug, Clone, PartialEq)]
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
            Type::Lambda(lambda) => std::fmt::Display::fmt(lambda, f),
            Type::Symbol(symbol) => write!(f, "{}", symbol),
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaType {
    pub parameters_types: Vec<Type>,
    pub return_type: Box<Type>,
}

impl LambdaType {
    pub fn new(parameters_types: Vec<Type>, return_type: Box<Type>) -> Self {
        Self {
            parameters_types,
            return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Unary,
    Variadic,
}

impl Parameter {
    pub fn new(name: String, parameter_type: Type, arity: Arity) -> Self {
        Self {
            name,
            parameter_type,
            arity,
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn parameter_type(&self) -> &Type {
        &self.parameter_type
    }

    pub fn arity(&self) -> &Arity {
        &self.arity
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
pub enum ApplicationStrategy {
    Eager,
    Lazy,
}

impl ApplicationStrategy {
    pub fn is_eager(&self) -> bool {
        matches!(self, ApplicationStrategy::Eager)
    }

    pub fn is_lazy(&self) -> bool {
        matches!(self, ApplicationStrategy::Lazy)
    }
}
