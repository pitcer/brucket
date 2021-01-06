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

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    ConstantValue(ConstantValue),
    Identifier(Path),
    Application(Application),
    Let(Let),
    If(If),
    Lambda(Lambda),
    Module(Module),
    Function(Function),
    InternalFunction(InternalFunction),
    Constant(Constant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub value: Box<Expression>,
    pub then: Box<Expression>,
}

impl Let {
    pub fn new(name: String, value: Box<Expression>, then: Box<Expression>) -> Self {
        Self { name, value, then }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expression>,
    pub if_true: Box<Expression>,
    pub if_false: Box<Expression>,
}

impl If {
    pub fn new(
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    ) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub visibility: Visibility,
    pub application_strategy: ApplicationStrategy,
    pub name: String,
    pub body: Lambda,
}

impl Function {
    pub fn new(
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
        name: String,
        body: Lambda,
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
pub struct Constant {
    pub visibility: Visibility,
    pub name: String,
    pub value: Box<Expression>,
}

impl Constant {
    pub fn new(visibility: Visibility, name: String, value: Box<Expression>) -> Self {
        Self {
            visibility,
            name,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Application {
    pub identifier: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Application {
    pub fn new(identifier: Box<Expression>, arguments: Vec<Expression>) -> Self {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComplexPath {
    pub identifier: String,
    pub path: Vec<String>,
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
pub struct Lambda {
    parameters: Vec<Parameter>,
    return_type: Type,
    body: Box<Expression>,
    used_identifiers: HashSet<String>,
}

impl Lambda {
    pub fn new(
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Box<Expression>,
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

    pub fn body(&self) -> &Expression {
        &self.body
    }

    pub fn used_identifiers(&self) -> &HashSet<String> {
        &self.used_identifiers
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    is_static: bool,
    identifier: String,
    functions: Vec<Function>,
    internal_functions: Vec<InternalFunction>,
    constants: Vec<Constant>,
}

impl Module {
    pub fn new(
        is_static: bool,
        identifier: String,
        functions: Vec<Function>,
        internal_functions: Vec<InternalFunction>,
        constants: Vec<Constant>,
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

    pub fn functions(&self) -> &Vec<Function> {
        &self.functions
    }

    pub fn internal_functions(&self) -> &Vec<InternalFunction> {
        &self.internal_functions
    }

    pub fn constants(&self) -> &Vec<Constant> {
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
