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

use crate::analyzer::ast::{
    Application, ApplicationIdentifier, ConstantValue, Expression, If, Lambda, Let, Number, Path,
};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Lambda(LambdaType),
}

impl From<&crate::analyzer::ast::Type> for Type {
    fn from(ast_type: &crate::analyzer::ast::Type) -> Self {
        match ast_type {
            crate::analyzer::ast::Type::Boolean => Type::Boolean,
            crate::analyzer::ast::Type::Integer => Type::Integer,
            crate::analyzer::ast::Type::String => Type::String,
            crate::analyzer::ast::Type::Any => Type::Unknown,
            crate::analyzer::ast::Type::Symbol(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaType {
    pub return_type: Box<Type>,
    pub parameters_types: Vec<Type>,
}

#[derive(Debug)]
pub struct Environment {
    pub variables: HashMap<Path, Type>,
}

impl Environment {
    fn insert_variable(&mut self, path: Path, variable_type: Type) {
        self.variables.insert(path, variable_type);
    }

    fn remove_variable(&mut self, path: &Path) {
        self.variables.remove(path);
    }

    fn get_variable(&self, path: &Path) -> Option<Type> {
        self.variables.get(path).cloned()
    }
}

pub type TypedResult = Result<Type, Cow<'static, str>>;

pub trait Typed {
    // TODO: return TypedExpression
    fn get_type(&self, environment: &mut Environment) -> TypedResult;
}

impl Typed for ConstantValue {
    fn get_type(&self, _environment: &mut Environment) -> TypedResult {
        Ok(match self {
            ConstantValue::Unit => Type::Unit,
            ConstantValue::Null => Type::Unknown,
            ConstantValue::Numeric(number) => match number {
                Number::Integer(_) => Type::Integer,
                Number::FloatingPoint(_) => Type::Float,
            },
            ConstantValue::Boolean(_) => Type::Boolean,
            ConstantValue::String(_) => Type::String,
        })
    }
}

impl Typed for Path {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        environment
            .get_variable(self)
            .ok_or_else(|| format!("Undefined variable: {:?}", self).into())
    }
}

impl Typed for Application {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        match &self.identifier {
            ApplicationIdentifier::Identifier(path) => path.get_type(environment),
            ApplicationIdentifier::Lambda(lambda) => lambda.get_type(environment),
        }
    }
}

impl Typed for Lambda {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        let return_type = self.body().get_type(environment)?;
        // TODO: get parameters types from usage context
        let parameters_types = self
            .parameters()
            .iter()
            .map(|parameter| parameter.parameter_type().into())
            .collect::<Vec<Type>>();
        Ok(Type::Lambda(LambdaType {
            return_type: Box::new(return_type),
            parameters_types,
        }))
    }
}

impl Typed for Let {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        let value_type = self.value.get_type(environment)?;
        let path = Path::Simple(self.name.clone());
        environment.insert_variable(path.clone(), value_type);
        let then_type = self.then.get_type(environment);
        environment.remove_variable(&path);
        then_type
    }
}

impl Typed for If {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        let true_type = self.if_true.get_type(environment)?;
        let false_type = self.if_false.get_type(environment)?;
        if true_type == false_type {
            Ok(true_type)
        } else {
            Err(format!(
                "Types in if expression must be the same, actual: {:?}, {:?}",
                true_type, false_type
            )
            .into())
        }
    }
}

impl Typed for Expression {
    fn get_type(&self, environment: &mut Environment) -> TypedResult {
        match self {
            Expression::ConstantValue(value) => value.get_type(environment),
            Expression::Identifier(identifier) => identifier.get_type(environment),
            Expression::Application(application) => application.get_type(environment),
            Expression::Let(let_expression) => let_expression.get_type(environment),
            Expression::If(if_expression) => if_expression.get_type(environment),
            Expression::Lambda(lambda) => lambda.get_type(environment),
            Expression::Module(_) => unimplemented!(),
            Expression::Function(_) => unimplemented!(),
            Expression::Constant(_) => unimplemented!(),
            Expression::InternalFunction(_) => unimplemented!(),
        }
    }
}
