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

use std::borrow::Cow;
use std::collections::HashMap;

use crate::analyzer::type_analyzer::typed_expression::{TypedExpression, TypedExpressionType};
use crate::ast::{Application, ConstantValue, If, Lambda, LambdaType, Let, Number, Path, Type};
use crate::parser::expression::Expression;

#[cfg(test)]
mod test;
pub mod typed_expression;

#[derive(Debug)]
pub struct Environment {
    pub variables: HashMap<Path, Type>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            variables: HashMap::default(),
        }
    }
}

impl Environment {
    fn insert_variable(&mut self, path: Path, variable_type: Type) {
        self.variables.insert(path, variable_type);
    }

    fn remove_variable(&mut self, path: &Path) -> Option<Type> {
        self.variables.remove(path)
    }

    fn get_variable(&self, path: &Path) -> Option<&Type> {
        self.variables.get(path)
    }
}

pub type TypedError = Cow<'static, str>;
pub type TypedResult = Result<TypedExpression, TypedError>;

pub trait Typed {
    fn into_typed(self, environment: &mut Environment) -> TypedResult;
}

impl Typed for ConstantValue {
    fn into_typed(self, _environment: &mut Environment) -> TypedResult {
        let constant_value_type = match &self {
            ConstantValue::Unit => Type::Unit,
            ConstantValue::Null => Type::Any,
            ConstantValue::Numeric(number) => match number {
                Number::Integer(_) => Type::Integer,
                Number::FloatingPoint(_) => Type::Float,
            },
            ConstantValue::Boolean(_) => Type::Boolean,
            ConstantValue::String(_) => Type::String,
        };
        let expression = TypedExpressionType::ConstantValue(self);
        let expression = TypedExpression::new(expression, constant_value_type);
        Ok(expression)
    }
}

impl Typed for Path {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        let path_type = environment
            .get_variable(&self)
            .ok_or_else(|| format!("Failed to get type of variable {}", self))?
            .clone();
        let expression = TypedExpressionType::Identifier(self);
        let expression = TypedExpression::new(expression, path_type);
        Ok(expression)
    }
}

impl Typed for Application<Expression> {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        let typed_identifier = self.identifier.into_typed(environment)?;
        if let Type::Lambda(lambda_type) = typed_identifier.evaluated_type.clone() {
            let typed_arguments = self
                .arguments
                .into_iter()
                .map(|argument| argument.into_typed(environment))
                .collect::<Result<Vec<TypedExpression>, TypedError>>()?;
            let application = Application::new(Box::new(typed_identifier), typed_arguments);
            let expression = TypedExpressionType::Application(application);
            let expression = TypedExpression::new(expression, *lambda_type.return_type);
            Ok(expression)
        } else {
            Err("Application identifier must evaluate to Lambda".into())
        }
    }
}

impl Typed for Lambda<Expression> {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        // TODO: reduce number of clones
        // ---
        for parameter in &self.parameters {
            environment.insert_variable(
                Path::Simple(parameter.name.to_owned()),
                parameter.parameter_type.clone(),
            );
        }
        let typed_body = self.body.into_typed(environment)?;
        for parameter in &self.parameters {
            environment.remove_variable(&Path::Simple(parameter.name.clone()));
        }
        // ---
        // TODO: get parameters types from usage context
        let parameters_types = self
            .parameters
            .iter()
            .map(|parameter| parameter.parameter_type.clone())
            .collect::<Vec<Type>>();
        let lambda_type = LambdaType::new(
            parameters_types,
            Box::new(typed_body.evaluated_type.clone()),
        );
        let lambda = Lambda::new(
            self.parameters,
            self.return_type,
            typed_body.into(),
            self.used_identifiers,
        );
        let expression = TypedExpressionType::Lambda(lambda);
        let expression = TypedExpression::new(expression, Type::Lambda(lambda_type));
        Ok(expression)
    }
}

impl Typed for Let<Expression> {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        let typed_value = self.value.into_typed(environment)?;
        let path = Path::Simple(self.name.clone());
        let value_type = typed_value.evaluated_type.clone();
        environment.insert_variable(path.clone(), value_type);
        let typed_then = self.then.into_typed(environment)?;
        environment.remove_variable(&path);
        let then_type = typed_then.evaluated_type.clone();
        let typed_let = Let::new(self.name, typed_value.into(), typed_then.into());
        let expression = TypedExpressionType::Let(typed_let);
        let expression = TypedExpression::new(expression, then_type);
        Ok(expression)
    }
}

impl Typed for If<Expression> {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        let typed_then = self.if_true.into_typed(environment)?;
        let typed_else = self.if_false.into_typed(environment)?;
        let then_type = typed_then.evaluated_type.clone();
        if then_type == typed_else.evaluated_type {
            let typed_condition = self.condition.into_typed(environment)?;
            let typed_if = If::new(typed_condition.into(), typed_then.into(), typed_else.into());
            let expression = TypedExpressionType::If(typed_if);
            let expression = TypedExpression::new(expression, then_type);
            Ok(expression)
        } else {
            Err(format!(
                "Types in both if expression's branches must be the same, actual: {}, {}",
                then_type, typed_else.evaluated_type
            )
            .into())
        }
    }
}

impl Typed for Expression {
    fn into_typed(self, environment: &mut Environment) -> TypedResult {
        match self {
            Expression::ConstantValue(value) => value.into_typed(environment),
            Expression::Identifier(identifier) => identifier.into_typed(environment),
            Expression::Application(application) => application.into_typed(environment),
            Expression::Let(let_expression) => let_expression.into_typed(environment),
            Expression::If(if_expression) => if_expression.into_typed(environment),
            Expression::Lambda(lambda) => lambda.into_typed(environment),
            Expression::Module(_) => unimplemented!(),
            Expression::Function(_) => unimplemented!(),
            Expression::Constant(_) => unimplemented!(),
            Expression::InternalFunction(_) => unimplemented!(),
        }
    }
}
