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
use crate::parser::{Constant, Expression};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::option::Option::Some;
use std::rc::Rc;

#[macro_use]
pub mod environment;
mod internal;

type ValueResult = Result<Value, String>;
type InternalEnvironment = HashMap<&'static str, fn(Vec<Value>) -> ValueResult>;

pub struct Evaluator {
    default_environment: Environment,
    internal_environment: InternalEnvironment,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Numeric(i32),
    Textual(String),
    Boolean(bool),
    Closure(Vec<String>, Expression, Environment),
    Module(String, Environment),
    Identified(String, Rc<Value>),
}

impl Default for Evaluator {
    fn default() -> Self {
        let default_environment = Environment::new();
        Self::new(default_environment)
    }
}

impl Evaluator {
    pub fn new(default_environment: Environment) -> Self {
        let internal_environment = Self::create_internal_environment();
        Self {
            default_environment,
            internal_environment,
        }
    }

    fn create_internal_environment() -> InternalEnvironment {
        let mut environment = InternalEnvironment::new();
        environment.insert("add", internal::add);
        environment.insert("subtract", internal::subtract);
        environment.insert("multiply", internal::multiply);
        environment.insert("divide", internal::divide);
        environment.insert("remainder", internal::remainder);
        environment.insert("is_equal", internal::is_equal);
        environment.insert("is_greater", internal::is_greater);
        environment.insert("is_greater_or_equal", internal::is_greater_or_equal);
        environment.insert("is_less", internal::is_less);
        environment.insert("is_less_or_equal", internal::is_less_or_equal);
        environment
    }

    pub fn evaluate(&self, expression: &Expression) -> ValueResult {
        let mut environment = self.default_environment.clone();
        self.evaluate_environment(expression, &mut environment)
    }

    fn evaluate_environment(
        &self,
        expression: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        match expression {
            Expression::Constant(value) => match value {
                Constant::Unit => Ok(Value::Unit),
                Constant::Numeric(value) => Ok(Value::Numeric(*value as i32)),
                Constant::Boolean(value) => Ok(Value::Boolean(*value)),
                Constant::String(value) => Ok(Value::Textual(value.clone())),
            },
            Expression::Identifier(value) => Self::get_from_environment(environment, value),
            Expression::Application(identifier, arguments) => {
                self.evaluate_application(identifier, arguments, environment)
            }
            Expression::InternalCall(identifier, arguments) => {
                self.evaluate_internal_call(identifier, arguments, environment)
            }
            Expression::Let(name, value, then) => self.evaluate_let(name, value, then, environment),
            Expression::Letrec(name, value, then) => {
                self.evaluate_letrec(name, value, then, environment)
            }
            Expression::If(condition, if_true_then, if_false_then) => {
                self.evaluate_if(condition, if_true_then, if_false_then, environment)
            }
            Expression::Lambda(parameters, body) => {
                Self::evaluate_lambda(parameters, body, environment)
            }
            Expression::Module(identifier, members) => {
                self.evaluate_module(identifier, members, environment)
            }
            Expression::Identified(identifier, value) => {
                self.evaluate_identified(identifier, value, environment)
            }
            Expression::And(arguments) => self.evaluate_and(arguments, environment),
            Expression::Or(arguments) => self.evaluate_or(arguments, environment),
        }
    }

    fn evaluate_let(
        &self,
        identifier: &str,
        value: &Expression,
        then: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let value = self.evaluate_environment(value, environment)?;
        environment.insert(identifier.to_string(), Rc::new(value));
        let result = self.evaluate_environment(then, environment);
        environment.remove(identifier);
        result
    }

    fn evaluate_letrec(
        &self,
        identifier: &str,
        value: &Expression,
        then: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let evaluated_value = self.evaluate_environment(value, environment)?;
        let used_identifiers = Self::get_used_identifiers(value);
        let value = Rc::new(evaluated_value);
        if let Value::Closure(_, _, environment) = value.borrow() {
            if used_identifiers.contains(&&identifier.to_string()) {
                environment.insert_weak(identifier.to_string(), Rc::downgrade(&value));
            }
        }
        environment.insert(identifier.to_string(), value);
        let result = self.evaluate_environment(then, environment);
        environment.remove(identifier);
        result
    }

    fn evaluate_if(
        &self,
        condition: &Expression,
        if_true_then: &Expression,
        if_false_then: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let condition = self.evaluate_environment(condition, environment)?;
        if let Value::Boolean(value) = condition {
            let body = if value { if_true_then } else { if_false_then };
            self.evaluate_environment(body, environment)
        } else {
            Err("Invalid condition type".to_string())
        }
    }

    fn evaluate_lambda(
        parameters: &[String],
        body: &Expression,
        environment: &Environment,
    ) -> ValueResult {
        let environment = Self::get_optimized_lambda_environment(body, environment);
        Ok(Value::Closure(
            parameters.to_vec(),
            body.clone(),
            environment,
        ))
    }

    fn get_optimized_lambda_environment(
        body: &Expression,
        environment: &Environment,
    ) -> Environment {
        let identifiers = Self::get_used_identifiers(body);
        let new_env: Environment = identifiers
            .into_iter()
            .filter_map(|identifier| {
                let value = environment.get(identifier)?;
                Some((identifier.clone(), value))
            })
            .collect();
        new_env.insert_all_weak(environment);
        new_env
    }

    fn get_used_identifiers(expression: &Expression) -> Vec<&String> {
        let mut identifiers = Vec::new();
        match expression {
            Expression::Identifier(identifier) => identifiers.push(identifier),
            Expression::Application(identifier, arguments) => {
                identifiers.append(&mut Self::get_used_identifiers(identifier));
                for argument in arguments {
                    identifiers.append(&mut Self::get_used_identifiers(argument));
                }
            }
            Expression::InternalCall(_, arguments) => {
                for argument in arguments {
                    identifiers.append(&mut Self::get_used_identifiers(argument));
                }
            }
            Expression::Let(_, value, body) => {
                identifiers.append(&mut Self::get_used_identifiers(value));
                identifiers.append(&mut Self::get_used_identifiers(body));
            }
            Expression::Letrec(_, value, body) => {
                identifiers.append(&mut Self::get_used_identifiers(value));
                identifiers.append(&mut Self::get_used_identifiers(body));
            }
            Expression::If(condition, if_true, if_false) => {
                identifiers.append(&mut Self::get_used_identifiers(condition));
                identifiers.append(&mut Self::get_used_identifiers(if_true));
                identifiers.append(&mut Self::get_used_identifiers(if_false));
            }
            Expression::Lambda(_, body) => {
                identifiers.append(&mut Self::get_used_identifiers(body));
            }
            Expression::Module(_, members) => {
                for member in members {
                    identifiers.append(&mut Self::get_used_identifiers(member));
                }
            }
            Expression::Identified(_, body) => {
                identifiers.append(&mut Self::get_used_identifiers(body))
            }
            Expression::Constant(_) => (),
            Expression::And(arguments) => {
                for argument in arguments {
                    identifiers.append(&mut Self::get_used_identifiers(argument));
                }
            }
            Expression::Or(arguments) => {
                for argument in arguments {
                    identifiers.append(&mut Self::get_used_identifiers(argument));
                }
            }
        }
        identifiers
    }

    fn get_from_environment(environment: &Environment, name: &str) -> ValueResult {
        let value = environment
            .get(name)
            .or_else(|| environment.get_weak(name).and_then(|value| value.upgrade()));
        if let Some(value) = value {
            let value: &Value = &value;
            Ok(value.clone())
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }

    fn evaluate_application(
        &self,
        identifier: &Expression,
        arguments: &[Expression],
        environment: &mut Environment,
    ) -> ValueResult {
        let identifier = self.evaluate_environment(identifier, environment)?;
        if let Value::Closure(parameters, body, mut closure_environment) = identifier {
            let parameters_length = parameters.len();
            let arguments_length = arguments.len();
            if parameters_length != arguments_length {
                return Err(format!(
                    "Invalid number of arguments. Expected: {}, Actual: {}",
                    parameters_length, arguments_length
                ));
            }
            let arguments = self.evaluate_arguments(arguments, environment)?;
            for (index, argument) in arguments.into_iter().enumerate() {
                let parameter = &parameters[index];
                closure_environment.insert(parameter.clone(), Rc::new(argument));
            }
            let result = self.evaluate_environment(&body, &mut closure_environment);
            for parameter in &parameters {
                closure_environment.remove(parameter);
            }
            result
        } else {
            Err(format!(
                "Invalid identifier type. Expected: Value::Closure, Actual: {:?}",
                identifier
            ))
        }
    }

    fn evaluate_internal_call(
        &self,
        identifier: &str,
        arguments: &[Expression],
        environment: &mut Environment,
    ) -> ValueResult {
        let function = self.internal_environment.get(identifier);
        if function.is_none() {
            return Err(format!("Undefined internal identifier: {}", identifier));
        }
        let function = function.unwrap();
        let arguments = self.evaluate_arguments(arguments, environment)?;
        function(arguments)
    }

    fn evaluate_arguments(
        &self,
        arguments: &[Expression],
        environment: &mut Environment,
    ) -> Result<Vec<Value>, String> {
        let mut result = Vec::new();
        for argument in arguments {
            let argument = self.evaluate_environment(argument, environment)?;
            result.push(argument);
        }
        Ok(result)
    }

    fn evaluate_identified(
        &self,
        identifier: &str,
        value: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let value = self.evaluate_environment(value, environment)?;
        Ok(Value::Identified(identifier.to_string(), Rc::new(value)))
    }

    fn evaluate_module(
        &self,
        identifier: &str,
        members: &[Expression],
        environment: &mut Environment,
    ) -> ValueResult {
        let mut environment = environment.clone();
        let mut module_environment = Environment::new();
        for member in members {
            let member = self.evaluate_environment(member, &mut environment)?;
            if let Value::Identified(identifier, value) = member {
                environment.insert(identifier.clone(), Rc::clone(&value));
                module_environment.insert(identifier, value);
            } else {
                return Err("Cannot identify module member".to_string());
            }
        }
        Ok(Value::Module(identifier.to_string(), module_environment))
    }

    fn evaluate_and(&self, arguments: &[Expression], environment: &mut Environment) -> ValueResult {
        for argument in arguments {
            let argument = self.evaluate_environment(argument, environment)?;
            if let Value::Boolean(value) = argument {
                if !value {
                    return Ok(Value::Boolean(false));
                }
            } else {
                return Err("Invalid argument type".to_string());
            }
        }
        Ok(Value::Boolean(true))
    }

    fn evaluate_or(&self, arguments: &[Expression], environment: &mut Environment) -> ValueResult {
        for argument in arguments {
            let argument = self.evaluate_environment(argument, environment)?;
            if let Value::Boolean(value) = argument {
                if value {
                    return Ok(Value::Boolean(true));
                }
            } else {
                return Err("Invalid argument type".to_string());
            }
        }
        Ok(Value::Boolean(false))
    }
}

#[cfg(test)]
mod test;
