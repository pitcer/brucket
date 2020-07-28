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

use crate::parser::{Call, Constant, Expression, Lambda};
use std::collections::HashMap;
use std::option::Option::Some;

mod internal;

pub type Environment = HashMap<String, Value>;

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
    Closure(Closure),
    Module(String, Environment),
    Identified(String, Box<Value>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Closure {
    Empty(Expression, Environment),
    Parametrized(String, Expression, Environment),
}

impl Evaluator {
    pub fn default() -> Self {
        let default_environment = Environment::new();
        Self::new(default_environment)
    }

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
            Expression::Call(call) => match call {
                Call::Empty(identifier) => self.evaluate_empty_call(identifier, environment),
                Call::Unary(identifier, argument) => {
                    self.evaluate_unary_call(identifier, argument, environment)
                }
                Call::Internal(identifier, arguments) => {
                    self.evaluate_internal_call(identifier, arguments, environment)
                }
            },
            Expression::Let(name, value, then) => self.evaluate_let(name, value, then, environment),
            Expression::If(condition, if_true_then, if_false_then) => {
                self.evaluate_if(condition, if_true_then, if_false_then, environment)
            }
            Expression::Lambda(lambda) => Self::evaluate_lambda(lambda, environment),
            Expression::Module(identifier, members) => {
                self.evaluate_module(identifier, members, environment)
            }
            Expression::Identified(identifier, value) => {
                self.evaluate_identified(identifier, value, environment)
            }
        }
    }

    fn evaluate_let(
        &self,
        name: &str,
        value: &Expression,
        then: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let value = self.evaluate_environment(value, environment)?;
        environment.insert(name.to_string(), value);
        let result = self.evaluate_environment(then, environment);
        environment.remove(name);
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
            if value {
                Ok(self.evaluate_environment(if_true_then, environment)?)
            } else {
                Ok(self.evaluate_environment(if_false_then, environment)?)
            }
        } else {
            Err("Invalid condition type".to_string())
        }
    }

    fn evaluate_lambda(lambda: &Lambda, environment: &Environment) -> ValueResult {
        match lambda {
            Lambda::Empty(body) => Self::evaluate_empty_lambda(body, environment),
            Lambda::Parametrized(parameter, body) => {
                Self::evaluate_parametrized_lambda(parameter, body, environment)
            }
        }
    }

    fn evaluate_empty_lambda(body: &Expression, environment: &Environment) -> ValueResult {
        let environment = Self::get_optimized_lambda_environment(body, environment);
        Ok(Value::Closure(Closure::Empty(body.clone(), environment)))
    }

    fn evaluate_parametrized_lambda(
        parameter: &str,
        body: &Expression,
        environment: &Environment,
    ) -> ValueResult {
        let environment = Self::get_optimized_lambda_environment(body, environment);
        Ok(Value::Closure(Closure::Parametrized(
            parameter.to_string(),
            body.clone(),
            environment,
        )))
    }

    fn get_optimized_lambda_environment(
        body: &Expression,
        environment: &Environment,
    ) -> Environment {
        let identifiers = Self::get_used_identifiers(body);
        identifiers
            .into_iter()
            .filter_map(|identifier| {
                let value = environment.get(identifier)?;
                Some((identifier.clone(), value.clone()))
            })
            .collect()
    }

    fn get_used_identifiers(expression: &Expression) -> Vec<&String> {
        let mut identifiers = Vec::new();
        match expression {
            Expression::Identifier(identifier) => identifiers.push(identifier),
            Expression::Call(call) => match call {
                Call::Empty(identifier) => {
                    identifiers.append(&mut Self::get_used_identifiers(identifier))
                }
                Call::Unary(identifier, argument) => {
                    identifiers.append(&mut Self::get_used_identifiers(identifier));
                    identifiers.append(&mut Self::get_used_identifiers(argument));
                }
                Call::Internal(_, arguments) => {
                    for argument in arguments {
                        identifiers.append(&mut Self::get_used_identifiers(argument));
                    }
                }
            },
            Expression::Let(_, value, body) => {
                identifiers.append(&mut Self::get_used_identifiers(value));
                identifiers.append(&mut Self::get_used_identifiers(body));
            }
            Expression::If(condition, if_true, if_false) => {
                identifiers.append(&mut Self::get_used_identifiers(condition));
                identifiers.append(&mut Self::get_used_identifiers(if_true));
                identifiers.append(&mut Self::get_used_identifiers(if_false));
            }
            Expression::Lambda(lambda) => match lambda {
                Lambda::Empty(body) => {
                    identifiers.append(&mut Self::get_used_identifiers(body));
                }
                Lambda::Parametrized(_, body) => {
                    identifiers.append(&mut Self::get_used_identifiers(body));
                }
            },
            Expression::Module(_, members) => {
                for member in members {
                    identifiers.append(&mut Self::get_used_identifiers(member));
                }
            }
            Expression::Identified(_, body) => {
                identifiers.append(&mut Self::get_used_identifiers(body))
            }
            _ => (),
        }
        identifiers
    }

    fn get_from_environment(environment: &Environment, name: &str) -> ValueResult {
        let value = environment.get(name);
        if let Some(value) = value {
            Ok(value.clone())
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }

    fn evaluate_empty_call(
        &self,
        identifier: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let identifier = self.evaluate_environment(identifier, environment)?;
        if let Value::Closure(closure) = identifier {
            if let Closure::Empty(body, mut environment) = closure {
                self.evaluate_environment(&body, &mut environment)
            } else {
                Err("Invalid closure type".to_string())
            }
        } else {
            Err("Invalid identifier type".to_string())
        }
    }

    fn evaluate_unary_call(
        &self,
        identifier: &Expression,
        argument: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let identifier = self.evaluate_environment(identifier, environment)?;
        if let Value::Closure(closure) = identifier {
            if let Closure::Parametrized(parameter, body, mut closure_environment) = closure {
                let argument = self.evaluate_environment(argument, environment)?;
                closure_environment.insert(parameter.clone(), argument);
                let result = self.evaluate_environment(&body, &mut closure_environment);
                closure_environment.remove(&parameter);
                result
            } else {
                Err("Invalid closure type".to_string())
            }
        } else {
            Err("Invalid identifier type".to_string())
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
        let mut values = Vec::new();
        for argument in arguments {
            let value = self.evaluate_environment(argument, environment)?;
            values.push(value);
        }
        function(values)
    }

    fn evaluate_identified(
        &self,
        identifier: &str,
        value: &Expression,
        environment: &mut Environment,
    ) -> ValueResult {
        let value = self.evaluate_environment(value, environment)?;
        Ok(Value::Identified(identifier.to_string(), Box::from(value)))
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
                environment.insert(identifier.clone(), *value.clone());
                module_environment.insert(identifier, *value);
            } else {
                return Err("Cannot identify module member".to_string());
            }
        }
        Ok(Value::Module(identifier.to_string(), module_environment))
    }
}

#[cfg(test)]
mod test;
