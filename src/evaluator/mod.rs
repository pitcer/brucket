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

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::option::Option::Some;
use std::rc::Rc;
use std::slice::Iter;

use crate::evaluator::environment::Environment;
use crate::evaluator::internal::InternalEnvironment;
use crate::interpreter::ModuleEnvironment;
use crate::parser::{
    ApplicationStrategy, ConstantValue, Expression, Lambda, Module, Parameter, Path,
};

#[macro_use]
pub mod environment;
mod internal;

type ValueResult = Result<Value, String>;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Null,
    Numeric(i32),
    Textual(String),
    Boolean(bool),
    Pair(Box<Value>, Box<Value>),
    Closure(Closure),
    FunctionClosure(ApplicationStrategy, Closure),
    Thunk(Box<Expression>, Environment),
    Module(String, Environment),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    parameters: Vec<Parameter>,
    body: Box<Expression>,
    environment: Environment,
}

impl Closure {
    pub fn new(
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        environment: Environment,
    ) -> Self {
        Self {
            parameters,
            body,
            environment,
        }
    }

    pub fn environment(&self) -> &Environment {
        &self.environment
    }
}

pub struct Evaluator {
    internal_environment: InternalEnvironment,
}

impl Default for Evaluator {
    fn default() -> Self {
        let internal_environment = InternalEnvironment::default();
        Evaluator::new(internal_environment)
    }
}

impl Evaluator {
    fn new(internal_environment: InternalEnvironment) -> Self {
        Self {
            internal_environment,
        }
    }

    #[cfg(test)]
    fn evaluate(&self, expression: &Expression) -> ValueResult {
        let module_environment = ModuleEnvironment::default();
        self.evaluate_with_module_environment(expression, &module_environment)
    }

    pub fn evaluate_with_module_environment(
        &self,
        expression: &Expression,
        module_environment: &ModuleEnvironment,
    ) -> ValueResult {
        let mut environment = Environment::default();
        self.evaluate_environment(expression, module_environment, &mut environment)
    }

    fn evaluate_environment(
        &self,
        expression: &Expression,
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        match expression {
            Expression::ConstantValue(value) => match value {
                ConstantValue::Unit => Ok(Value::Unit),
                ConstantValue::Null => Ok(Value::Null),
                ConstantValue::Numeric(value) => Ok(Value::Numeric(*value as i32)),
                ConstantValue::Boolean(value) => Ok(Value::Boolean(*value)),
                ConstantValue::String(value) => Ok(Value::Textual(value.clone())),
            },
            Expression::Identifier(value) => {
                Self::get_from_environment(module_environment, environment, value)
            }
            Expression::Application(identifier, arguments) => {
                self.evaluate_application(identifier, arguments, module_environment, environment)
            }
            Expression::InternalCall(identifier, arguments) => {
                self.evaluate_internal_call(identifier, arguments, module_environment, environment)
            }
            Expression::Let(name, value, then) => {
                self.evaluate_let(name, value, then, module_environment, environment)
            }
            Expression::If(condition, if_true_then, if_false_then) => self.evaluate_if(
                condition,
                if_true_then,
                if_false_then,
                module_environment,
                environment,
            ),
            Expression::Lambda(lambda) => {
                Ok(Value::Closure(Self::evaluate_lambda(lambda, environment)))
            }
            Expression::Module(module) => {
                self.evaluate_module(module, module_environment, environment)
            }
            Expression::Function(_, application_strategy, _, lambda) => Ok(Value::FunctionClosure(
                application_strategy.clone(),
                Self::evaluate_lambda(lambda, environment),
            )),
            Expression::Constant(_, _, value) => {
                self.evaluate_environment(value, module_environment, environment)
            }
        }
    }

    fn evaluate_let(
        &self,
        identifier: &str,
        value: &Expression,
        then: &Expression,
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let evaluated_value = self.evaluate_environment(value, module_environment, environment)?;
        let evaluated_value = Rc::new(evaluated_value);
        if let Expression::Lambda(lambda) = value {
            if let Value::Closure(closure) = evaluated_value.borrow() {
                let identifier = identifier.to_string();
                if lambda.used_identifiers().contains(&identifier) {
                    let closure_environment = closure.environment();
                    closure_environment.insert_weak(identifier, Rc::downgrade(&evaluated_value));
                }
            }
        }
        environment.insert(identifier.to_string(), evaluated_value);
        let result = self.evaluate_environment(then, module_environment, environment);
        environment.remove(identifier);
        result
    }

    fn evaluate_if(
        &self,
        condition: &Expression,
        if_true_then: &Expression,
        if_false_then: &Expression,
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let condition = self.evaluate_environment(condition, module_environment, environment)?;
        if let Value::Boolean(value) = condition {
            let body = if value { if_true_then } else { if_false_then };
            self.evaluate_environment(body, module_environment, environment)
        } else {
            Err("Invalid condition type".to_string())
        }
    }

    fn evaluate_lambda(lambda: &Lambda, environment: &Environment) -> Closure {
        let environment = Self::get_optimized_lambda_environment(lambda, environment);
        Closure::new(
            lambda.parameters().clone(),
            Box::from(lambda.body().clone()),
            environment,
        )
    }

    fn get_optimized_lambda_environment(lambda: &Lambda, environment: &Environment) -> Environment {
        let identifiers = lambda.used_identifiers();
        let new_env: Environment = identifiers
            .iter()
            .filter_map(|identifier| {
                let value = environment.get(identifier)?;
                Some((identifier.clone(), value))
            })
            .collect();
        new_env.insert_all_weak(environment);
        new_env
    }

    fn get_from_environment(
        module_environment: &ModuleEnvironment,
        environment: &Environment,
        path: &Path,
    ) -> ValueResult {
        match path {
            Path::Simple(identifier) => {
                for module in module_environment.iter() {
                    if module.0.eq("base") {
                        let value = module.1.get(identifier).or_else(|| {
                            environment
                                .get_weak(identifier)
                                .and_then(|value| value.upgrade())
                        });
                        if let Some(value) = value {
                            let value: &Value = &value;
                            return Ok(value.clone());
                        }
                    }
                }
                let value = environment.get(identifier).or_else(|| {
                    environment
                        .get_weak(identifier)
                        .and_then(|value| value.upgrade())
                });
                if let Some(value) = value {
                    let value: &Value = &value;
                    Ok(value.clone())
                } else {
                    Err(format!("Undefined variable: {}", identifier))
                }
            }
            Path::Complex(path) => {
                let first_path = path.path().get(0);
                if let Some(first_path) = first_path {
                    let module_env = module_environment.get(first_path);
                    if let Some(module_env) = module_env {
                        Self::get_from_environment(
                            module_environment,
                            module_env,
                            &Path::Simple(path.identifier().clone()),
                        )
                    } else {
                        Err("".to_string())
                    }
                } else {
                    Err("".to_string())
                }
            }
        }
    }

    fn evaluate_application(
        &self,
        identifier: &Expression,
        arguments: &[Expression],
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let identifier = self.evaluate_environment(identifier, module_environment, environment)?;
        match identifier {
            Value::Closure(closure) => self.evaluate_closure_application(
                ApplicationStrategy::Eager,
                arguments,
                module_environment,
                environment,
                closure,
            ),
            Value::FunctionClosure(application_strategy, closure) => self
                .evaluate_closure_application(
                    application_strategy,
                    arguments,
                    module_environment,
                    environment,
                    closure,
                ),
            Value::Thunk(body, mut environment) => {
                self.evaluate_environment(&body, module_environment, &mut environment)
            }
            _ => Err(format!(
                "Invalid identifier type. Expected: Closure or Thunk, Actual: {:?}",
                identifier
            )),
        }
    }

    fn evaluate_closure_application(
        &self,
        application_strategy: ApplicationStrategy,
        arguments: &[Expression],
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
        mut closure: Closure,
    ) -> Result<Value, String> {
        let mut arguments_iterator = arguments.iter();
        let mut has_variadic_parameter = false;
        for parameter in &closure.parameters {
            match parameter {
                Parameter::Unary(name) => {
                    let argument = arguments_iterator
                        .next()
                        .ok_or_else(|| "Missing argument.".to_string())?;
                    let argument = if application_strategy.is_eager() {
                        self.evaluate_environment(argument, module_environment, environment)
                    } else {
                        Ok(Value::Thunk(
                            Box::new(argument.clone()),
                            environment.clone(),
                        ))
                    }?;
                    closure.environment.insert(name.clone(), Rc::new(argument));
                }
                Parameter::Variadic(name) => {
                    let list = self.create_pair_list(
                        application_strategy,
                        arguments_iterator,
                        module_environment,
                        environment,
                    )?;
                    closure.environment.insert(name.clone(), Rc::new(list));
                    has_variadic_parameter = true;
                    break;
                }
            }
        }
        let parameters_length = closure.parameters.len();
        let arguments_length = arguments.len();
        if !has_variadic_parameter && parameters_length != arguments_length {
            return Err(format!(
                "Invalid number of arguments. Expected: {}, Actual: {}",
                parameters_length, arguments_length
            ));
        }
        let result =
            self.evaluate_environment(&closure.body, module_environment, &mut closure.environment);
        for parameter in &closure.parameters {
            let name = parameter.name();
            closure.environment.remove(name);
        }
        result
    }

    fn create_pair_list(
        &self,
        application_strategy: ApplicationStrategy,
        arguments: Iter<Expression>,
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let mut result = Value::Null;
        for argument in arguments.rev() {
            let argument = if application_strategy.is_eager() {
                self.evaluate_environment(argument, module_environment, environment)
            } else {
                Ok(Value::Thunk(
                    Box::new(argument.clone()),
                    environment.clone(),
                ))
            }?;
            result = Value::Pair(Box::new(argument), Box::new(result));
        }
        Ok(result)
    }

    fn evaluate_internal_call(
        &self,
        identifier: &str,
        arguments: &[Expression],
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let function = self.internal_environment.get(identifier);
        if function.is_none() {
            return Err(format!("Undefined internal identifier: {}", identifier));
        }
        let function = function.unwrap();
        let arguments = self.evaluate_arguments(arguments, module_environment, environment)?;
        function(arguments)
    }

    fn evaluate_arguments(
        &self,
        arguments: &[Expression],
        module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> Result<Vec<Value>, String> {
        let mut result = Vec::new();
        for argument in arguments {
            let argument = self.evaluate_environment(argument, module_environment, environment)?;
            result.push(argument);
        }
        Ok(result)
    }

    fn evaluate_module(
        &self,
        module: &Module,
        global_module_environment: &ModuleEnvironment,
        environment: &mut Environment,
    ) -> ValueResult {
        let module_environment = Environment::new();
        let mut constants_environment = environment.clone();
        let mut closures = Vec::new();
        let mut evaluated_closures = HashMap::new();
        for function in module.functions() {
            if let Expression::Function(visibility, application_strategy, identifier, lambda) =
                &function
            {
                let closure = Self::evaluate_lambda(lambda, environment);
                let closure = Value::FunctionClosure(application_strategy.clone(), closure);
                let closure = Rc::new(closure);
                if visibility.is_public() {
                    module_environment.insert(identifier.clone(), Rc::clone(&closure));
                }
                constants_environment.insert(identifier.clone(), Rc::clone(&closure));
                let used_identifiers = lambda.used_identifiers();
                closures.push((identifier, Rc::clone(&closure), used_identifiers));
                evaluated_closures.insert(identifier, closure);
            } else {
                return Err("Invalid function type".to_string());
            }
        }
        Self::fill_closures(&closures, &evaluated_closures)?;
        let mut evaluated_constants = HashMap::new();
        for constant in module.constants() {
            if let Expression::Constant(visibility, identifier, value) = constant {
                let value = self.evaluate_environment(
                    value,
                    global_module_environment,
                    &mut constants_environment,
                )?;
                let value = Rc::new(value);
                if visibility.is_public() {
                    module_environment.insert(identifier.clone(), Rc::clone(&value));
                }
                constants_environment.insert(identifier.clone(), Rc::clone(&value));
                evaluated_constants.insert(identifier, value);
            } else {
                return Err("Invalid constant type".to_string());
            }
        }
        Self::fill_closures(&closures, &evaluated_constants)?;
        let identifier = module.identifier();
        Ok(Value::Module(identifier.to_string(), module_environment))
    }

    fn fill_closures(
        closures: &[(&String, Rc<Value>, &HashSet<String>)],
        values: &HashMap<&String, Rc<Value>>,
    ) -> Result<(), String> {
        for (identifier, closure, used_identifiers) in closures {
            if let Value::FunctionClosure(_, closure) = closure.borrow() {
                let environment = closure.environment();
                for used_identifier in used_identifiers.iter() {
                    if let Some(evaluated_value) = values.get(used_identifier) {
                        if &used_identifier == identifier {
                            let value = Rc::downgrade(evaluated_value);
                            environment.insert_weak(used_identifier.clone(), value);
                        } else {
                            let value = Rc::clone(evaluated_value);
                            environment.insert(used_identifier.clone(), value);
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test;
