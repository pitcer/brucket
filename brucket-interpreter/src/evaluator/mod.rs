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

use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::option::Option::Some;
use std::rc::Rc;
use std::slice::Iter;

use brucket_ast::ast::{
    Application, ApplicationStrategy, Arity, Constant, ConstantValue, Function, If,
    InternalFunction, Lambda, Let, Module, Number, Path,
};
use brucket_ast::parser::expression::Expression;

use crate::evaluator::environment::Environment;
use crate::evaluator::internal::InternalEnvironment;
use crate::interpreter::ModuleEnvironment;
use crate::value::{Closure, InternalFunctionClosure, Numeric, Value};

#[macro_use]
pub mod environment;
pub mod internal;
#[cfg(test)]
mod test;

type ValueError = Cow<'static, str>;
type ValueResult = Result<Value, ValueError>;

pub struct Evaluator {
    internal_environment: InternalEnvironment,
}

impl Default for Evaluator {
    fn default() -> Self {
        let internal_environment = InternalEnvironment::default();
        Self::new(internal_environment)
    }
}

impl Evaluator {
    fn new(internal_environment: InternalEnvironment) -> Self {
        Self {
            internal_environment,
        }
    }

    #[cfg(test)]
    fn evaluate(&mut self, expression: &Expression) -> ValueResult {
        let module_environment = ModuleEnvironment::default();
        let static_module_environment = Environment::new();
        self.evaluate_with_module_environment(
            expression,
            &static_module_environment,
            &module_environment,
        )
    }

    pub fn evaluate_with_module_environment(
        &mut self,
        expression: &Expression,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
    ) -> ValueResult {
        let environment = Environment::default();
        self.evaluate_environment(
            expression,
            static_module_environment,
            module_environment,
            &environment,
        )
    }

    fn evaluate_environment(
        &mut self,
        expression: &Expression,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        match expression {
            Expression::ConstantValue(value) => match value {
                ConstantValue::Unit => Ok(Value::Unit),
                ConstantValue::Null => Ok(Value::Null),
                ConstantValue::Numeric(value) => {
                    let value = match value {
                        Number::Integer(value) => Numeric::Integer(
                            value
                                .parse::<i32>()
                                .map_err(|_| "Error while parsing to i32".to_string())?,
                        ),
                        Number::FloatingPoint(value) => Numeric::FloatingPoint(
                            value
                                .parse::<f64>()
                                .map_err(|_| "Error while parsing to f64".to_string())?,
                        ),
                    };
                    Ok(Value::Numeric(value))
                }
                ConstantValue::Boolean(value) => Ok(Value::Boolean(value.to_bool())),
                ConstantValue::String(value) => Ok(Value::Textual(value.clone())),
            },
            Expression::Identifier(value) => Self::get_from_environment(
                static_module_environment,
                module_environment,
                environment,
                value,
            ),
            Expression::Application(Application {
                identifier,
                arguments,
            }) => self.evaluate_application(
                identifier,
                arguments,
                static_module_environment,
                module_environment,
                environment,
            ),
            Expression::Let(Let { name, value, then }) => self.evaluate_let(
                name,
                value,
                then,
                static_module_environment,
                module_environment,
                environment,
            ),
            Expression::If(If {
                condition,
                if_true,
                if_false,
            }) => self.evaluate_if(
                condition,
                if_true,
                if_false,
                static_module_environment,
                module_environment,
                environment,
            ),
            Expression::Lambda(lambda) => {
                Ok(Value::Closure(Self::evaluate_lambda(lambda, environment)))
            }
            Expression::Module(module) => self.evaluate_module(
                module,
                static_module_environment,
                module_environment,
                environment,
            ),
            Expression::Function(Function {
                visibility: _,
                application_strategy,
                name: _,
                body: lambda,
            }) => Ok(Value::FunctionClosure(
                application_strategy.clone(),
                Self::evaluate_lambda(lambda, environment),
            )),
            Expression::InternalFunction(internal_function) => {
                self.evaluate_internal_function(environment, internal_function)
            }
            Expression::Constant(Constant {
                visibility: _,
                name: _,
                value,
            }) => self.evaluate_environment(
                value,
                static_module_environment,
                module_environment,
                environment,
            ),
        }
    }

    fn evaluate_internal_function(
        &mut self,
        environment: &Environment,
        internal_function: &InternalFunction,
    ) -> ValueResult {
        let env = Environment::default();
        env.insert_all_weak(environment);
        let function = self
            .internal_environment
            .get(&internal_function.name)
            .ok_or(format!(
                "Undefined internal identifier: {}",
                internal_function.name
            ))?;
        Ok(Value::InternalFunctionClosure(
            InternalFunctionClosure::new(
                internal_function.application_strategy.clone(),
                internal_function.parameters.clone(),
                *function,
                env,
            ),
        ))
    }

    fn evaluate_let(
        &mut self,
        identifier: &str,
        value: &Expression,
        then: &Expression,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        let evaluated_value = self.evaluate_environment(
            value,
            static_module_environment,
            module_environment,
            environment,
        )?;
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
        let result = self.evaluate_environment(
            then,
            static_module_environment,
            module_environment,
            environment,
        );
        environment.remove(identifier);
        result
    }

    fn evaluate_if(
        &mut self,
        condition: &Expression,
        if_true_then: &Expression,
        if_false_then: &Expression,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        let condition = self.evaluate_environment(
            condition,
            static_module_environment,
            module_environment,
            environment,
        )?;
        if let Value::Boolean(value) = condition {
            let body = if value { if_true_then } else { if_false_then };
            self.evaluate_environment(
                body,
                static_module_environment,
                module_environment,
                environment,
            )
        } else {
            Err(Cow::from("Invalid condition type"))
        }
    }

    fn evaluate_lambda(lambda: &Lambda<Expression>, environment: &Environment) -> Closure {
        let environment = Self::get_optimized_lambda_environment(lambda, environment);
        Closure::new(lambda.parameters.clone(), lambda.body.clone(), environment)
    }

    fn get_optimized_lambda_environment<T>(
        lambda: &Lambda<T>,
        environment: &Environment,
    ) -> Environment {
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
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
        path: &Path,
    ) -> ValueResult {
        match path {
            Path::Simple(identifier) => {
                let value = environment.get(identifier).or_else(|| {
                    environment
                        .get_weak(identifier)
                        .and_then(|value| value.upgrade())
                });
                if let Some(value) = value {
                    let value: &Value = &value;
                    Ok(value.clone())
                } else {
                    let value = static_module_environment.get(identifier).or_else(|| {
                        static_module_environment
                            .get_weak(identifier)
                            .and_then(|value| value.upgrade())
                    });
                    if let Some(value) = value {
                        let value: &Value = &value;
                        Ok(value.clone())
                    } else {
                        Err(Cow::from(format!("Undefined variable: {}", identifier)))
                    }
                }
            }
            Path::Complex(path) => {
                let first_path = path.path().get(0);
                if let Some(first_path) = first_path {
                    let module_env = module_environment.get(first_path);
                    if let Some(module_env) = module_env {
                        Self::get_from_environment(
                            static_module_environment,
                            module_environment,
                            module_env,
                            &Path::Simple(path.identifier().clone()),
                        )
                    } else {
                        Err(Cow::from(format!("Undefined environment: {}", first_path)))
                    }
                } else {
                    Err(Cow::from("Undefined first_path"))
                }
            }
        }
    }

    fn evaluate_application(
        &mut self,
        identifier: &Expression,
        arguments: &[Expression],
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        let identifier = self.evaluate_environment(
            identifier,
            static_module_environment,
            module_environment,
            environment,
        )?;
        match identifier {
            Value::Closure(closure) => self.evaluate_closure_application(
                ApplicationStrategy::Eager,
                arguments,
                static_module_environment,
                module_environment,
                environment,
                closure,
            ),
            Value::FunctionClosure(application_strategy, closure) => self
                .evaluate_closure_application(
                    application_strategy,
                    arguments,
                    static_module_environment,
                    module_environment,
                    environment,
                    closure,
                ),
            Value::InternalFunctionClosure(closure) => self.evaluate_internal_closure_application(
                arguments,
                static_module_environment,
                module_environment,
                environment,
                closure,
            ),
            Value::Thunk(body, environment) => self.evaluate_environment(
                &body,
                static_module_environment,
                module_environment,
                &environment,
            ),
            _ => Err(Cow::from(format!(
                "Invalid identifier type. Expected: Closure or Thunk, Actual: {:?}",
                identifier
            ))),
        }
    }

    fn evaluate_closure_application(
        &mut self,
        application_strategy: ApplicationStrategy,
        arguments: &[Expression],
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
        closure: Closure,
    ) -> ValueResult {
        let mut arguments_iterator = arguments.iter();
        let mut has_variadic_parameter = false;
        let closure_environment = closure.environment();
        for parameter in closure.parameters() {
            let name = parameter.name();
            let arity = parameter.arity();
            match arity {
                Arity::Unary => {
                    let argument = arguments_iterator
                        .next()
                        .ok_or_else(|| format!("Missing argument for a parameter '{}'", name))?;
                    let argument = if application_strategy.is_eager() {
                        self.evaluate_environment(
                            argument,
                            static_module_environment,
                            module_environment,
                            environment,
                        )
                    } else {
                        Ok(Value::Thunk(
                            Box::new(argument.clone()),
                            environment.clone(),
                        ))
                    }?;
                    closure_environment.insert(name.clone(), Rc::new(argument));
                }
                Arity::Variadic => {
                    let list = self.create_pair_list(
                        application_strategy,
                        arguments_iterator,
                        static_module_environment,
                        module_environment,
                        environment,
                    )?;
                    closure_environment.insert(name.clone(), Rc::new(list));
                    has_variadic_parameter = true;
                    break;
                }
            }
        }
        let parameters = closure.parameters();
        let parameters_length = parameters.len();
        let arguments_length = arguments.len();
        if !has_variadic_parameter && parameters_length != arguments_length {
            return Err(Cow::from(format!(
                "Invalid number of arguments. Expected: {}, Actual: {}",
                parameters_length, arguments_length
            )));
        }
        let result = self.evaluate_environment(
            closure.body(),
            static_module_environment,
            module_environment,
            closure_environment,
        );
        for parameter in parameters {
            let name = parameter.name();
            closure_environment.remove(name);
        }
        result
    }

    fn evaluate_internal_closure_application(
        &mut self,
        arguments: &[Expression],
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
        closure: InternalFunctionClosure,
    ) -> ValueResult {
        let mut arguments_iterator = arguments.iter();
        let mut has_variadic_parameter = false;
        let mut closure_environment = HashMap::new();
        for parameter in &closure.parameters {
            let name = parameter.name();
            let arity = parameter.arity();
            match arity {
                Arity::Unary => {
                    let argument = arguments_iterator
                        .next()
                        .ok_or_else(|| format!("Missing argument for a parameter '{}'", name))?;
                    let argument = if closure.application_strategy.is_eager() {
                        self.evaluate_environment(
                            argument,
                            static_module_environment,
                            module_environment,
                            environment,
                        )
                    } else {
                        Ok(Value::Thunk(
                            Box::new(argument.clone()),
                            environment.clone(),
                        ))
                    }?;
                    closure_environment.insert(name.clone(), argument);
                }
                Arity::Variadic => {
                    let list = self.create_pair_list(
                        closure.application_strategy,
                        arguments_iterator,
                        static_module_environment,
                        module_environment,
                        environment,
                    )?;
                    closure_environment.insert(name.clone(), list);
                    has_variadic_parameter = true;
                    break;
                }
            }
        }
        let parameters = closure.parameters;
        let parameters_length = parameters.len();
        let arguments_length = arguments.len();
        if !has_variadic_parameter && parameters_length != arguments_length {
            return Err(Cow::from(format!(
                "Invalid number of arguments. Expected: {}, Actual: {}",
                parameters_length, arguments_length
            )));
        }
        let function = closure.function;
        function(closure_environment)
    }

    fn create_pair_list(
        &mut self,
        application_strategy: ApplicationStrategy,
        arguments: Iter<Expression>,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        let mut result = Value::Null;
        for argument in arguments.rev() {
            let argument = if application_strategy.is_eager() {
                self.evaluate_environment(
                    argument,
                    static_module_environment,
                    module_environment,
                    environment,
                )
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

    fn evaluate_module(
        &mut self,
        module: &Module<Expression>,
        static_module_environment: &Environment,
        global_module_environment: &ModuleEnvironment,
        environment: &Environment,
    ) -> ValueResult {
        let module_environment = Environment::new();
        let constants_environment = environment.clone();
        let mut closures = Vec::new();
        let mut evaluated_closures = HashMap::new();
        for function in module.functions() {
            let visibility = &function.visibility;
            let application_strategy = &function.application_strategy;
            let identifier = &function.name;
            let lambda = &function.body;
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
        }
        Self::fill_closures(&closures, &evaluated_closures)?;
        let mut evaluated_internal_functions = HashMap::new();
        for internal_function in module.internal_functions() {
            let visibility = &internal_function.visibility;
            let identifier = &internal_function.name;
            let closure = self.evaluate_internal_function(environment, internal_function)?;
            let closure = Rc::new(closure);
            if visibility.is_public() {
                module_environment.insert(identifier.clone(), Rc::clone(&closure));
            }
            constants_environment.insert(identifier.clone(), Rc::clone(&closure));
            evaluated_internal_functions.insert(identifier, closure);
        }
        Self::fill_closures(&closures, &evaluated_internal_functions)?;
        let mut evaluated_constants = HashMap::new();
        for constant in module.constants() {
            let visibility = &constant.visibility;
            let identifier = &constant.name;
            let value = &constant.value;
            let value = self.evaluate_environment(
                value,
                static_module_environment,
                global_module_environment,
                &constants_environment,
            )?;
            let value = Rc::new(value);
            if visibility.is_public() {
                module_environment.insert(identifier.clone(), Rc::clone(&value));
            }
            constants_environment.insert(identifier.clone(), Rc::clone(&value));
            evaluated_constants.insert(identifier, value);
        }
        Self::fill_closures(&closures, &evaluated_constants)?;
        let identifier = module.identifier();
        Ok(Value::Module(
            module.is_static(),
            identifier.to_string(),
            module_environment,
        ))
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
