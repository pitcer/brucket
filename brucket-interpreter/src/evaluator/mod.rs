use crate::evaluator::environment::Environment;
use crate::evaluator::internal::InternalEnvironment;
use crate::value::{Closure, InternalFunctionClosure, Numeric, Value};
use brucket_analyzer::variables_analyzer::{Variables, VariablesError};
use brucket_ast::constant_value::{Boolean, ConstantVariant, Number};
use brucket_ast::function::{ApplicationStrategy, InternalFunction};
use brucket_ast::lambda::{Arity, Lambda};
use brucket_ast::path::Path;
use brucket_ast::{Application, If, Node, Visibility};
use brucket_ast::{Let, Module};
use derive_more::Constructor;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::option::Option::Some;
use std::rc::Rc;
use std::slice::Iter;

#[macro_use]
pub mod environment;
pub mod internal;
#[cfg(test)]
mod tests;

pub type ModuleEnvironment = HashMap<String, Environment>;
type ValueError = Cow<'static, str>;
type ValueResult = Result<Value, ValueError>;

#[derive(Constructor)]
pub struct EvaluatorState<'a> {
    pub variables: &'a Variables,
}

#[derive(Clone, Default, Constructor)]
pub struct Evaluator {
    pub internal_environment: InternalEnvironment,
    pub static_module_environment: Environment,
    pub module_environment: ModuleEnvironment,
}

impl Evaluator {
    #[cfg(test)]
    fn evaluate_with_default_state(&mut self, node: &Node) -> ValueResult {
        let variables = Variables::default();
        let state = EvaluatorState::new(&variables);
        self.evaluate(node, &state)
    }

    #[cfg(test)]
    fn evaluate_with_variables(&mut self, variables: &Variables, node: &Node) -> ValueResult {
        let state = EvaluatorState::new(variables);
        self.evaluate(node, &state)
    }

    pub fn evaluate(&mut self, node: &Node, state: &EvaluatorState<'_>) -> ValueResult {
        let environment = Environment::default();
        self.evaluate_environment(node, &environment, state)
    }

    fn evaluate_environment(
        &mut self,
        node: &Node,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        match *node {
            Node::ConstantValue(ref value) => Self::evaluate_constant_value(&value.variant),
            Node::Identifier(ref identifier) => {
                self.get_from_environment(&identifier.path, environment, state)
            }
            Node::Application(ref application) => {
                self.evaluate_application(application, environment, state)
            }
            Node::Let(ref let_node) => self.evaluate_let(let_node, environment, state),
            Node::If(ref if_node) => self.evaluate_if(if_node, environment, state),
            Node::Lambda(ref lambda) => Ok(Value::Closure(Self::evaluate_lambda(
                lambda,
                environment,
                state,
            )?)),
            Node::Module(ref module) => self.evaluate_module(module, environment, state),
            Node::Function(ref function) => Ok(Value::FunctionClosure(
                function.application_strategy.clone(),
                Self::evaluate_lambda(&function.body, environment, state)?,
            )),
            Node::InternalFunction(ref internal_function) => {
                self.evaluate_internal_function(internal_function, environment)
            }
            Node::Constant(ref constant) => {
                self.evaluate_environment(&constant.value, environment, state)
            }
        }
    }

    fn evaluate_constant_value(variant: &ConstantVariant) -> ValueResult {
        match *variant {
            ConstantVariant::Unit => Ok(Value::Unit),
            ConstantVariant::Null => Ok(Value::Null),
            ConstantVariant::Numeric(ref value) => {
                let value = Self::evaluate_number(value)?;
                Ok(Value::Numeric(value))
            }
            ConstantVariant::Boolean(ref value) => match *value {
                Boolean::True => Ok(Value::Boolean(true)),
                Boolean::False => Ok(Value::Boolean(false)),
            },
            ConstantVariant::String(ref value) => Ok(Value::Textual(value.clone())),
        }
    }

    fn evaluate_number(value: &Number) -> Result<Numeric, ValueError> {
        match *value {
            Number::Integer(ref value) => value
                .parse::<i32>()
                .map(Numeric::Integer)
                .map_err(|error| Cow::from(format!("Error while parsing to i32: {}", error))),

            Number::FloatingPoint(ref value) => value
                .parse::<f64>()
                .map(Numeric::FloatingPoint)
                .map_err(|error| Cow::from(format!("Error while parsing to f64: {}", error))),
        }
    }

    fn evaluate_internal_function(
        &mut self,
        internal_function: &InternalFunction,
        environment: &Environment,
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
        let_node: &Let,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let value = &*let_node.value;
        let evaluated_value = self.evaluate_environment(value, environment, state)?;
        let evaluated_value = Rc::new(evaluated_value);
        let name = &let_node.name;
        if let Node::Lambda(_) = *value {
            if let Value::Closure(ref closure) = *evaluated_value {
                let used_variables = &state.variables.get(value)?.used_variables;
                if used_variables.contains(name) {
                    let closure_environment = &closure.environment;
                    closure_environment.insert_weak(name.clone(), Rc::downgrade(&evaluated_value));
                }
            }
        }
        environment.insert(name.clone(), evaluated_value);
        let then = &*let_node.then;
        let result = self.evaluate_environment(then, environment, state);
        environment.remove(name);
        result
    }

    fn evaluate_if(
        &mut self,
        if_node: &If,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let condition = &*if_node.condition;
        let condition = self.evaluate_environment(condition, environment, state)?;
        if let Value::Boolean(value) = condition {
            let body = if value {
                &if_node.if_true
            } else {
                &if_node.if_false
            };
            self.evaluate_environment(body, environment, state)
        } else {
            Err(Cow::from("Invalid condition type"))
        }
    }

    fn evaluate_lambda(
        lambda: &Lambda,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> Result<Closure, VariablesError> {
        let environment = Self::create_optimized_lambda_environment(lambda, environment, state)?;
        Ok(Closure::new(
            lambda.parameters.clone(),
            lambda.body.clone(),
            environment,
        ))
    }

    fn create_optimized_lambda_environment(
        lambda: &Lambda,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> Result<Environment, ValueError> {
        let variables = state.variables.get(lambda)?;
        let free_variables = &variables.free_variables;
        let new_env: Environment = free_variables
            .iter()
            .filter_map(|identifier| {
                let value = environment.get(identifier)?;
                Some((identifier.clone(), value))
            })
            .collect();
        new_env.insert_all_weak(environment);
        Ok(new_env)
    }

    fn get_from_environment(
        &self,
        path: &Path,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        match *path {
            Path::Simple(ref identifier) => {
                let value = environment.get(identifier).or_else(|| {
                    environment
                        .get_weak(identifier)
                        .and_then(|value| value.upgrade())
                });
                if let Some(value) = value {
                    let value: &Value = &value;
                    Ok(value.clone())
                } else {
                    let value = self.static_module_environment.get(identifier).or_else(|| {
                        self.static_module_environment
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
            Path::Complex(ref path) => {
                let first_path = path.get(0);
                if let Some(first_path) = first_path {
                    let module_env = self.module_environment.get(first_path);
                    if let Some(module_env) = module_env {
                        path.last().map_or_else(
                            || Err(Cow::from("Path is empty")),
                            |identifier| {
                                self.get_from_environment(
                                    &Path::Simple(identifier.clone()),
                                    module_env,
                                    state,
                                )
                            },
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
        application: &Application,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let identifier = &*application.identifier;
        let identifier = self.evaluate_environment(identifier, environment, state)?;
        let arguments = &*application.arguments;
        match identifier {
            Value::Closure(closure) => self.evaluate_closure_application(
                &closure,
                &ApplicationStrategy::Eager,
                arguments,
                environment,
                state,
            ),
            Value::FunctionClosure(application_strategy, closure) => self
                .evaluate_closure_application(
                    &closure,
                    &application_strategy,
                    arguments,
                    environment,
                    state,
                ),
            Value::InternalFunctionClosure(closure) => {
                self.evaluate_internal_closure_application(closure, arguments, environment, state)
            }
            Value::Thunk(body, environment) => {
                self.evaluate_environment(&body, &environment, state)
            }
            _ => Err(Cow::from(format!(
                "Invalid identifier type. Expected: Closure or Thunk, Actual: {:?}",
                identifier
            ))),
        }
    }

    fn evaluate_closure_application(
        &mut self,
        closure: &Closure,
        application_strategy: &ApplicationStrategy,
        arguments: &[Node],
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let mut arguments_iterator = arguments.iter();
        let mut has_variadic_parameter = false;
        let closure_environment = &closure.environment;
        for parameter in &closure.parameters {
            let name = &parameter.name;
            let arity = &parameter.arity;
            match *arity {
                Arity::Unary => {
                    let argument = arguments_iterator
                        .next()
                        .ok_or_else(|| format!("Missing argument for a parameter '{}'", name))?;
                    let argument = if let ApplicationStrategy::Eager = *application_strategy {
                        self.evaluate_environment(argument, environment, state)
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
                        &application_strategy,
                        arguments_iterator,
                        environment,
                        state,
                    )?;
                    closure_environment.insert(name.clone(), Rc::new(list));
                    has_variadic_parameter = true;
                    break;
                }
            }
        }
        let parameters = &*closure.parameters;
        let parameters_length = parameters.len();
        let arguments_length = arguments.len();
        if !has_variadic_parameter && parameters_length != arguments_length {
            return Err(Cow::from(format!(
                "Invalid number of arguments. Expected: {}, Actual: {}",
                parameters_length, arguments_length
            )));
        }
        let result = self.evaluate_environment(&closure.body, closure_environment, state);
        for parameter in parameters {
            let name = &parameter.name;
            closure_environment.remove(name);
        }
        result
    }

    fn evaluate_internal_closure_application(
        &mut self,
        closure: InternalFunctionClosure,
        arguments: &[Node],
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let mut arguments_iterator = arguments.iter();
        let mut has_variadic_parameter = false;
        let mut closure_environment = HashMap::new();
        let application_strategy = &closure.application_strategy;
        for parameter in &closure.parameters {
            let name = &parameter.name;
            let arity = &parameter.arity;
            match *arity {
                Arity::Unary => {
                    let argument = arguments_iterator
                        .next()
                        .ok_or_else(|| format!("Missing argument for a parameter '{}'", name))?;
                    let argument = if let ApplicationStrategy::Eager = *application_strategy {
                        self.evaluate_environment(argument, environment, state)
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
                        application_strategy,
                        arguments_iterator,
                        environment,
                        state,
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
        application_strategy: &ApplicationStrategy,
        arguments: Iter<'_, Node>,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let mut result = Value::Null;
        for argument in arguments.rev() {
            let argument = if let ApplicationStrategy::Eager = *application_strategy {
                self.evaluate_environment(argument, environment, state)
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
        module: &Module,
        environment: &Environment,
        state: &EvaluatorState<'_>,
    ) -> ValueResult {
        let module_environment = Environment::default();
        let constants_environment = environment.clone();
        let mut closures = Vec::new();
        let mut evaluated_closures = HashMap::new();
        for function in &module.functions {
            let visibility = &function.visibility;
            let application_strategy = &function.application_strategy;
            let identifier = &function.name;
            let lambda = &function.body;
            let closure = Self::evaluate_lambda(lambda, environment, state)?;
            let closure = Value::FunctionClosure(application_strategy.clone(), closure);
            let closure = Rc::new(closure);
            if let Visibility::Public = *visibility {
                module_environment.insert(identifier.clone(), Rc::clone(&closure));
            }
            constants_environment.insert(identifier.clone(), Rc::clone(&closure));
            let used_identifiers = &state.variables.get(lambda)?.free_variables;
            closures.push((identifier, Rc::clone(&closure), used_identifiers));
            evaluated_closures.insert(identifier.clone(), closure);
        }
        Self::fill_closures(&closures, &evaluated_closures);
        let mut evaluated_internal_functions = HashMap::new();
        for internal_function in &module.internal_functions {
            let visibility = &internal_function.visibility;
            let identifier = &internal_function.name;
            let closure = self.evaluate_internal_function(internal_function, environment)?;
            let closure = Rc::new(closure);
            if let Visibility::Public = *visibility {
                module_environment.insert(identifier.clone(), Rc::clone(&closure));
            }
            constants_environment.insert(identifier.clone(), Rc::clone(&closure));
            evaluated_internal_functions.insert(identifier.clone(), closure);
        }
        Self::fill_closures(&closures, &evaluated_internal_functions);
        let mut evaluated_constants = HashMap::new();
        for constant in &module.constants {
            let visibility = &constant.visibility;
            let identifier = &constant.name;
            let value = &*constant.value;
            let value = self.evaluate_environment(value, &constants_environment, state)?;
            let value = Rc::new(value);
            if let Visibility::Public = *visibility {
                module_environment.insert(identifier.clone(), Rc::clone(&value));
            }
            constants_environment.insert(identifier.clone(), Rc::clone(&value));
            evaluated_constants.insert(identifier.clone(), value);
        }
        Self::fill_closures(&closures, &evaluated_constants);
        let identifier = &module.identifier;
        Ok(Value::Module(
            module.is_static,
            identifier.to_string(),
            module_environment,
        ))
    }

    fn fill_closures(
        closures: &[(&String, Rc<Value>, &HashSet<String>)],
        values: &HashMap<String, Rc<Value>>,
    ) {
        for &(identifier, ref closure, used_identifiers) in closures {
            if let Value::FunctionClosure(_, ref closure) = **closure {
                let environment = &closure.environment;
                for used_identifier in used_identifiers.iter() {
                    if let Some(evaluated_value) = values.get(used_identifier) {
                        if used_identifier == identifier {
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
    }
}
