use brucket_ast::ast_type::Type;
use brucket_ast::constant_value::ConstantValue;
use brucket_ast::function::{Function, InternalFunction};
use brucket_ast::lambda::Lambda;
use brucket_ast::path::Path;
use brucket_ast::{Application, Constant, If, Let, Module, NodeIdHolder};
use brucket_ast::{Identifier, Node, NodeId};
use derive_more::Constructor;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::mem;

#[cfg(test)]
mod tests;

pub type VariablesError = Cow<'static, str>;
pub type VariablesResult = Result<NodeVariables, VariablesError>;

#[derive(Clone, Debug, PartialEq, Default, Constructor)]
pub struct Variables(pub HashMap<NodeId, NodeVariables>);

impl Variables {
    #[inline]
    pub fn insert(&mut self, node: &impl NodeIdHolder, node_variables: NodeVariables) {
        let node_id = node.node_id();
        self.0.insert(node_id, node_variables);
    }

    #[inline]
    pub fn get(&self, node: &impl NodeIdHolder) -> Result<&NodeVariables, VariablesError> {
        let node_id = node.node_id();
        self.0
            .get(&node_id)
            .ok_or_else(|| Cow::from(format!("Variables for node {} not found", node_id)))
    }
}

#[derive(Default, Constructor)]
pub struct Environment {
    variables: Variables,
}

#[derive(Debug, Clone, PartialEq, Default, Constructor)]
pub struct NodeVariables {
    pub variables: HashMap<String, Variable>,
    pub used_variables: HashSet<String>,
    pub free_variables: HashSet<String>,
}

impl NodeVariables {
    fn append(&mut self, node_variables: NodeVariables) {
        for variable in node_variables.variables {
            self.variables.insert(variable.0, variable.1);
        }
        for used_variable in node_variables.used_variables {
            self.used_variables.insert(used_variable);
        }
        for free_variable in node_variables.free_variables {
            self.free_variables.insert(free_variable);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Constructor)]
pub struct Variable {
    pub name: String,
    pub expected_type: Type,
}

#[derive(Default, Constructor)]
pub struct VariablesAnalyzer {
    environment: Environment,
}

impl VariablesAnalyzer {
    #[inline]
    pub fn analyze_variables(
        &mut self,
        node: &Node,
    ) -> Result<(NodeVariables, Variables), VariablesError> {
        let node_variables = self.analyze_node_variables(node)?;
        let variables = mem::take(&mut self.environment.variables);
        Ok((node_variables, variables))
    }

    fn analyze_node_variables(&mut self, node: &Node) -> VariablesResult {
        let node_variables = match *node {
            Node::ConstantValue(ref value) => self.analyze_constant_value_variables(value),
            Node::Identifier(ref identifier) => self.analyze_identifier_variables(identifier),
            Node::Application(ref application) => {
                self.analyze_application_variables(application)?
            }
            Node::Let(ref let_node) => self.analyze_let_variables(let_node)?,
            Node::If(ref if_node) => self.analyze_if_variables(if_node)?,
            Node::Lambda(ref lambda) => self.analyze_lambda_variables(lambda)?,
            Node::Function(ref function) => self.analyze_function_variables(function)?,
            Node::InternalFunction(ref function) => {
                self.analyze_internal_function_variables(function)
            }
            Node::Constant(ref constant) => self.analyze_constant_variables(constant)?,
            Node::Module(ref module) => self.analyze_module_variables(module)?,
        };
        Ok(node_variables)
    }

    fn analyze_constant_value_variables(&mut self, value: &ConstantValue) -> NodeVariables {
        let node_variables = NodeVariables::default();
        self.environment
            .variables
            .insert(value, node_variables.clone());
        node_variables
    }

    fn analyze_identifier_variables(&mut self, identifier: &Identifier) -> NodeVariables {
        let mut node_variables = NodeVariables::default();

        let path = &identifier.path;
        if let Path::Simple(ref name) = *path {
            node_variables.used_variables.insert(name.clone());
            node_variables.free_variables.insert(name.clone());
        }

        self.environment
            .variables
            .insert(identifier, node_variables.clone());
        node_variables
    }

    fn analyze_application_variables(&mut self, application: &Application) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let identifier = &application.identifier;
        let identifier_variables = self.analyze_node_variables(identifier)?;
        node_variables.append(identifier_variables);

        application
            .arguments
            .iter()
            .map(|argument| self.analyze_node_variables(argument))
            .collect::<Result<Vec<NodeVariables>, VariablesError>>()?
            .into_iter()
            .for_each(|argument_variables| node_variables.append(argument_variables));

        self.environment
            .variables
            .insert(application, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_let_variables(&mut self, let_node: &Let) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let name = &let_node.name;
        let value_type = let_node.value_type.clone();
        let variable = Variable::new(name.clone(), value_type);
        node_variables.variables.insert(name.clone(), variable);

        let value = &let_node.value;
        let value_variables = self.analyze_node_variables(value)?;
        node_variables.append(value_variables);

        let then = &let_node.then;
        let then_variables = self.analyze_node_variables(then)?;
        node_variables.append(then_variables);

        node_variables.free_variables.remove(name);

        self.environment
            .variables
            .insert(let_node, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_if_variables(&mut self, if_node: &If) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let condition = &if_node.condition;
        let condition_variables = self.analyze_node_variables(condition)?;
        node_variables.append(condition_variables);

        let then = &if_node.if_true;
        let then_variables = self.analyze_node_variables(then)?;
        node_variables.append(then_variables);

        let else_node = &if_node.if_false;
        let else_variables = self.analyze_node_variables(else_node)?;
        node_variables.append(else_variables);

        self.environment
            .variables
            .insert(if_node, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_lambda_variables(&mut self, lambda: &Lambda) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let body = &lambda.body;
        let body_variables = self.analyze_node_variables(body)?;
        node_variables.append(body_variables);

        let parameters = &*lambda.parameters;
        for parameter in parameters {
            let name = &parameter.name;
            node_variables.free_variables.remove(name);
        }

        self.environment
            .variables
            .insert(lambda, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_function_variables(&mut self, function: &Function) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let body = &function.body;
        let body_variables = self.analyze_lambda_variables(body)?;
        node_variables.append(body_variables);

        self.environment
            .variables
            .insert(function, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_internal_function_variables(
        &mut self,
        internal_function: &InternalFunction,
    ) -> NodeVariables {
        let node_variables = NodeVariables::default();
        self.environment
            .variables
            .insert(internal_function, node_variables.clone());
        node_variables
    }

    fn analyze_constant_variables(&mut self, constant: &Constant) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        let value = &constant.value;
        let value_variables = self.analyze_node_variables(value)?;
        node_variables.append(value_variables);

        self.environment
            .variables
            .insert(constant, node_variables.clone());
        Ok(node_variables)
    }

    fn analyze_module_variables(&mut self, module: &Module) -> VariablesResult {
        let mut node_variables = NodeVariables::default();

        module
            .functions
            .iter()
            .map(|function| self.analyze_function_variables(function))
            .collect::<Result<Vec<NodeVariables>, VariablesError>>()?
            .into_iter()
            .for_each(|function_variables| node_variables.append(function_variables));

        module
            .internal_functions
            .iter()
            .map(|function| self.analyze_internal_function_variables(function))
            .collect::<Vec<NodeVariables>>()
            .into_iter()
            .for_each(|function_variables| node_variables.append(function_variables));

        module
            .constants
            .iter()
            .map(|constant| self.analyze_constant_variables(constant))
            .collect::<Result<Vec<NodeVariables>, VariablesError>>()?
            .into_iter()
            .for_each(|constant_variables| node_variables.append(constant_variables));

        self.environment
            .variables
            .insert(module, node_variables.clone());
        Ok(node_variables)
    }
}
