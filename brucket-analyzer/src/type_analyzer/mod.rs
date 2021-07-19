use brucket_ast::ast_type::{LambdaType, Type};
use brucket_ast::constant_value::{ConstantValue, ConstantVariant, Number};
use brucket_ast::lambda::Lambda;
use brucket_ast::path::Path;
use brucket_ast::{Application, Identifier, If, Let, Node, NodeId, NodeIdHolder};
use derive_more::Constructor;
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem;

#[cfg(test)]
mod tests;

pub type TypedError = Cow<'static, str>;
pub type TypedResult = Result<Type, TypedError>;

#[derive(Debug, PartialEq, Default, Constructor)]
pub struct NodeTypes(HashMap<NodeId, Type>);

impl NodeTypes {
    pub fn insert(&mut self, node: &dyn NodeIdHolder, node_type: Type) {
        let node_id = node.node_id();
        self.0.insert(node_id, node_type);
    }

    pub fn get(&self, node: &dyn NodeIdHolder) -> Result<&Type, TypedError> {
        let node_id = node.node_id();
        self.0
            .get(&node_id)
            .ok_or_else(|| format!("Type for node {} not found", node_id).into())
    }
}

#[derive(Debug, Default, Constructor)]
pub struct Environment {
    variables: HashMap<Path, Type>,
    node_types: NodeTypes,
}

impl Environment {
    pub fn insert_variable(&mut self, path: Path, variable_type: Type) {
        self.variables.insert(path, variable_type);
    }

    pub fn remove_variable(&mut self, path: &Path) -> Option<Type> {
        self.variables.remove(path)
    }

    #[must_use]
    pub fn get_variable(&self, path: &Path) -> Option<&Type> {
        self.variables.get(path)
    }
}

#[derive(Default, Constructor)]
pub struct TypeAnalyzer {
    environment: Environment,
}

impl TypeAnalyzer {
    pub fn analyze_types(&mut self, node: &Node) -> Result<(Type, NodeTypes), TypedError> {
        let node_type = self.analyze_node_types(node)?;
        let node_types = mem::take(&mut self.environment.node_types);
        Ok((node_type, node_types))
    }

    #[allow(clippy::match_same_arms)]
    fn analyze_node_types(&mut self, node: &Node) -> TypedResult {
        let node_type = match node {
            Node::ConstantValue(value) => self.analyze_constant_value_types(value),
            Node::Identifier(identifier) => self.analyze_identifier_types(identifier)?,
            Node::Application(application) => self.analyze_application_types(application)?,
            Node::Let(let_node) => self.analyze_let_types(let_node)?,
            Node::If(if_node) => self.analyze_if_types(if_node)?,
            Node::Lambda(lambda) => self.analyze_lambda_types(lambda)?,
            Node::Module(_) => unimplemented!(),
            Node::Function(_) => unimplemented!(),
            Node::Constant(_) => unimplemented!(),
            Node::InternalFunction(_) => unimplemented!(),
        };
        Ok(node_type)
    }

    fn analyze_constant_value_types(&mut self, value: &ConstantValue) -> Type {
        let value_type = match &value.variant {
            ConstantVariant::Unit => Type::Unit,
            ConstantVariant::Null => Type::Any,
            ConstantVariant::Numeric(number) => match number {
                Number::Integer(_) => Type::Integer,
                Number::FloatingPoint(_) => Type::Float,
            },
            ConstantVariant::Boolean(_) => Type::Boolean,
            ConstantVariant::String(_) => Type::String,
        };
        self.environment
            .node_types
            .insert(value, value_type.clone());
        value_type
    }

    fn analyze_identifier_types(&mut self, identifier: &Identifier) -> TypedResult {
        let path = &identifier.path;
        let path_type = self
            .environment
            .get_variable(path)
            .ok_or_else(|| format!("Failed to get type of variable {}", path))?
            .clone();
        self.environment
            .node_types
            .insert(identifier, path_type.clone());
        Ok(path_type)
    }

    fn analyze_application_types(&mut self, application: &Application) -> TypedResult {
        let identifier = &application.identifier;
        let identifier_type = self.analyze_node_types(identifier)?;
        if let Type::Lambda(lambda_type) = identifier_type {
            for argument in &application.arguments {
                self.analyze_node_types(argument)?;
            }
            self.environment
                .node_types
                .insert(application, *lambda_type.return_type.clone());
            Ok(*lambda_type.return_type)
        } else {
            Err("Application identifier must evaluate to Lambda".into())
        }
    }

    fn analyze_let_types(&mut self, let_node: &Let) -> TypedResult {
        let value = &let_node.value;
        let value_type = self.analyze_node_types(value)?;
        let name = &let_node.name;
        let expected_value_type = &let_node.value_type;
        if *expected_value_type != Type::Any && *expected_value_type != value_type {
            return Err(format!(
                "Invalid let variable type, name: {}, expected: {}, actual: {}",
                name, expected_value_type, value_type
            )
            .into());
        }
        let path = Path::Simple(name.clone());
        self.environment.insert_variable(path.clone(), value_type);
        let then = &let_node.then;
        let then_type = self.analyze_node_types(then)?;
        self.environment.remove_variable(&path);
        self.environment
            .node_types
            .insert(let_node, then_type.clone());
        Ok(then_type)
    }

    fn analyze_if_types(&mut self, if_node: &If) -> TypedResult {
        let if_true = &if_node.if_true;
        let if_false = &if_node.if_false;
        let then_type = self.analyze_node_types(if_true)?;
        let else_type = self.analyze_node_types(if_false)?;
        if then_type != else_type {
            return Err(format!(
                "Types in both if expression's branches must be the same, actual: {}, {}",
                then_type, else_type
            )
            .into());
        }
        let condition = &if_node.condition;
        let condition_type = self.analyze_node_types(condition)?;
        if condition_type != Type::Boolean {
            return Err(format!(
                "Condition type must have Boolean type, actual: {}",
                condition_type
            )
            .into());
        }
        self.environment
            .node_types
            .insert(if_node, then_type.clone());
        Ok(then_type)
    }

    fn analyze_lambda_types(&mut self, lambda: &Lambda) -> TypedResult {
        // TODO: reduce number of clones
        // ---
        let parameters = &lambda.parameters;
        for parameter in parameters {
            self.environment.insert_variable(
                Path::Simple(parameter.name.clone()),
                parameter.parameter_type.clone(),
            );
        }
        let body = &lambda.body;
        let body_type = self.analyze_node_types(body)?;
        for parameter in parameters {
            self.environment
                .remove_variable(&Path::Simple(parameter.name.clone()));
        }
        // ---
        // TODO: get parameters types from usage context
        let parameters_types = lambda
            .parameters
            .iter()
            .map(|parameter| parameter.parameter_type.clone())
            .collect::<Vec<Type>>();
        let lambda_type = LambdaType::new(parameters_types, Box::new(body_type));
        let lambda_type = Type::Lambda(lambda_type);
        self.environment
            .node_types
            .insert(lambda, lambda_type.clone());
        Ok(lambda_type)
    }
}
