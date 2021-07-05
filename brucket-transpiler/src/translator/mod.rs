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

use crate::translator::state::{TranslationState, Variable};
use brucket_analyzer::type_analyzer::NodeTypes;
use brucket_ast::ast::ast_type::{LambdaType, Type};
use brucket_ast::ast::constant_value::{Boolean, ConstantValue, ConstantVariant, Number};
use brucket_ast::ast::function::{Function, InternalFunction};
use brucket_ast::ast::lambda::{Lambda, Parameter};
use brucket_ast::ast::path::Path;
use brucket_ast::ast::{Application, Constant, Identifier, If, Let, Module, Node};
use c_generator::generator::Generator;
use c_generator::syntax::c_type::{CPrimitiveType, CType, FunctionPointer};
use c_generator::syntax::expression::{
    Arguments, CExpression, FunctionCallExpression, FunctionIdentifier, NumberExpression,
};
use c_generator::syntax::function::{FunctionHeader, FunctionParameter, Parameters};
use c_generator::syntax::instruction::{IfElseInstruction, Instruction, VariableInstruction};
use c_generator::syntax::modifiers::Modifiers;
use derive_more::Constructor;
use std::borrow::Cow;

pub mod state;

pub type TranslatorResult<T> = Result<T, TranslatorError>;
pub type TranslatorError = Cow<'static, str>;

#[derive(Default, Constructor)]
pub struct Environment {
    pub types: NodeTypes,
    pub state: TranslationState,
}

#[derive(Default, Constructor)]
pub struct Translator {
    pub environment: Environment,
}

impl Translator {
    pub fn translate(&mut self, node: Node) -> TranslatorResult<CExpression> {
        self.translate_node(node)
    }

    fn translate_node(&mut self, node: Node) -> TranslatorResult<CExpression> {
        match node {
            Node::ConstantValue(value) => self.translate_constant_value(value),
            Node::Identifier(identifier) => self.translate_identifier(identifier),
            Node::Application(application) => self.translate_application(application),
            Node::Let(let_expression) => self.translate_let(let_expression),
            Node::If(if_expression) => self.translate_if(if_expression),
            Node::Lambda(lambda) => self.translate_lambda(lambda),
            Node::Function(function) => self.translate_function(function),
            Node::InternalFunction(function) => self.translate_internal_function(function),
            Node::Constant(constant) => self.translate_constant(constant),
            Node::Module(module) => self.translate_module(module),
        }
    }

    fn translate_type(&mut self, node_type: &Type) -> TranslatorResult<CType> {
        translate_type(&mut self.environment.state, node_type)
    }

    fn translate_constant_value(&self, value: ConstantValue) -> TranslatorResult<CExpression> {
        self.translate_constant_variant(value.variant)
    }

    fn translate_constant_variant(
        &self,
        variant: ConstantVariant,
    ) -> TranslatorResult<CExpression> {
        let expression = match variant {
            ConstantVariant::Unit => CExpression::Empty,
            ConstantVariant::Null => CExpression::NamedReference("NULL".to_string()),
            ConstantVariant::Numeric(numeric) => match numeric {
                Number::Integer(value) => CExpression::Number(NumberExpression::Integer(value)),
                Number::FloatingPoint(value) => {
                    CExpression::Number(NumberExpression::FloatingPoint(value))
                }
            },
            ConstantVariant::Boolean(boolean) => match boolean {
                Boolean::True => CExpression::NamedReference("TRUE".to_string()),
                Boolean::False => CExpression::NamedReference("FALSE".to_string()),
            },
            ConstantVariant::String(string) => CExpression::String(string),
        };
        Ok(expression)
    }

    fn translate_identifier(&self, identifier: Identifier) -> TranslatorResult<CExpression> {
        let path = self.translate_path(&identifier.path)?;
        Ok(CExpression::NamedReference(path))
    }

    fn translate_path(&self, path: &Path) -> TranslatorResult<String> {
        match path {
            Path::Simple(path) => Ok(path
                .replace("+", "__$internal_add")
                .replace("-", "__$internal_subtract")
                .replace("*", "__$internal_multiply")
                .replace("/", "__$internal_divide")
                .replace("%", "__$internal_remainder")),
            Path::Complex(complex_path) => Ok(format!(
                "{}_{}",
                complex_path.path.join("_"),
                &complex_path.identifier
            )),
        }
    }

    fn translate_application(&mut self, application: Application) -> TranslatorResult<CExpression> {
        let name = self.translate_node(*application.identifier)?;
        if let CExpression::NamedReference(name) = name {
            let arguments = application
                .arguments
                .into_iter()
                .map(|argument| self.translate_node(argument))
                .collect::<Result<Vec<CExpression>, TranslatorError>>()?;
            Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                FunctionIdentifier::NamedReference(name),
                arguments,
            )))
        } else {
            Err(Cow::from(
                "Unsupported function identifier in application type",
            ))
        }
    }

    fn translate_let(&mut self, let_node: Let) -> TranslatorResult<CExpression> {
        let then_type = self.environment.types.get(&*let_node.then)?;
        let then_c_type = translate_type(&mut self.environment.state, then_type)?;
        let value_type = self.environment.types.get(&*let_node.value)?;
        let value_c_type = translate_type(&mut self.environment.state, value_type)?;
        let value = self.translate_node(*let_node.value)?;
        let variable = Variable::new(let_node.name.clone(), value_c_type.clone());
        let next = if !self.environment.state.contains_variable(&variable) {
            self.environment.state.push_variable(variable);
            let next = self.translate_node(*let_node.then)?;
            self.environment.state.pop_variable();
            next
        } else {
            self.translate_node(*let_node.then)?
        };
        let let_count = self.environment.state.let_count();
        let function_name = format!("__$let_{}_{}", let_count, let_node.name);
        self.environment.state.increment_let();
        let arguments = self
            .environment
            .state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name.to_string()))
            .collect::<Arguments>();
        let parameters = self
            .environment
            .state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(variable.variable_type.clone(), variable.name.to_string())
            })
            .collect::<Parameters>();
        self.environment.state.add_function(
            FunctionHeader::new(then_c_type, function_name.clone(), parameters),
            vec![
                Instruction::Variable(VariableInstruction::new(
                    Modifiers::default(),
                    value_c_type,
                    let_node.name,
                    value,
                )),
                Instruction::Return(next),
            ],
        );
        Ok(CExpression::FunctionCall(FunctionCallExpression::new(
            FunctionIdentifier::NamedReference(function_name),
            arguments,
        )))
    }

    fn translate_if(&mut self, if_node: If) -> TranslatorResult<CExpression> {
        let if_type = self.environment.types.get(&*if_node.if_true)?;
        let if_c_type = translate_type(&mut self.environment.state, if_type)?;
        let condition = self.translate_node(*if_node.condition)?;
        let if_body = self.translate_node(*if_node.if_true)?;
        let else_body = self.translate_node(*if_node.if_false)?;
        let if_count = self.environment.state.if_count();
        let function_name = format!("__$if_{}", if_count);
        self.environment.state.increment_if();
        let arguments = self
            .environment
            .state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name.clone()))
            .collect::<Arguments>();
        let parameters = self
            .environment
            .state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(variable.variable_type.clone(), variable.name.clone())
            })
            .collect::<Parameters>();
        self.environment.state.add_function(
            FunctionHeader::new(if_c_type, function_name.clone(), parameters),
            vec![Instruction::IfElse(IfElseInstruction::new(
                condition,
                vec![Instruction::Return(if_body)],
                vec![Instruction::Return(else_body)],
            ))],
        );
        Ok(CExpression::FunctionCall(FunctionCallExpression::new(
            FunctionIdentifier::NamedReference(function_name),
            arguments,
        )))
    }

    fn translate_lambda(&mut self, lambda: Lambda) -> TranslatorResult<CExpression> {
        let body_type = self.environment.types.get(&*lambda.body)?;
        let body_c_type = translate_type(&mut self.environment.state, body_type)?;
        let body = self.translate_node(*lambda.body)?;
        let lambda_count = self.environment.state.lambda_count();
        let function_name = format!("__$lambda_{}", lambda_count);
        self.environment.state.increment_lambda();
        let parameters = lambda
            .parameters
            .into_iter()
            .map(|parameter| self.translate_parameter(parameter))
            .collect::<TranslatorResult<Parameters>>()?;
        self.environment.state.add_function(
            FunctionHeader::new(body_c_type, function_name.clone(), parameters),
            vec![Instruction::Return(body)],
        );
        // TODO: close used variables in closure
        Ok(CExpression::NamedReference(function_name))
    }

    fn translate_parameter(&mut self, parameter: Parameter) -> TranslatorResult<FunctionParameter> {
        // TODO: provide also self.arity
        let parameter_type = self.translate_type(&parameter.parameter_type)?;
        Ok(FunctionParameter::new(parameter_type, parameter.name))
    }

    fn translate_function(&self, _function: Function) -> TranslatorResult<CExpression> {
        unimplemented!("Function#translate()")
    }

    fn translate_internal_function(
        &self,
        _function: InternalFunction,
    ) -> TranslatorResult<CExpression> {
        unimplemented!("InternalFunction#translate()")
    }

    fn translate_constant(&self, _constant: Constant) -> TranslatorResult<CExpression> {
        unimplemented!("Constant#translate()")
    }

    fn translate_module(&self, _module: Module) -> TranslatorResult<CExpression> {
        unimplemented!("Module#translate()")
    }
}

fn translate_type(state: &mut TranslationState, node_type: &Type) -> TranslatorResult<CType> {
    match node_type {
        Type::Any => Err("Cannot translate any type to C equivalent".into()),
        Type::Unit => Ok(CType::Primitive(CPrimitiveType::Void)),
        Type::Boolean => Ok(CType::Custom("BOOL".to_owned())),
        Type::Integer => Ok(CType::Primitive(CPrimitiveType::Int)),
        Type::Float => Ok(CType::Primitive(CPrimitiveType::Double)),
        Type::String => Ok(CType::Custom("char*".to_owned())),
        Type::Symbol(symbol) => Ok(CType::Custom(symbol.clone())),
        Type::Lambda(lambda) => translate_lambda_type(state, lambda),
    }
}

fn translate_lambda_type(
    state: &mut TranslationState,
    lambda_type: &LambdaType,
) -> TranslatorResult<CType> {
    let return_type = translate_type(state, &*lambda_type.return_type)?;
    let parameters_types = lambda_type
        .parameters_types
        .iter()
        .map(|parameter_type| translate_type(state, &parameter_type))
        .collect::<Result<Vec<CType>, TranslatorError>>()?;
    let typedef_count = state.typedef_count();
    let type_name = format!("__$type_{}", typedef_count);
    state.increment_typedef();
    let function_pointer = FunctionPointer::new(return_type, type_name.clone(), parameters_types);
    let typedef_value = function_pointer.generate()?;
    state.add_typedef(typedef_value);
    Ok(CType::Custom(type_name))
}
