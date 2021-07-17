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
use brucket_analyzer::variables_analyzer::Variables;
use brucket_ast::ast::ast_type::{LambdaType, Type};
use brucket_ast::ast::constant_value::{Boolean, ConstantValue, ConstantVariant, Number};
use brucket_ast::ast::function::{Function, InternalFunction};
use brucket_ast::ast::lambda::{Lambda, Parameter};
use brucket_ast::ast::path::Path;
use brucket_ast::ast::{Application, Constant, Identifier, If, Let, Module, Node};
use c_generator::generator::Generator;
use c_generator::syntax::c_struct::{CStruct, Fields};
use c_generator::syntax::c_type::{CPrimitiveType, CType, FunctionPointer};
use c_generator::syntax::expression::{
    Arguments, CExpression, CompoundLiteral, FunctionCallExpression, FunctionIdentifier,
    NumberExpression,
};
use c_generator::syntax::function::{FunctionHeader, FunctionParameter, Parameters};
use c_generator::syntax::instruction::{
    IfElseInstruction, Instruction, Instructions, VariableDeclaration, VariableDefinition,
    VariableInstruction,
};
use c_generator::syntax::modifiers::Modifiers;
use derive_more::Constructor;
use std::borrow::Cow;

pub mod state;
#[cfg(test)]
mod tests;

pub type TranslatorResult<T> = Result<T, TranslatorError>;
pub type TranslatorError = Cow<'static, str>;

#[derive(Default, Constructor)]
pub struct Environment {
    pub types: NodeTypes,
    pub variables: Variables,
    pub state: TranslationState,
}

#[derive(Debug, PartialEq, Constructor)]
pub struct Translation {
    pub preceding_instructions: Instructions,
    pub result_expression: CExpression,
    pub result_type: CType,
}

#[derive(Default, Constructor)]
pub struct Translator;

impl Translator {
    pub fn translate(
        &self,
        node: Node,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        self.translate_node(node, environment)
    }

    fn translate_node(
        &self,
        node: Node,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        match node {
            Node::ConstantValue(value) => self.translate_constant_value(value, environment),
            Node::Identifier(identifier) => self.translate_identifier(identifier, environment),
            Node::Application(application) => self.translate_application(application, environment),
            Node::Let(let_expression) => self.translate_let(let_expression, environment),
            Node::If(if_expression) => self.translate_if(if_expression, environment),
            Node::Lambda(lambda) => self.translate_lambda(lambda, environment),
            Node::Function(function) => self.translate_function(function, environment),
            Node::InternalFunction(function) => {
                self.translate_internal_function(function, environment)
            }
            Node::Constant(constant) => self.translate_constant(constant, environment),
            Node::Module(module) => self.translate_module(module, environment),
        }
    }

    fn translate_type(
        &self,
        node_type: &Type,
        state: &mut TranslationState,
    ) -> TranslatorResult<CType> {
        match node_type {
            Type::Any => Err("Cannot translate any type to C equivalent".into()),
            Type::Unit => Ok(CType::Primitive(CPrimitiveType::Void)),
            Type::Boolean => Ok(CType::Custom("BOOL".to_owned())),
            Type::Integer => Ok(CType::Primitive(CPrimitiveType::Int)),
            Type::Float => Ok(CType::Primitive(CPrimitiveType::Double)),
            Type::String => Ok(CType::Custom("char*".to_owned())),
            Type::Symbol(symbol) => Ok(CType::Custom(symbol.clone())),
            Type::Lambda(lambda) => self.translate_lambda_type(lambda, state),
        }
    }

    fn translate_lambda_type(
        &self,
        lambda_type: &LambdaType,
        state: &mut TranslationState,
    ) -> TranslatorResult<CType> {
        let return_type = self.translate_type(&*lambda_type.return_type, state)?;
        let parameters_types = lambda_type
            .parameters_types
            .iter()
            .map(|parameter_type| self.translate_type(&parameter_type, state))
            .collect::<Result<Vec<CType>, TranslatorError>>()?;
        let typedef_count = state.typedef_count();
        let type_name = format!("__$type_{}", typedef_count);
        state.increment_typedef();
        let function_pointer =
            FunctionPointer::new(return_type, type_name.clone(), parameters_types);
        let typedef_value = function_pointer.generate()?;
        state.add_typedef(typedef_value);
        Ok(CType::Custom(type_name))
    }

    fn translate_constant_value(
        &self,
        value: ConstantValue,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let value_type = environment.types.get(&value)?;
        let c_type = self.translate_type(value_type, &mut environment.state)?;
        let expression = self.translate_constant_variant(value.variant)?;
        Ok(Translation::new(
            Instructions::default(),
            expression,
            c_type,
        ))
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

    fn translate_identifier(
        &self,
        identifier: Identifier,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let identifier_type = environment.types.get(&identifier)?;
        let c_type = self.translate_type(identifier_type, &mut environment.state)?;
        let path = self.translate_path(&identifier.path)?;
        let variable = environment
            .state
            .variables
            .iter()
            .find(|variable| variable.name == path);
        let c_type = variable
            .map(|variable| variable.variable_type.clone())
            .unwrap_or(c_type);
        Ok(Translation::new(
            Instructions::default(),
            CExpression::NamedReference(path),
            c_type,
        ))
    }

    fn translate_path(&self, path: &Path) -> TranslatorResult<String> {
        match path {
            Path::Simple(path) => Ok(path
                .replace("+", "__$internal_add")
                .replace("-", "__$internal_subtract")
                .replace("*", "__$internal_multiply")
                .replace("/", "__$internal_divide")
                .replace("%", "__$internal_remainder")),
            Path::Complex(complex_path) => Ok(complex_path.join("_")),
        }
    }

    fn translate_application(
        &self,
        application: Application,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let translation_result = self.translate_node(*application.identifier, environment)?;

        if let CExpression::NamedReference(name) = translation_result.result_expression {
            let mut instructions = translation_result.preceding_instructions;
            let (mut arguments, arguments_instructions): (Vec<_>, Vec<_>) = application
                .arguments
                .into_iter()
                .map(|argument| self.translate_node(argument, environment))
                .collect::<Result<Vec<Translation>, TranslatorError>>()?
                .into_iter()
                .map(|argument| (argument.result_expression, argument.preceding_instructions))
                .unzip();
            let mut arguments_instructions = arguments_instructions
                .into_iter()
                .flatten()
                .collect::<Instructions>();
            instructions.append(&mut arguments_instructions);

            if let CType::Closure(_, return_type, free_arguments) = translation_result.result_type {
                let mut free_arguments = free_arguments
                    .iter()
                    .map(|argument| CExpression::NamedReference(format!("{}.{}", name, argument)))
                    .collect();
                arguments.append(&mut free_arguments);

                Ok(Translation::new(
                    instructions,
                    CExpression::FunctionCall(FunctionCallExpression::new(
                        FunctionIdentifier::NamedReference(format!("{}.lambda", name)),
                        arguments,
                    )),
                    *return_type,
                ))
            } else {
                Ok(Translation::new(
                    instructions,
                    CExpression::FunctionCall(FunctionCallExpression::new(
                        FunctionIdentifier::NamedReference(name),
                        arguments,
                    )),
                    translation_result.result_type,
                ))
            }
        } else {
            Err(Cow::from(
                "Unsupported function identifier in application type",
            ))
        }
    }

    fn translate_let(
        &self,
        let_node: Let,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let value = self.translate_node(*let_node.value, environment)?;
        let variable = Variable::new(let_node.name.clone(), value.result_type.clone());

        let variable_defined = environment.state.contains_variable(&variable);
        let mut then = if !variable_defined {
            environment.state.push_variable(variable);
            let next = self.translate_node(*let_node.then, environment)?;
            environment.state.pop_variable();
            next
        } else {
            self.translate_node(*let_node.then, environment)?
        };
        let let_count = environment.state.let_count();
        let function_name = format!("__$let_{}_{}", let_count, let_node.name);
        environment.state.increment_let();
        let arguments = environment
            .state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name.to_string()))
            .collect::<Arguments>();
        let parameters = environment
            .state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(variable.variable_type.clone(), variable.name.to_string())
            })
            .collect::<Parameters>();

        let mut instructions = value.preceding_instructions;
        if variable_defined {
            instructions.push(Instruction::VariableDefinition(VariableDefinition::new(
                let_node.name,
                value.result_expression,
            )));
        } else {
            instructions.push(Instruction::Variable(VariableInstruction::new(
                Modifiers::default(),
                value.result_type,
                let_node.name,
                value.result_expression,
            )));
        }
        instructions.append(&mut then.preceding_instructions);
        instructions.push(Instruction::Return(then.result_expression));

        environment.state.add_function(
            FunctionHeader::new(then.result_type.clone(), function_name.clone(), parameters),
            instructions,
        );

        Ok(Translation::new(
            Instructions::default(),
            CExpression::FunctionCall(FunctionCallExpression::new(
                FunctionIdentifier::NamedReference(function_name),
                arguments,
            )),
            then.result_type,
        ))
    }

    fn translate_if(
        &self,
        if_node: If,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let condition_translation = self.translate_node(*if_node.condition, environment)?;
        let if_body_translation = self.translate_node(*if_node.if_true, environment)?;
        let else_body_translation = self.translate_node(*if_node.if_false, environment)?;

        let if_count = environment.state.if_count();
        let function_name = format!("__$if_{}", if_count);
        environment.state.increment_if();

        let arguments = environment
            .state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name.clone()))
            .collect::<Arguments>();

        let parameters = environment
            .state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(variable.variable_type.clone(), variable.name.clone())
            })
            .collect::<Parameters>();

        let mut instructions = condition_translation.preceding_instructions;
        let mut then_instructions = if_body_translation.preceding_instructions;
        let mut else_instructions = else_body_translation.preceding_instructions;

        then_instructions.push(Instruction::Return(if_body_translation.result_expression));
        else_instructions.push(Instruction::Return(else_body_translation.result_expression));
        instructions.push(Instruction::IfElse(IfElseInstruction::new(
            condition_translation.result_expression,
            then_instructions,
            else_instructions,
        )));

        environment.state.add_function(
            FunctionHeader::new(
                if_body_translation.result_type.clone(),
                function_name.clone(),
                parameters,
            ),
            instructions,
        );

        Ok(Translation::new(
            Instructions::default(),
            CExpression::FunctionCall(FunctionCallExpression::new(
                FunctionIdentifier::NamedReference(function_name),
                arguments,
            )),
            if_body_translation.result_type,
        ))
    }

    fn translate_lambda(
        &self,
        lambda: Lambda,
        environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        let lambda_count = environment.state.lambda_count();
        let function_name = format!("__$lambda_{}", lambda_count);
        environment.state.increment_lambda();

        let state = &mut environment.state;
        let variables = &environment.variables.get(&lambda)?;

        let mut fields = variables
            .free_variables
            .iter()
            .filter_map(|free_variable| {
                state
                    .variables
                    .iter()
                    .find(|variable| &variable.name == free_variable)
            })
            .map(|variable| {
                let modifiers = Modifiers::default();
                let name = variable.name.clone();
                Ok(VariableDeclaration::new(
                    modifiers,
                    variable.variable_type.clone(),
                    name,
                ))
            })
            .collect::<TranslatorResult<Fields>>()?;

        let mut initializers = fields
            .iter()
            .map(|field| &field.name)
            .cloned()
            .map(CExpression::NamedReference)
            .collect::<Vec<CExpression>>();
        initializers.insert(0, CExpression::NamedReference(function_name.clone()));

        let mut free_parameters = fields
            .iter()
            .map(|field| FunctionParameter::new(field.variable_type.clone(), field.name.clone()))
            .collect::<Parameters>();

        let lambda_type = environment.types.get(&lambda)?;
        if let Type::Lambda(lambda_type) = lambda_type {
            let return_type = self.translate_type(&*lambda_type.return_type, state)?;
            let mut parameters = free_parameters
                .iter()
                .map(|parameter| parameter.parameter_type.clone())
                .collect();
            let mut parameters_types = lambda_type
                .parameters_types
                .iter()
                .map(|parameter_type| self.translate_type(&parameter_type, state))
                .collect::<Result<Vec<CType>, TranslatorError>>()?;
            parameters_types.append(&mut parameters);
            let typedef_count = state.typedef_count();
            let type_name = format!("__$type_{}", typedef_count);
            state.increment_typedef();
            let function_pointer =
                FunctionPointer::new(return_type, type_name.clone(), parameters_types);
            let typedef_value = function_pointer.generate()?;
            state.add_typedef(typedef_value);
            let c_type = CType::Custom(type_name);

            fields.insert(
                0,
                VariableDeclaration::new(Modifiers::default(), c_type, "lambda".to_owned()),
            );

            let struct_name = format!("__$lambda_{}_closure", lambda_count);
            let c_struct = CStruct::new(struct_name.clone(), fields);
            environment.state.add_struct(c_struct);

            let mut parameters = lambda
                .parameters
                .into_iter()
                .map(|parameter| self.translate_parameter(parameter, environment))
                .collect::<TranslatorResult<Parameters>>()?;
            let free_arguments = free_parameters
                .iter()
                .map(|parameter| parameter.name.clone())
                .collect();
            parameters.append(&mut free_parameters);

            let body_type = environment.types.get(&*lambda.body)?;
            let body_c_type = self.translate_type(body_type, &mut environment.state)?;
            let body_translation = self.translate_node(*lambda.body, environment)?;
            let mut body = body_translation.preceding_instructions;
            body.push(Instruction::Return(body_translation.result_expression));

            environment.state.add_function(
                FunctionHeader::new(body_c_type.clone(), function_name.clone(), parameters),
                body,
            );

            let temporary = format!("{}_temp", &function_name);
            let translation = Translation::new(
                vec![Instruction::Variable(VariableInstruction::new(
                    Modifiers::default(),
                    CType::Struct(struct_name.clone()),
                    temporary.clone(),
                    CExpression::CompoundLiteral(CompoundLiteral::new(
                        CType::Struct(struct_name.clone()),
                        initializers,
                    )),
                ))],
                CExpression::NamedReference(temporary),
                CType::Closure(struct_name, Box::from(body_c_type), free_arguments),
            );
            Ok(translation)
        } else {
            Err(Cow::from("Lambda type is not lambda"))
        }
    }

    fn translate_parameter(
        &self,
        parameter: Parameter,
        environment: &mut Environment,
    ) -> TranslatorResult<FunctionParameter> {
        // TODO: provide also self.arity
        let parameter_type =
            self.translate_type(&parameter.parameter_type, &mut environment.state)?;
        Ok(FunctionParameter::new(parameter_type, parameter.name))
    }

    fn translate_function(
        &self,
        _function: Function,
        _environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        unimplemented!("Function#translate()")
    }

    fn translate_internal_function(
        &self,
        _function: InternalFunction,
        _environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        unimplemented!("InternalFunction#translate()")
    }

    fn translate_constant(
        &self,
        _constant: Constant,
        _environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        unimplemented!("Constant#translate()")
    }

    fn translate_module(
        &self,
        _module: Module,
        _environment: &mut Environment,
    ) -> TranslatorResult<Translation> {
        unimplemented!("Module#translate()")
    }
}
