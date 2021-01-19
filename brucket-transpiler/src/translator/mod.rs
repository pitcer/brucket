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

use brucket_ast::analyzer::type_analyzer::typed_expression::{
    TypedExpression, TypedExpressionType,
};
use brucket_ast::ast::{
    Application, Boolean, Constant, ConstantValue, Function, If, InternalFunction, Lambda,
    LambdaType, Let, Module, Number, Parameter, Path, Type,
};
use c_generator::generator::Generator;
use c_generator::syntax::c_type::{CPrimitiveType, CType, FunctionPointer};
use c_generator::syntax::expression::{
    Arguments, CExpression, FunctionCallExpression, FunctionIdentifier, NumberExpression,
};
use c_generator::syntax::function::{FunctionHeader, FunctionParameter, Parameters};
use c_generator::syntax::instruction::{IfElseInstruction, Instruction, VariableInstruction};
use c_generator::syntax::modifiers::Modifiers;

use crate::translator::state::{TranslationState, Variable};

pub mod state;

pub type TranslatorResult<T> = Result<T, TranslatorError>;
pub type TranslatorError = Cow<'static, str>;

pub trait Translate<T> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<T>;
}

impl Translate<CType> for Type {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CType> {
        match self {
            Type::Any => Err("Cannot translate any type to C equivalent".into()),
            Type::Unit => Ok(CType::Primitive(CPrimitiveType::Void)),
            Type::Boolean => Ok(CType::Custom("BOOL".to_owned())),
            Type::Integer => Ok(CType::Primitive(CPrimitiveType::Int)),
            Type::Float => Ok(CType::Primitive(CPrimitiveType::Double)),
            Type::String => Ok(CType::Custom("char*".to_owned())),
            Type::Symbol(symbol) => Ok(CType::Custom(symbol)),
            Type::Lambda(lambda) => lambda.translate(state),
        }
    }
}

impl Translate<CType> for LambdaType {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CType> {
        let return_type = self.return_type.translate(state)?;
        let parameters_types = self
            .parameters_types
            .into_iter()
            .map(|parameter_type| parameter_type.translate(state))
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
}

impl Translate<CExpression> for ConstantValue {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let expression = match self {
            ConstantValue::Unit => CExpression::Empty,
            ConstantValue::Null => CExpression::NamedReference("NULL".to_string()),
            ConstantValue::Numeric(numeric) => match numeric {
                Number::Integer(value) => CExpression::Number(NumberExpression::Integer(value)),
                Number::FloatingPoint(value) => {
                    CExpression::Number(NumberExpression::FloatingPoint(value))
                }
            },
            ConstantValue::Boolean(boolean) => match boolean {
                Boolean::True => CExpression::NamedReference("TRUE".to_string()),
                Boolean::False => CExpression::NamedReference("FALSE".to_string()),
            },
            ConstantValue::String(string) => CExpression::String(string),
        };
        Ok(expression)
    }
}

impl Translate<String> for Path {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<String> {
        match self {
            Path::Simple(path) => Ok(path
                .replace("+", "__$internal_add")
                .replace("-", "__$internal_subtract")
                .replace("*", "__$internal_multiply")
                .replace("/", "__$internal_divide")
                .replace("%", "__$internal_remainder")),
            Path::Complex(complex_path) => Ok(format!(
                "{}_{}",
                complex_path.path().join("_"),
                complex_path.identifier()
            )),
        }
    }
}

impl Translate<CExpression> for Application<TypedExpression> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let name = self.identifier.translate(state)?;
        if let CExpression::NamedReference(name) = name {
            let arguments = self
                .arguments
                .into_iter()
                .map(|argument| argument.translate(state))
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
}

impl Translate<CExpression> for Let<TypedExpression> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let then_type = self.then.evaluated_type.clone();
        let then_c_type = then_type.translate(state)?;
        let value_type = self.value.evaluated_type.clone();
        let value_c_type = value_type.translate(state)?;
        let value = self.value.translate(state)?;
        let variable = Variable::new(self.name.clone(), value_c_type.clone());
        let next = if !state.contains_variable(&variable) {
            state.push_variable(variable);
            let next = self.then.translate(state)?;
            state.pop_variable();
            next
        } else {
            self.then.translate(state)?
        };
        let let_count = state.let_count();
        let function_name = format!("__$let_{}_{}", let_count, self.name);
        state.increment_let();
        let arguments = state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name().to_string()))
            .collect::<Arguments>();
        let parameters = state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(
                    variable.variable_type().clone(),
                    variable.name().to_string(),
                )
            })
            .collect::<Parameters>();
        state.add_function(
            FunctionHeader::new(then_c_type, function_name.clone(), parameters),
            vec![
                Instruction::Variable(VariableInstruction::new(
                    Modifiers::default(),
                    value_c_type,
                    self.name,
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
}

impl Translate<CExpression> for If<TypedExpression> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let if_type = self.if_true.evaluated_type.clone();
        let if_c_type = if_type.translate(state)?;
        let condition = self.condition.translate(state)?;
        let if_body = self.if_true.translate(state)?;
        let else_body = self.if_false.translate(state)?;
        let if_count = state.if_count();
        let function_name = format!("__$if_{}", if_count);
        state.increment_if();
        let arguments = state
            .variables()
            .iter()
            .map(|variable| CExpression::NamedReference(variable.name().to_string()))
            .collect::<Arguments>();
        let parameters = state
            .variables()
            .iter()
            .map(|variable| {
                FunctionParameter::new(
                    variable.variable_type().clone(),
                    variable.name().to_string(),
                )
            })
            .collect::<Parameters>();
        state.add_function(
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
}

impl Translate<CExpression> for Lambda<TypedExpression> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let body_type = self.body.evaluated_type.clone();
        let body_c_type = body_type.translate(state)?;
        let body = self.body.translate(state)?;
        let lambda_count = state.lambda_count();
        let function_name = format!("__$lambda_{}", lambda_count);
        state.increment_lambda();
        let parameters = self
            .parameters
            .into_iter()
            .map(|parameter| parameter.translate(state))
            .collect::<TranslatorResult<Parameters>>()?;
        state.add_function(
            FunctionHeader::new(body_c_type, function_name.clone(), parameters),
            vec![Instruction::Return(body)],
        );
        // TODO: close used variables in closure
        Ok(CExpression::NamedReference(function_name))
    }
}

impl Translate<FunctionParameter> for Parameter {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<FunctionParameter> {
        // TODO: provide also self.arity
        let parameter_type = self.parameter_type.translate(state)?;
        Ok(FunctionParameter::new(parameter_type, self.name))
    }
}

impl Translate<CExpression> for Module<TypedExpression> {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Module#translate()")
    }
}

impl Translate<CExpression> for Function<TypedExpression> {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Function#translate()")
    }
}

impl Translate<CExpression> for InternalFunction {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("InternalFunction#translate()")
    }
}

impl Translate<CExpression> for Constant<TypedExpression> {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Constant#translate()")
    }
}

impl Translate<CExpression> for TypedExpression {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        match self.expression {
            TypedExpressionType::ConstantValue(value) => value.translate(state),
            TypedExpressionType::Identifier(path) => {
                Ok(CExpression::NamedReference(path.translate(state)?))
            }
            TypedExpressionType::Application(application) => application.translate(state),
            TypedExpressionType::Let(let_expression) => let_expression.translate(state),
            TypedExpressionType::If(if_expression) => if_expression.translate(state),
            TypedExpressionType::Lambda(lambda) => lambda.translate(state),
            TypedExpressionType::Module(module) => module.translate(state),
            TypedExpressionType::Function(function) => function.translate(state),
            TypedExpressionType::InternalFunction(internal_function) => {
                internal_function.translate(state)
            }
            TypedExpressionType::Constant(constant) => constant.translate(state),
        }
    }
}
