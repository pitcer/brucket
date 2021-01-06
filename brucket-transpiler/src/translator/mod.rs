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

use brucket_ast::ast::{
    Application, Boolean, Constant, ConstantValue, Expression, Function, If, Lambda, Let, Module,
    Number, Path,
};
use c_generator::syntax::expression::{
    Arguments, CExpression, FunctionCallExpression, NumberExpression,
};
use c_generator::syntax::function::{FunctionHeader, FunctionParameter, Parameters};
use c_generator::syntax::instruction::{IfElseInstruction, Instruction, VariableInstruction};
use c_generator::syntax::{PrimitiveType, Type};

use crate::translator::state::{TranslationState, Variable};
use brucket_ast::analyzer::type_analyzer::Typed;

pub mod state;

pub type TranslatorResult<T> = Result<T, TranslatorError>;
pub type TranslatorError = Cow<'static, str>;

pub trait Translate<T> {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<T>;
}

impl Translate<CExpression> for ConstantValue {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let expression = match self {
            ConstantValue::Unit => CExpression::NamedReference("UNIT".to_string()),
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
                .replace("+", "__internal_plus")
                .replace("-", "__internal_minus")
                .replace("*", "__internal_times")
                .replace("/", "__internal_divide")
                .replace("%", "__internal_remainder")),
            Path::Complex(complex_path) => Ok(format!(
                "{}_{}",
                complex_path.path().join("_"),
                complex_path.identifier()
            )),
        }
    }
}

impl Translate<CExpression> for Application {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let name = self.identifier.translate(state)?;
        if let CExpression::NamedReference(name) = name {
            let arguments = self
                .arguments
                .into_iter()
                .map(|argument| argument.translate(state))
                .collect::<Result<Vec<CExpression>, TranslatorError>>()?;
            Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                name, arguments,
            )))
        } else {
            Err(Cow::from(
                "Unsupported function identifier in application type",
            ))
        }
    }
}

fn to_c_type(t: brucket_ast::analyzer::type_analyzer::Type) -> Type {
    match t {
        brucket_ast::analyzer::type_analyzer::Type::Unknown => panic!("unknown type"),
        brucket_ast::analyzer::type_analyzer::Type::Unit => Type::Primitive(PrimitiveType::Int),
        brucket_ast::analyzer::type_analyzer::Type::Boolean => Type::Primitive(PrimitiveType::Int),
        brucket_ast::analyzer::type_analyzer::Type::Integer => Type::Primitive(PrimitiveType::Int),
        brucket_ast::analyzer::type_analyzer::Type::Float => Type::Primitive(PrimitiveType::Double),
        brucket_ast::analyzer::type_analyzer::Type::String => Type::Custom("char*".to_owned()),
        brucket_ast::analyzer::type_analyzer::Type::Lambda(_lambda) => {
            unimplemented!()
        }
    }
}

impl Translate<CExpression> for Let {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let then_type = self.get_type(&mut state.env)?;
        let value_type = &self.value.get_type(&mut state.env)?;
        let value = self.value.translate(state)?;
        let variable = Variable::new(self.name.clone(), to_c_type(value_type.clone()));
        let next = if !state.contains_variable(&variable) {
            state.push_variable(variable);
            let next = self.then.translate(state)?;
            state.pop_variable();
            next
        } else {
            self.then.translate(state)?
        };
        let let_count = state.let_count();
        let function_name = format!("__internal_let_{}_{}", let_count, self.name);
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
            FunctionHeader::new(to_c_type(then_type), function_name.clone(), parameters),
            vec![
                Instruction::Variable(VariableInstruction::new(
                    to_c_type(value_type.clone()),
                    self.name,
                    Some(value),
                )),
                Instruction::Return(next),
            ],
        );
        Ok(CExpression::FunctionCall(FunctionCallExpression::new(
            function_name,
            arguments,
        )))
    }
}

impl Translate<CExpression> for If {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        let condition = self.condition.translate(state)?;
        let if_body = self.if_true.translate(state)?;
        let else_body = self.if_false.translate(state)?;
        let if_count = state.if_count();
        let function_name = format!("__internal_if_{}", if_count);
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
            FunctionHeader::new(
                Type::Primitive(PrimitiveType::Int),
                function_name.clone(),
                parameters,
            ),
            vec![Instruction::IfElse(IfElseInstruction::new(
                condition,
                vec![Instruction::Return(if_body)],
                vec![Instruction::Return(else_body)],
            ))],
        );
        Ok(CExpression::FunctionCall(FunctionCallExpression::new(
            function_name,
            arguments,
        )))
    }
}

impl Translate<String> for Lambda {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<String> {
        unimplemented!("Lambda#translate()")
    }
}

impl Translate<CExpression> for Module {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Module#translate()")
    }
}

impl Translate<CExpression> for Function {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Function#translate()")
    }
}

impl Translate<CExpression> for Constant {
    fn translate(self, _state: &mut TranslationState) -> TranslatorResult<CExpression> {
        unimplemented!("Constant#translate()")
    }
}

impl Translate<CExpression> for Expression {
    fn translate(self, state: &mut TranslationState) -> TranslatorResult<CExpression> {
        match self {
            Expression::ConstantValue(value) => value.translate(state),
            Expression::Identifier(path) => Ok(CExpression::NamedReference(path.translate(state)?)),
            Expression::Application(application) => application.translate(state),
            Expression::Let(let_expression) => let_expression.translate(state),
            Expression::If(if_expression) => if_expression.translate(state),
            Expression::Lambda(lambda) => Ok(CExpression::NamedReference(lambda.translate(state)?)),
            Expression::Module(module) => module.translate(state),
            Expression::Function(function) => function.translate(state),
            Expression::InternalFunction(_internal_function) => unimplemented!(),
            Expression::Constant(constant) => constant.translate(state),
        }
    }
}
