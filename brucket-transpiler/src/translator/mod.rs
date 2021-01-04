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

use brucket_ast::ast::{
    Boolean, ConstantValue, Expression as BrucketExpression, Expression, Number, Path,
};
use c_generator::syntax::expression::{
    Arguments, Expression as CExpression, FunctionCallExpression, NumberExpression,
};
use c_generator::syntax::function::{Function, Parameters};
use c_generator::syntax::instruction::{IfElseInstruction, Instruction, VariableInstruction};
use c_generator::syntax::module::{ModuleMember, ModuleMembers};
use c_generator::syntax::{PrimitiveType, Type};

pub type TranslatorResult<T> = Result<(T, ModuleMembers), TranslatorError>;
pub type TranslatorError = String;

pub trait Translator<F, T> {
    fn translate(&self, from: F) -> TranslatorResult<T>;
}

pub trait Translatable<T> {
    fn translate(&self, state: TranslationState) -> TranslatorResult<T>;
}

pub struct TranslationState {
    let_count: i32,
    if_count: i32,
}

impl TranslationState {
    pub fn new(let_count: i32, if_count: i32) -> Self {
        Self {
            let_count,
            if_count,
        }
    }

    pub fn incremented(&self) -> Self {
        Self::new(self.let_count + 1, self.if_count + 1)
    }
}

impl Translatable<CExpression> for ConstantValue {
    fn translate(&self, _state: TranslationState) -> TranslatorResult<CExpression> {
        let expression = match self {
            ConstantValue::Unit => CExpression::NamedReference("UNIT".to_string()),
            ConstantValue::Null => CExpression::NamedReference("NULL".to_string()),
            ConstantValue::Numeric(numeric) => match numeric {
                Number::Integer(value) => {
                    CExpression::Number(NumberExpression::Integer(value.clone()))
                }
                Number::FloatingPoint(value) => {
                    CExpression::Number(NumberExpression::FloatingPoint(value.clone()))
                }
            },
            ConstantValue::Boolean(boolean) => match boolean {
                Boolean::True => CExpression::NamedReference("TRUE".to_string()),
                Boolean::False => CExpression::NamedReference("FALSE".to_string()),
            },
            ConstantValue::String(string) => CExpression::String(string.to_string()),
        };
        Ok((expression, ModuleMembers::default()))
    }
}

impl Translatable<CExpression> for Path {
    fn translate(&self, _state: TranslationState) -> TranslatorResult<CExpression> {
        let expression = match self {
            Path::Simple(path) => {
                let path = path
                    .replace("+", "__internal_plus")
                    .replace("-", "__internal_minus")
                    .replace("*", "__internal_times")
                    .replace("/", "__internal_divide")
                    .replace("%", "__internal_remainder");
                CExpression::NamedReference(path)
            }
            Path::Complex(complex_path) => CExpression::NamedReference(format!(
                "{}_{}",
                complex_path.path().join("_"),
                complex_path.identifier()
            )),
        };
        Ok((expression, ModuleMembers::default()))
    }
}

impl Translatable<CExpression> for BrucketExpression {
    fn translate(&self, state: TranslationState) -> TranslatorResult<CExpression> {
        let mut members = ModuleMembers::new();
        let expression = match self {
            Expression::ConstantValue(value) => {
                let mut value = value.translate(state.incremented())?;
                members.append(&mut value.1);
                Ok(value.0)
            }
            Expression::Identifier(path) => {
                let mut value = path.translate(state.incremented())?;
                members.append(&mut value.1);
                Ok(value.0)
            }
            Expression::Application(identifier, arguments) => {
                let name = identifier.translate(state.incremented())?.0;
                if let CExpression::NamedReference(name) = name {
                    let arguments = arguments
                        .iter()
                        .map(|argument| argument.translate(state.incremented().incremented()))
                        .collect::<Result<Vec<(CExpression, ModuleMembers)>, TranslatorError>>()?;
                    let (arguments, application_members) = unfold_tuple_vector(arguments);
                    let mut application_members =
                        application_members.into_iter().flatten().collect();
                    members.append(&mut application_members);
                    Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                        name, arguments,
                    )))
                } else {
                    Err("Unsupported function name in application type".to_string())
                }
            }
            Expression::InternalCall(identifier, arguments) => {
                let arguments = arguments
                    .iter()
                    .map(|argument| argument.translate(state.incremented()))
                    .collect::<Result<Vec<(CExpression, ModuleMembers)>, TranslatorError>>()?;
                let (arguments, application_members) = unfold_tuple_vector(arguments);
                let mut application_members = application_members.into_iter().flatten().collect();
                members.append(&mut application_members);
                Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                    identifier.to_string(),
                    arguments,
                )))
            }
            Expression::Let(name, value, next) => {
                let (value, mut value_members) = value.translate(state.incremented())?;
                let (next, mut next_members) = next.translate(state.incremented().incremented())?;
                members.append(&mut value_members);
                members.append(&mut next_members);
                let function_name = format!("__internal_let_{}_{}", state.let_count, name);
                let function = ModuleMember::Function(Function::new(
                    Type::Primitive(PrimitiveType::Int),
                    function_name.clone(),
                    Parameters::default(),
                    vec![
                        Instruction::Variable(VariableInstruction::new(
                            Type::Primitive(PrimitiveType::Int),
                            name.to_string(),
                            Some(value),
                        )),
                        Instruction::Return(next),
                    ],
                ));
                members.push(function);
                Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                    function_name,
                    Arguments::default(),
                )))
            }
            Expression::If(if_expression) => {
                let (condition, mut condition_members) =
                    if_expression.condition.translate(state.incremented())?;
                let (if_body, mut if_members) = if_expression
                    .if_true
                    .translate(state.incremented().incremented())?;
                let (else_body, mut else_members) = if_expression
                    .if_false
                    .translate(state.incremented().incremented().incremented())?;
                members.append(&mut condition_members);
                members.append(&mut if_members);
                members.append(&mut else_members);
                let function_name = format!("__internal_if_{}", state.if_count);
                let function = ModuleMember::Function(Function::new(
                    Type::Primitive(PrimitiveType::Int),
                    function_name.clone(),
                    Parameters::default(),
                    vec![Instruction::IfElse(IfElseInstruction::new(
                        condition,
                        vec![Instruction::Return(if_body)],
                        vec![Instruction::Return(else_body)],
                    ))],
                ));
                members.push(function);
                Ok(CExpression::FunctionCall(FunctionCallExpression::new(
                    function_name,
                    Arguments::default(),
                )))
            }
            Expression::Lambda(_) => Err("Unsupported expression".to_string()),
            Expression::Module(_) => Err("Unsupported expression".to_string()),
            Expression::Function(_, _, _, _) => Err("Unsupported expression".to_string()),
            Expression::Constant(_, _, _) => Err("Unsupported expression".to_string()),
        };
        expression.map(|expression| (expression, members))
    }
}

fn unfold_tuple_vector<T1, T2>(vector: Vec<(T1, T2)>) -> (Vec<T1>, Vec<T2>) {
    let length = vector.len();
    vector.into_iter().fold(
        (Vec::with_capacity(length), Vec::with_capacity(length)),
        |mut accumulator, element| {
            accumulator.0.push(element.0);
            accumulator.1.push(element.1);
            (accumulator.0, accumulator.1)
        },
    )
}
