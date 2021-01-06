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

use crate::analyzer::ast::Arity;
use crate::analyzer::ast::{
    Application, ApplicationIdentifier, If, Lambda, Let, Parameter, Path, Type,
};
use crate::analyzer::ast::{Boolean, ConstantValue, Number};
use crate::analyzer::ast::{ComplexPath, Expression};

pub mod ast;
pub mod type_analyzer;

pub struct Analyzer;

impl Analyzer {
    pub fn analyze(&self, expression: crate::ast::Expression) -> Expression {
        match expression {
            crate::ast::Expression::ConstantValue(v) => Expression::ConstantValue(match v {
                crate::ast::ConstantValue::Unit => ConstantValue::Unit,
                crate::ast::ConstantValue::Null => ConstantValue::Null,
                crate::ast::ConstantValue::Numeric(v) => ConstantValue::Numeric(match v {
                    crate::ast::Number::Integer(v) => Number::Integer(v),
                    crate::ast::Number::FloatingPoint(v) => Number::FloatingPoint(v),
                }),
                crate::ast::ConstantValue::Boolean(v) => ConstantValue::Boolean(match v {
                    crate::ast::Boolean::True => Boolean::True,
                    crate::ast::Boolean::False => Boolean::False,
                }),
                crate::ast::ConstantValue::String(v) => ConstantValue::String(v),
            }),
            crate::ast::Expression::Identifier(path) => Expression::Identifier(analyze_path(path)),
            crate::ast::Expression::Application(application) => {
                let identifier = match *application.identifier {
                    crate::ast::Expression::Identifier(identifier) => {
                        ApplicationIdentifier::Identifier(analyze_path(identifier))
                    }
                    crate::ast::Expression::Lambda(lambda) => {
                        ApplicationIdentifier::Lambda(self.analyze_lambda(lambda))
                    }
                    _ => panic!("Invalid application identifier type"),
                };
                let args = application
                    .arguments
                    .into_iter()
                    .map(|arg| self.analyze(arg))
                    .collect::<Vec<_>>();
                Expression::Application(Application {
                    identifier,
                    arguments: args,
                })
            }
            crate::ast::Expression::InternalCall(_) => unimplemented!(),
            crate::ast::Expression::Let(let_expr) => Expression::Let(Let {
                name: let_expr.name,
                value: Box::new(self.analyze(*let_expr.value)),
                then: Box::new(self.analyze(*let_expr.then)),
            }),
            crate::ast::Expression::If(if_expr) => Expression::If(If {
                condition: Box::new(self.analyze(*if_expr.condition)),
                if_true: Box::new(self.analyze(*if_expr.if_true)),
                if_false: Box::new(self.analyze(*if_expr.if_false)),
            }),
            crate::ast::Expression::Lambda(lambda) => {
                Expression::Lambda(self.analyze_lambda(lambda))
            }
            crate::ast::Expression::Module(_) => unimplemented!(),
            crate::ast::Expression::Function(_) => unimplemented!(),
            crate::ast::Expression::Constant(_) => unimplemented!(),
        }
    }

    fn analyze_lambda(&self, lambda: crate::ast::Lambda) -> Lambda {
        Lambda {
            return_type: lambda.return_type().into(),
            used_identifiers: lambda.used_identifiers().clone(),
            parameters: lambda
                .parameters()
                .iter()
                .map(|p| p.into())
                .collect::<Vec<_>>(),
            body: Box::new(self.analyze(lambda.body().clone())),
        }
    }
}

fn analyze_path(path: crate::ast::Path) -> Path {
    match path {
        crate::ast::Path::Simple(s) => Path::Simple(s),
        crate::ast::Path::Complex(s) => Path::Complex(ComplexPath::new(s.identifier, s.path)),
    }
}

impl From<&crate::ast::Parameter> for Parameter {
    fn from(param: &crate::ast::Parameter) -> Self {
        Parameter {
            name: param.name.clone(),
            parameter_type: (&param.parameter_type).into(),
            arity: match param.arity {
                crate::ast::Arity::Unary => Arity::Unary,
                crate::ast::Arity::Variadic => Arity::Variadic,
            },
        }
    }
}

impl From<&crate::ast::Type> for Type {
    fn from(ast_type: &crate::ast::Type) -> Self {
        match ast_type {
            crate::ast::Type::Boolean => Type::Boolean,
            crate::ast::Type::Integer => Type::Integer,
            crate::ast::Type::String => Type::String,
            crate::ast::Type::Any => Type::Any,
            crate::ast::Type::Symbol(s) => Type::Symbol(s.clone()),
        }
    }
}
