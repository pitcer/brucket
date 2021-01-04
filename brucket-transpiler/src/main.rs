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

#![forbid(unsafe_code)]

use std::io;
use std::io::Read;

use brucket_ast::lexer::Lexer;
use brucket_ast::parser::Parser;
use c_generator::generator::Generator;
use c_generator::syntax::c_macro::{DefineMacro, Macro};
use c_generator::syntax::expression::{Expression, FunctionCallExpression, NumberExpression};
use c_generator::syntax::function::{Function, FunctionParameter, Parameters};
use c_generator::syntax::instruction::Instruction;
use c_generator::syntax::module::{Module, ModuleMember};
use c_generator::syntax::{PrimitiveType, Type};
use translator::Translate;

use crate::translator::TranslationState;
use std::borrow::Cow;

mod translator;
mod transpiler;

fn main() -> Result<(), Cow<'static, str>> {
    let mut standard_input = io::stdin();
    let syntax = read(&mut standard_input).expect("Cannot read syntax from standard input");
    let lexer = Lexer::default();
    let parser = Parser::default();
    let tokens = lexer.tokenize(syntax.into())?;
    let expression = parser.parse(tokens)?;
    let state = TranslationState::new(0, 0);
    let (expression, mut expression_members) = expression.translate(state)?;
    let include_stdio_macro = ModuleMember::Macro(Macro::Include("stdio.h".to_string()));
    let define_unit_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
        "UNIT".to_string(),
        "0".to_string(),
    )));
    let define_true_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
        "TRUE".to_string(),
        "1".to_string(),
    )));
    let define_false_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
        "FALSE".to_string(),
        "0".to_string(),
    )));
    let plus_function = create_binary_operator_function("__internal_plus", '+');
    let minus_function = create_binary_operator_function("__internal_minus", '-');
    let times_function = create_binary_operator_function("__internal_times", '*');
    let divide_function = create_binary_operator_function("__internal_divide", '/');
    let remainder_function = create_binary_operator_function("__internal_remainder", '%');
    let main_function = ModuleMember::Function(Function::new(
        Type::Primitive(PrimitiveType::Int),
        "main".to_string(),
        Parameters::default(),
        vec![
            Instruction::Expression(Expression::FunctionCall(FunctionCallExpression::new(
                "printf".to_string(),
                vec![Expression::String("%d\\n".to_string()), expression],
            ))),
            Instruction::Return(Expression::Number(NumberExpression::Integer(
                "0".to_string(),
            ))),
        ],
    ));
    let mut members = Vec::with_capacity(expression_members.len() + 5);
    members.push(include_stdio_macro);
    members.push(define_unit_macro);
    members.push(define_true_macro);
    members.push(define_false_macro);
    members.push(plus_function);
    members.push(minus_function);
    members.push(times_function);
    members.push(divide_function);
    members.push(remainder_function);
    members.append(&mut expression_members);
    members.push(main_function);
    let module = Module::new(members);
    let generated = module.generate()?;
    println!("{}", generated);
    Ok(())
}

fn create_binary_operator_function(name: &str, operator: char) -> ModuleMember {
    ModuleMember::Function(Function::new(
        Type::Primitive(PrimitiveType::Int),
        name.to_string(),
        vec![
            FunctionParameter::new(Type::Primitive(PrimitiveType::Int), "first".to_string()),
            FunctionParameter::new(Type::Primitive(PrimitiveType::Int), "second".to_string()),
        ],
        vec![Instruction::Return(Expression::NamedReference(format!(
            "first {} second",
            operator
        )))],
    ))
}

fn read(input: &mut impl Read) -> io::Result<String> {
    let mut result = String::new();
    input.read_to_string(&mut result)?;
    Ok(result)
}
