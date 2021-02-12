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

use brucket_ast::lexer::Lexer;
use brucket_ast::parser::Parser;
use c_generator::generator::{GeneratorResult, GeneratorState, IndentedGenerator};
use c_generator::syntax::c_macro::{DefineMacro, Macro};
use c_generator::syntax::expression::{
    CExpression, FunctionCallExpression, FunctionIdentifier, NumberExpression,
};
use c_generator::syntax::function::{
    FunctionDefinition, FunctionHeader, FunctionParameter, Parameters,
};
use c_generator::syntax::instruction::Instruction;
use c_generator::syntax::module::{Module, ModuleMember, ModuleMembers};

use crate::translator::state::TranslationState;
use crate::translator::Translate;
use brucket_ast::analyzer::type_analyzer::{Environment, Typed};
use brucket_ast::ast::{LambdaType, Path, Type};
use c_generator::syntax::c_type::{CPrimitiveType, CType};
use std::borrow::Cow;

pub struct Transpiler;

impl Default for Transpiler {
    fn default() -> Self {
        Self {}
    }
}

impl Transpiler {
    pub fn transpile(&self, syntax: Cow<str>) -> GeneratorResult {
        let lexer = Lexer::default();
        let parser = Parser::default();
        let tokens = lexer.tokenize(syntax)?;
        let expression = parser.parse(tokens)?;
        let mut type_analyzer_environment = self.create_type_analyzer_environment();
        let expression = expression.into_typed(&mut type_analyzer_environment)?;
        let mut state = TranslationState::default();
        let expression = expression.translate(&mut state)?;
        let expression_members = state.into_members();
        let members = self.create_module_members(expression, expression_members);
        let module = Module::new(members);
        let generator_state = GeneratorState::default();
        module.generate_indented(&generator_state)
    }

    fn create_type_analyzer_environment(&self) -> Environment {
        let mut environment = Environment::default();
        let binary_int_lambda_type = Type::Lambda(LambdaType::new(
            vec![Type::Integer, Type::Integer],
            Type::Integer.into(),
        ));
        environment.insert_variable(Path::Simple("+".to_owned()), binary_int_lambda_type.clone());
        environment.insert_variable(Path::Simple("-".to_owned()), binary_int_lambda_type.clone());
        environment.insert_variable(Path::Simple("*".to_owned()), binary_int_lambda_type.clone());
        environment.insert_variable(Path::Simple("/".to_owned()), binary_int_lambda_type.clone());
        environment.insert_variable(Path::Simple("%".to_owned()), binary_int_lambda_type);
        environment
    }

    fn create_module_members(
        &self,
        expression: CExpression,
        mut expression_members: ModuleMembers,
    ) -> Vec<ModuleMember> {
        let include_stdio_macro = ModuleMember::Macro(Macro::Include("stdio.h".to_string()));
        let define_unit_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
            "BOOL".to_string(),
            "signed char".to_string(),
        )));
        let define_true_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
            "TRUE".to_string(),
            "1".to_string(),
        )));
        let define_false_macro = ModuleMember::Macro(Macro::Define(DefineMacro::new(
            "FALSE".to_string(),
            "0".to_string(),
        )));
        let plus_function = self.create_binary_operator_function("__$internal_add", '+');
        let minus_function = self.create_binary_operator_function("__$internal_subtract", '-');
        let times_function = self.create_binary_operator_function("__$internal_multiply", '*');
        let divide_function = self.create_binary_operator_function("__$internal_divide", '/');
        let remainder_function = self.create_binary_operator_function("__$internal_remainder", '%');
        let main_function = self.create_main_function(expression);
        let mut members = Vec::with_capacity(expression_members.len() + 10);
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
        members
    }

    fn create_main_function(&self, expression: CExpression) -> ModuleMember {
        ModuleMember::FunctionDefinition(FunctionDefinition::new(
            FunctionHeader::new(
                CType::Primitive(CPrimitiveType::Int),
                "main".to_string(),
                Parameters::default(),
            ),
            vec![
                Instruction::Expression(CExpression::FunctionCall(FunctionCallExpression::new(
                    FunctionIdentifier::NamedReference("printf".to_string()),
                    vec![CExpression::String("%d\\n".to_string()), expression],
                ))),
                Instruction::Return(CExpression::Number(NumberExpression::Integer(
                    "0".to_string(),
                ))),
            ],
        ))
    }

    fn create_binary_operator_function(&self, name: &str, operator: char) -> ModuleMember {
        ModuleMember::FunctionDefinition(FunctionDefinition::new(
            FunctionHeader::new(
                CType::Primitive(CPrimitiveType::Int),
                name.to_string(),
                vec![
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "first".to_string(),
                    ),
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "second".to_string(),
                    ),
                ],
            ),
            vec![Instruction::Return(CExpression::NamedReference(format!(
                "first {} second",
                operator
            )))],
        ))
    }
}
