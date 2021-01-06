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

use brucket_ast::analyzer::type_analyzer::Environment;
use c_generator::syntax::function::{FunctionDeclaration, FunctionDefinition, FunctionHeader};
use c_generator::syntax::instruction::Instructions;
use c_generator::syntax::module::{ModuleMember, ModuleMembers};
use c_generator::syntax::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TranslationState {
    let_count: i32,
    if_count: i32,
    variables: Vec<Variable>,
    declarations: ModuleMembers,
    members: ModuleMembers,
    pub env: Environment,
}

impl Default for TranslationState {
    fn default() -> Self {
        Self::new(
            0,
            0,
            Vec::default(),
            ModuleMembers::default(),
            ModuleMembers::default(),
            Environment {
                variables: HashMap::default(),
            },
        )
    }
}

impl TranslationState {
    pub fn new(
        let_count: i32,
        if_count: i32,
        variables: Vec<Variable>,
        declarations: ModuleMembers,
        members: ModuleMembers,
        env: Environment,
    ) -> Self {
        Self {
            let_count,
            if_count,
            variables,
            declarations,
            members,
            env,
        }
    }

    pub fn add_function(&mut self, header: FunctionHeader, body: Instructions) {
        let function_declaration =
            ModuleMember::FunctionDeclaration(FunctionDeclaration::new(header.clone()));
        let function_definition =
            ModuleMember::FunctionDefinition(FunctionDefinition::new(header, body));
        self.declarations.push(function_declaration);
        self.members.push(function_definition);
    }

    pub fn push_variable(&mut self, variable: Variable) {
        self.variables.push(variable);
    }

    pub fn pop_variable(&mut self) {
        self.variables.pop();
    }

    pub fn contains_variable(&self, variable: &Variable) -> bool {
        self.variables.contains(variable)
    }

    pub fn increment_let(&mut self) {
        self.let_count += 1
    }

    pub fn increment_if(&mut self) {
        self.if_count += 1
    }

    pub fn into_members(mut self) -> ModuleMembers {
        self.declarations.append(&mut self.members);
        self.declarations
    }

    pub fn let_count(&self) -> i32 {
        self.let_count
    }

    pub fn if_count(&self) -> i32 {
        self.if_count
    }

    pub fn variables(&self) -> &Vec<Variable> {
        &self.variables
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    name: String,
    variable_type: Type,
}

impl Variable {
    pub fn new(name: String, variable_type: Type) -> Self {
        Self {
            name,
            variable_type,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn variable_type(&self) -> &Type {
        &self.variable_type
    }
}
