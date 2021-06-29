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

use c_generator::syntax::c_type::CType;
use c_generator::syntax::function::{FunctionDeclaration, FunctionDefinition, FunctionHeader};
use c_generator::syntax::instruction::Instructions;
use c_generator::syntax::module::{ModuleMember, ModuleMembers};
use c_generator::syntax::typedef::Typedef;

#[derive(Debug)]
pub struct TranslationState {
    let_count: usize,
    if_count: usize,
    lambda_count: usize,
    typedef_count: usize,
    variables: Vec<Variable>,
    typedefs: ModuleMembers,
    declarations: ModuleMembers,
    members: ModuleMembers,
}

impl Default for TranslationState {
    fn default() -> Self {
        Self {
            let_count: 0,
            if_count: 0,
            lambda_count: 0,
            typedef_count: 0,
            variables: Vec::default(),
            typedefs: ModuleMembers::default(),
            declarations: ModuleMembers::default(),
            members: ModuleMembers::default(),
        }
    }
}

impl TranslationState {
    pub fn add_typedef(&mut self, value: String) {
        let typedef = ModuleMember::Typedef(Typedef::new(value));
        self.typedefs.push(typedef);
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

    pub fn increment_lambda(&mut self) {
        self.lambda_count += 1
    }

    pub fn increment_typedef(&mut self) {
        self.typedef_count += 1
    }

    pub fn get_members(&mut self) -> ModuleMembers {
        let length = self.declarations.len() + self.members.len() + self.typedefs.len();
        let mut members = Vec::with_capacity(length);
        members.append(&mut self.typedefs);
        members.append(&mut self.declarations);
        members.append(&mut self.members);
        members
    }

    pub fn let_count(&self) -> usize {
        self.let_count
    }

    pub fn if_count(&self) -> usize {
        self.if_count
    }

    pub fn lambda_count(&self) -> usize {
        self.lambda_count
    }

    pub fn typedef_count(&self) -> usize {
        self.typedef_count
    }

    pub fn variables(&self) -> &Vec<Variable> {
        &self.variables
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    name: String,
    variable_type: CType,
}

impl Variable {
    pub fn new(name: String, variable_type: CType) -> Self {
        Self {
            name,
            variable_type,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn variable_type(&self) -> &CType {
        &self.variable_type
    }
}
