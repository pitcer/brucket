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

use c_generator::syntax::c_struct::CStruct;
use c_generator::syntax::c_type::CType;
use c_generator::syntax::function::{FunctionDeclaration, FunctionDefinition, FunctionHeader};
use c_generator::syntax::instruction::Instructions;
use c_generator::syntax::module::{ModuleMember, ModuleMembers};
use c_generator::syntax::typedef::Typedef;
use derive_more::Constructor;

#[derive(Debug, Default)]
pub struct TranslationState {
    pub let_count: usize,
    pub if_count: usize,
    pub lambda_count: usize,
    pub typedef_count: usize,
    pub variables: Vec<Variable>,
    pub typedefs: ModuleMembers,
    pub declarations: ModuleMembers,
    pub members: ModuleMembers,
    pub structs: ModuleMembers,
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

    pub fn add_struct(&mut self, c_struct: CStruct) {
        let c_struct = ModuleMember::Struct(c_struct);
        self.structs.push(c_struct);
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

    pub fn all_members(&mut self) -> ModuleMembers {
        let length =
            self.declarations.len() + self.members.len() + self.typedefs.len() + self.structs.len();
        let mut members = Vec::with_capacity(length);
        members.append(&mut self.typedefs);
        members.append(&mut self.structs);
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Constructor)]
pub struct Variable {
    pub name: String,
    pub variable_type: CType,
}
