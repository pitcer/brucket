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
    #[inline]
    pub fn add_typedef(&mut self, value: String) {
        let typedef = ModuleMember::Typedef(Typedef::new(value));
        self.typedefs.push(typedef);
    }

    #[inline]
    pub fn add_function(&mut self, header: FunctionHeader, body: Instructions) {
        let function_declaration =
            ModuleMember::FunctionDeclaration(FunctionDeclaration::new(header.clone()));
        let function_definition =
            ModuleMember::FunctionDefinition(FunctionDefinition::new(header, body));
        self.declarations.push(function_declaration);
        self.members.push(function_definition);
    }

    #[inline]
    pub fn add_struct(&mut self, c_struct: CStruct) {
        let c_struct = ModuleMember::Struct(c_struct);
        self.structs.push(c_struct);
    }

    #[inline]
    pub fn push_variable(&mut self, variable: Variable) {
        self.variables.push(variable);
    }

    #[inline]
    pub fn pop_variable(&mut self) {
        self.variables.pop();
    }

    #[inline]
    #[must_use]
    pub fn contains_variable(&self, variable: &Variable) -> bool {
        self.variables.contains(variable)
    }

    #[inline]
    pub fn increment_let(&mut self) {
        self.let_count += 1;
    }

    #[inline]
    pub fn increment_if(&mut self) {
        self.if_count += 1;
    }

    #[inline]
    pub fn increment_lambda(&mut self) {
        self.lambda_count += 1;
    }

    #[inline]
    pub fn increment_typedef(&mut self) {
        self.typedef_count += 1;
    }

    #[inline]
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Constructor)]
pub struct Variable {
    pub name: String,
    pub variable_type: CType,
}
