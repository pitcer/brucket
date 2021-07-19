use crate::generator::{GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator};
use crate::syntax::c_macro::Macro;
use crate::syntax::c_struct::CStruct;
use crate::syntax::function::{FunctionDeclaration, FunctionDefinition};
use crate::syntax::instruction::{VariableDeclaration, VariableInstruction};
use crate::syntax::typedef::Typedef;
use derive_more::Constructor;

#[derive(Constructor)]
pub struct Module {
    members: ModuleMembers,
}

impl IndentedGenerator for Module {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        self.members.generate_indented(state)
    }
}

pub type ModuleMembers = Vec<ModuleMember>;

impl IndentedGenerator for ModuleMembers {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(|member| member.generate_indented(state))
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n\n"))
    }
}

#[derive(Debug, PartialEq)]
pub enum ModuleMember {
    Macro(Macro),
    FunctionDeclaration(FunctionDeclaration),
    FunctionDefinition(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
    Variable(VariableInstruction),
    Typedef(Typedef),
    Struct(CStruct),
}

impl IndentedGenerator for ModuleMember {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        match self {
            ModuleMember::Macro(c_macro) => c_macro.generate_indented(state),
            ModuleMember::FunctionDeclaration(function) => function.generate_indented(state),
            ModuleMember::FunctionDefinition(function) => function.generate_indented(state),
            ModuleMember::VariableDeclaration(variable) => variable.generate_indented(state),
            ModuleMember::Variable(variable) => variable.generate_indented(state),
            ModuleMember::Typedef(typedef) => typedef.generate_indented(state),
            ModuleMember::Struct(c_struct) => c_struct.generate_indented(state),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::c_type::{CPrimitiveType, CType};
    use crate::syntax::expression::CExpression;
    use crate::syntax::function::{FunctionHeader, Parameters};
    use crate::syntax::instruction::Instructions;
    use crate::syntax::TestResult;

    use super::*;
    use crate::syntax::modifiers::Modifier;

    #[test]
    fn test_module_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "",
            Module::new(ModuleMembers::default()).generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"#include <test.h>

static int FOOBAR = test;

int foobar() {

}"#,
            Module::new(vec![
                ModuleMember::Macro(Macro::Include("test.h".to_owned())),
                ModuleMember::Variable(VariableInstruction::new(
                    vec![Modifier::Static],
                    CType::Primitive(CPrimitiveType::Int),
                    "FOOBAR".to_owned(),
                    CExpression::NamedReference("test".to_owned())
                )),
                ModuleMember::FunctionDefinition(FunctionDefinition::new(
                    FunctionHeader::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "foobar".to_owned(),
                        Parameters::default()
                    ),
                    Instructions::default()
                ))
            ])
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_module_member_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "#include <test.h>",
            ModuleMember::Macro(Macro::Include("test.h".to_owned()))
                .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"int foobar() {

}"#,
            ModuleMember::FunctionDefinition(FunctionDefinition::new(
                FunctionHeader::new(
                    CType::Primitive(CPrimitiveType::Int),
                    "foobar".to_owned(),
                    Parameters::default()
                ),
                Instructions::default()
            ))
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "static int FOOBAR = test;",
            ModuleMember::Variable(VariableInstruction::new(
                vec![Modifier::Static],
                CType::Primitive(CPrimitiveType::Int),
                "FOOBAR".to_owned(),
                CExpression::NamedReference("test".to_owned())
            ))
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
