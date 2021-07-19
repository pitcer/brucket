use crate::generator::{
    Generator, GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator,
};
use crate::syntax::c_type::CType;
use crate::syntax::expression::CExpression;
use crate::syntax::modifiers::Modifiers;
use crate::syntax::typedef::Typedef;
use derive_more::Constructor;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Expression(CExpression),
    VariableDeclaration(VariableDeclaration),
    VariableDefinition(VariableDefinition),
    Variable(VariableInstruction),
    If(IfInstruction),
    IfElse(IfElseInstruction),
    Typedef(Typedef),
    Return(CExpression),
}

impl IndentedGenerator for Instruction {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let indentation = &state.indentation;
        match self {
            Instruction::Expression(expression) => {
                Ok(format!("{}{};", indentation, expression.generate()?))
            }
            Instruction::VariableDeclaration(variable) => variable.generate_indented(state),
            Instruction::VariableDefinition(variable) => variable.generate_indented(state),
            Instruction::Variable(variable) => variable.generate_indented(state),
            Instruction::If(instruction) => instruction.generate_indented(state),
            Instruction::IfElse(instruction) => instruction.generate_indented(state),
            Instruction::Typedef(typedef) => typedef.generate_indented(state),
            Instruction::Return(return_expression) => Ok(format!(
                "{}return {};",
                indentation,
                return_expression.generate()?
            )),
        }
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct VariableDeclaration {
    pub modifiers: Modifiers,
    pub variable_type: CType,
    pub name: String,
}

impl IndentedGenerator for VariableDeclaration {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let mut modifiers = self.modifiers.generate()?;
        if !modifiers.is_empty() {
            modifiers.push(' ');
        }
        Ok(format!(
            "{}{}{} {};",
            state.indentation,
            modifiers,
            self.variable_type.generate()?,
            self.name
        ))
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct VariableDefinition {
    name: String,
    value: CExpression,
}

impl IndentedGenerator for VariableDefinition {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!(
            "{}{} = {};",
            state.indentation,
            self.name,
            self.value.generate()?,
        ))
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct VariableInstruction {
    modifiers: Modifiers,
    variable_type: CType,
    name: String,
    value: CExpression,
}

impl IndentedGenerator for VariableInstruction {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let mut modifiers = self.modifiers.generate()?;
        if !modifiers.is_empty() {
            modifiers.push(' ')
        }
        Ok(format!(
            "{}{}{} {} = {};",
            state.indentation,
            modifiers,
            self.variable_type.generate()?,
            self.name,
            self.value.generate()?
        ))
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct IfInstruction {
    condition: CExpression,
    body: Instructions,
}

impl IndentedGenerator for IfInstruction {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let indentation = &state.indentation;
        let incremented_indentation = state.indentation.to_incremented();
        let state = GeneratorState::new(incremented_indentation);
        Ok(format!(
            "{}if ({}) {{\n{}\n{}}}",
            indentation,
            self.condition.generate()?,
            self.body.generate_indented(&state)?,
            indentation
        ))
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct IfElseInstruction {
    condition: CExpression,
    if_body: Instructions,
    else_body: Instructions,
}

impl IndentedGenerator for IfElseInstruction {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let indentation = &state.indentation;
        let incremented_indentation = state.indentation.to_incremented();
        let state = GeneratorState::new(incremented_indentation);
        Ok(format!(
            "{}if ({}) {{\n{}\n{}}} else {{\n{}\n{}}}",
            indentation,
            self.condition.generate()?,
            self.if_body.generate_indented(&state)?,
            indentation,
            self.else_body.generate_indented(&state)?,
            indentation
        ))
    }
}

pub type Instructions = Vec<Instruction>;

impl IndentedGenerator for Instructions {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(|instruction| instruction.generate_indented(state))
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n"))
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::c_type::CPrimitiveType;
    use crate::syntax::modifiers::Modifier;
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_variable_declaration_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foobar;",
            VariableDeclaration::new(
                vec![],
                CType::Primitive(CPrimitiveType::Int),
                "foobar".to_owned(),
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "const int foobar;",
            VariableDeclaration::new(
                vec![Modifier::Const],
                CType::Primitive(CPrimitiveType::Int),
                "foobar".to_owned(),
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_variable_definition_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "foo = bar;",
            VariableDefinition::new(
                "foo".to_owned(),
                CExpression::NamedReference("bar".to_owned())
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_variable_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foo = bar;",
            VariableInstruction::new(
                vec![],
                CType::Primitive(CPrimitiveType::Int),
                "foo".to_owned(),
                CExpression::NamedReference("bar".to_owned())
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "const int foo = bar;",
            VariableInstruction::new(
                vec![Modifier::Const],
                CType::Primitive(CPrimitiveType::Int),
                "foo".to_owned(),
                CExpression::NamedReference("bar".to_owned())
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_if_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
}"#,
            IfInstruction::new(
                CExpression::NamedReference("foobar".to_owned()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned()))
                ]
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_if_else_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
} else {
    bar;
    foo;
}"#,
            IfElseInstruction::new(
                CExpression::NamedReference("foobar".to_owned()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned()))
                ],
                vec![
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned()))
                ],
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "foobar;",
            Instruction::Expression(CExpression::NamedReference("foobar".to_owned()))
                .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "return foobar;",
            Instruction::Return(CExpression::NamedReference("foobar".to_owned()))
                .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "int foo = bar;",
            Instruction::Variable(VariableInstruction::new(
                vec![],
                CType::Primitive(CPrimitiveType::Int),
                "foo".to_owned(),
                CExpression::NamedReference("bar".to_owned())
            ))
            .generate_indented(&GeneratorState::default())?
        );

        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
}"#,
            Instruction::If(IfInstruction::new(
                CExpression::NamedReference("foobar".to_owned()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned()))
                ]
            ))
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
} else {
    bar;
    foo;
}"#,
            Instruction::IfElse(IfElseInstruction::new(
                CExpression::NamedReference("foobar".to_owned()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned()))
                ],
                vec![
                    Instruction::Expression(CExpression::NamedReference("bar".to_owned())),
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned()))
                ],
            ))
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
