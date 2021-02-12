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

use crate::generator::{
    Generator, GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator,
};
use crate::syntax::c_type::CType;
use crate::syntax::expression::CExpression;
use crate::syntax::modifiers::Modifiers;
use crate::syntax::typedef::Typedef;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct VariableDeclaration {
    modifiers: Modifiers,
    variable_type: CType,
    name: String,
}

impl VariableDeclaration {
    pub fn new(modifiers: Modifiers, variable_type: CType, name: String) -> Self {
        VariableDeclaration {
            modifiers,
            variable_type,
            name,
        }
    }
}

impl IndentedGenerator for VariableDeclaration {
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

#[derive(Debug)]
pub struct VariableDefinition {
    name: String,
    value: CExpression,
}

impl VariableDefinition {
    pub fn new(name: String, value: CExpression) -> Self {
        VariableDefinition { name, value }
    }
}

impl IndentedGenerator for VariableDefinition {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!(
            "{}{} = {};",
            state.indentation,
            self.name,
            self.value.generate()?,
        ))
    }
}

#[derive(Debug)]
pub struct VariableInstruction {
    modifiers: Modifiers,
    variable_type: CType,
    name: String,
    value: CExpression,
}

impl VariableInstruction {
    pub fn new(
        modifiers: Modifiers,
        variable_type: CType,
        name: String,
        value: CExpression,
    ) -> Self {
        VariableInstruction {
            modifiers,
            variable_type,
            name,
            value,
        }
    }
}

impl IndentedGenerator for VariableInstruction {
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

#[derive(Debug)]
pub struct IfInstruction {
    condition: CExpression,
    body: Instructions,
}

impl IfInstruction {
    pub fn new(condition: CExpression, body: Instructions) -> Self {
        Self { condition, body }
    }
}

impl IndentedGenerator for IfInstruction {
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

#[derive(Debug)]
pub struct IfElseInstruction {
    condition: CExpression,
    if_body: Instructions,
    else_body: Instructions,
}

impl IfElseInstruction {
    pub fn new(condition: CExpression, if_body: Instructions, else_body: Instructions) -> Self {
        Self {
            condition,
            if_body,
            else_body,
        }
    }
}

impl IndentedGenerator for IfElseInstruction {
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
                "foobar".to_string(),
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "const int foobar;",
            VariableDeclaration::new(
                vec![Modifier::Const],
                CType::Primitive(CPrimitiveType::Int),
                "foobar".to_string(),
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
                "foo".to_string(),
                CExpression::NamedReference("bar".to_string())
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
                "foo".to_string(),
                CExpression::NamedReference("bar".to_string())
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "const int foo = bar;",
            VariableInstruction::new(
                vec![Modifier::Const],
                CType::Primitive(CPrimitiveType::Int),
                "foo".to_string(),
                CExpression::NamedReference("bar".to_string())
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
                CExpression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_string()))
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
                CExpression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_string()))
                ],
                vec![
                    Instruction::Expression(CExpression::NamedReference("bar".to_string())),
                    Instruction::Expression(CExpression::NamedReference("foo".to_string()))
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
            Instruction::Expression(CExpression::NamedReference("foobar".to_string()))
                .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "return foobar;",
            Instruction::Return(CExpression::NamedReference("foobar".to_string()))
                .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "int foo = bar;",
            Instruction::Variable(VariableInstruction::new(
                vec![],
                CType::Primitive(CPrimitiveType::Int),
                "foo".to_string(),
                CExpression::NamedReference("bar".to_string())
            ))
            .generate_indented(&GeneratorState::default())?
        );

        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
}"#,
            Instruction::If(IfInstruction::new(
                CExpression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_string()))
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
                CExpression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Expression(CExpression::NamedReference("bar".to_string()))
                ],
                vec![
                    Instruction::Expression(CExpression::NamedReference("bar".to_string())),
                    Instruction::Expression(CExpression::NamedReference("foo".to_string()))
                ],
            ))
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
