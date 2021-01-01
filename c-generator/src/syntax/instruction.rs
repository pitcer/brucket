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

use crate::generator::{Generator, GeneratorError, GeneratorResult};
use crate::syntax::expression::Expression;
use crate::syntax::Type;

pub enum Instruction {
    Expression(Expression),
    Variable(VariableInstruction),
    If(IfInstruction),
    IfElse(IfElseInstruction),
    Return(Expression),
}

impl Generator for Instruction {
    fn generate(&self) -> GeneratorResult {
        match self {
            Instruction::Expression(expression) => Ok(format!("{};", expression.generate()?)),
            Instruction::Variable(variable_instruction) => variable_instruction.generate(),
            Instruction::If(if_instruction) => if_instruction.generate(),
            Instruction::IfElse(if_else_instruction) => if_else_instruction.generate(),
            Instruction::Return(return_expression) => {
                Ok(format!("return {};", return_expression.generate()?))
            }
        }
    }
}

pub struct VariableInstruction {
    variable_type: Type,
    name: String,
    value: Option<Expression>,
}

impl VariableInstruction {
    pub fn new(variable_type: Type, name: String, value: Option<Expression>) -> Self {
        Self {
            variable_type,
            name,
            value,
        }
    }
}

impl Generator for VariableInstruction {
    fn generate(&self) -> GeneratorResult {
        match &self.value {
            Some(value) => Ok(format!(
                "{} {} = {};",
                self.variable_type.generate()?,
                self.name,
                value.generate()?
            )),
            None => Ok(format!("{} {};", self.variable_type.generate()?, self.name)),
        }
    }
}

pub struct IfInstruction {
    condition: Expression,
    body: Instructions,
}

impl IfInstruction {
    pub fn new(condition: Expression, body: Instructions) -> Self {
        Self { condition, body }
    }
}

impl Generator for IfInstruction {
    fn generate(&self) -> GeneratorResult {
        let body = join_instructions(&self.body)?;
        Ok(format!(
            "if ({}) {{\n{}}}",
            self.condition.generate()?,
            body
        ))
    }
}

pub struct IfElseInstruction {
    condition: Expression,
    if_body: Instructions,
    else_body: Instructions,
}

impl IfElseInstruction {
    pub fn new(condition: Expression, if_body: Instructions, else_body: Instructions) -> Self {
        Self {
            condition,
            if_body,
            else_body,
        }
    }
}

impl Generator for IfElseInstruction {
    fn generate(&self) -> GeneratorResult {
        let if_body = join_instructions(&self.if_body)?;
        let else_body = join_instructions(&self.else_body)?;
        Ok(format!(
            "if ({}) {{\n{}}} else {{\n{}}}",
            self.condition.generate()?,
            if_body,
            else_body
        ))
    }
}

pub type Instructions = Vec<Instruction>;

fn join_instructions(instructions: &[Instruction]) -> GeneratorResult {
    Ok(instructions
        .iter()
        .map(|instruction| {
            instruction
                .generate()
                .map(|instruction| format!("    {}\n", instruction))
        })
        .collect::<Result<Vec<String>, GeneratorError>>()?
        .join(""))
}

#[cfg(test)]
mod test {
    use crate::syntax::{PrimitiveType, TestResult};

    use super::*;

    #[test]
    fn test_variable_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foobar;",
            VariableInstruction::new(
                Type::Primitive(PrimitiveType::Int),
                "foobar".to_string(),
                None
            )
            .generate()?
        );
        assert_eq!(
            "int foo = bar;",
            VariableInstruction::new(
                Type::Primitive(PrimitiveType::Int),
                "foo".to_string(),
                Some(Expression::NamedReference("bar".to_string()))
            )
            .generate()?
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
                Expression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(Expression::NamedReference("foo".to_string())),
                    Instruction::Expression(Expression::NamedReference("bar".to_string()))
                ]
            )
            .generate()?
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
                Expression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(Expression::NamedReference("foo".to_string())),
                    Instruction::Expression(Expression::NamedReference("bar".to_string()))
                ],
                vec![
                    Instruction::Expression(Expression::NamedReference("bar".to_string())),
                    Instruction::Expression(Expression::NamedReference("foo".to_string()))
                ],
            )
            .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_instruction_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "foobar;",
            Instruction::Expression(Expression::NamedReference("foobar".to_string())).generate()?
        );
        assert_eq!(
            "return foobar;",
            Instruction::Return(Expression::NamedReference("foobar".to_string())).generate()?
        );
        assert_eq!(
            "int foo = bar;",
            Instruction::Variable(VariableInstruction::new(
                Type::Primitive(PrimitiveType::Int),
                "foo".to_string(),
                Some(Expression::NamedReference("bar".to_string()))
            ))
            .generate()?
        );

        assert_eq!(
            r#"if (foobar) {
    foo;
    bar;
}"#,
            Instruction::If(IfInstruction::new(
                Expression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(Expression::NamedReference("foo".to_string())),
                    Instruction::Expression(Expression::NamedReference("bar".to_string()))
                ]
            ))
            .generate()?
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
                Expression::NamedReference("foobar".to_string()),
                vec![
                    Instruction::Expression(Expression::NamedReference("foo".to_string())),
                    Instruction::Expression(Expression::NamedReference("bar".to_string()))
                ],
                vec![
                    Instruction::Expression(Expression::NamedReference("bar".to_string())),
                    Instruction::Expression(Expression::NamedReference("foo".to_string()))
                ],
            ))
            .generate()?
        );
        Ok(())
    }
}
