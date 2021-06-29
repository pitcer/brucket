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
use crate::syntax::instruction::Instructions;
use derive_more::Constructor;

#[derive(Debug, Constructor)]
pub struct FunctionDeclaration {
    header: FunctionHeader,
}

impl IndentedGenerator for FunctionDeclaration {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!("{}{};", state.indentation, self.header.generate()?))
    }
}

#[derive(Debug, Constructor)]
pub struct FunctionDefinition {
    header: FunctionHeader,
    body: Instructions,
}

impl IndentedGenerator for FunctionDefinition {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let indentation = &state.indentation;
        let incremented_indentation = state.indentation.to_incremented();
        let state = GeneratorState::new(incremented_indentation);
        Ok(format!(
            "{}{} {{\n{}\n{}}}",
            indentation,
            self.header.generate()?,
            self.body.generate_indented(&state)?,
            indentation
        ))
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct FunctionHeader {
    return_type: CType,
    name: String,
    parameters: Parameters,
}

impl Generator for FunctionHeader {
    fn generate(self) -> GeneratorResult {
        Ok(format!(
            "{} {}({})",
            self.return_type.generate()?,
            self.name,
            self.parameters.generate()?
        ))
    }
}

pub type Parameters = Vec<FunctionParameter>;

impl Generator for Parameters {
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", "))
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct FunctionParameter {
    parameter_type: CType,
    name: String,
}

impl Generator for FunctionParameter {
    fn generate(self) -> GeneratorResult {
        Ok(format!("{} {}", self.parameter_type.generate()?, self.name))
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::c_type::CPrimitiveType;
    use crate::syntax::expression::CExpression;
    use crate::syntax::instruction::Instruction;
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_function_parameter_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foobar",
            FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "foobar".to_string())
                .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_function_header_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foobar(int foo, int bar)",
            FunctionHeader::new(
                CType::Primitive(CPrimitiveType::Int),
                "foobar".to_string(),
                vec![
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "foo".to_string()
                    ),
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "bar".to_string()
                    )
                ],
            )
            .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_function_declaration_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "int foobar(int foo, int bar);",
            FunctionDeclaration::new(FunctionHeader::new(
                CType::Primitive(CPrimitiveType::Int),
                "foobar".to_string(),
                vec![
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "foo".to_string()
                    ),
                    FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "bar".to_string()
                    )
                ],
            ))
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_function_definition_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            r#"int foobar() {

}"#,
            FunctionDefinition::new(
                FunctionHeader::new(
                    CType::Primitive(CPrimitiveType::Int),
                    "foobar".to_string(),
                    Parameters::default()
                ),
                Instructions::default()
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"int foobar() {
    return bar;
}"#,
            FunctionDefinition::new(
                FunctionHeader::new(
                    CType::Primitive(CPrimitiveType::Int),
                    "foobar".to_string(),
                    Parameters::default()
                ),
                vec![Instruction::Return(CExpression::NamedReference(
                    "bar".to_string()
                ))]
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"int foobar(int foo) {
    foo;
    return bar;
}"#,
            FunctionDefinition::new(
                FunctionHeader::new(
                    CType::Primitive(CPrimitiveType::Int),
                    "foobar".to_string(),
                    vec![FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "foo".to_string()
                    )]
                ),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Return(CExpression::NamedReference("bar".to_string()))
                ]
            )
            .generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            r#"int foobar(int foo, int bar) {
    foo;
    return bar;
}"#,
            FunctionDefinition::new(
                FunctionHeader::new(
                    CType::Primitive(CPrimitiveType::Int),
                    "foobar".to_string(),
                    vec![
                        FunctionParameter::new(
                            CType::Primitive(CPrimitiveType::Int),
                            "foo".to_string()
                        ),
                        FunctionParameter::new(
                            CType::Primitive(CPrimitiveType::Int),
                            "bar".to_string()
                        )
                    ]
                ),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_string())),
                    Instruction::Return(CExpression::NamedReference("bar".to_string()))
                ]
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
