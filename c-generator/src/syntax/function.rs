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
use crate::syntax::c_type::CType;
use crate::syntax::instruction::Instructions;

#[derive(Debug)]
pub struct FunctionDeclaration {
    header: FunctionHeader,
}

impl FunctionDeclaration {
    pub fn new(header: FunctionHeader) -> Self {
        Self { header }
    }
}

impl Generator for FunctionDeclaration {
    fn generate(self) -> GeneratorResult {
        let mut header = self.header.generate()?;
        header.push(';');
        Ok(header)
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    header: FunctionHeader,
    body: Instructions,
}

impl FunctionDefinition {
    pub fn new(header: FunctionHeader, body: Instructions) -> Self {
        Self { header, body }
    }
}

impl Generator for FunctionDefinition {
    fn generate(self) -> GeneratorResult {
        let header = self.header.generate()?;
        let body = self
            .body
            .into_iter()
            .map(|instruction| {
                instruction
                    .generate()
                    .map(|instruction| format!("    {}", instruction))
            })
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n");
        Ok(format!("{} {{\n{}\n}}", header, body))
    }
}

#[derive(Debug, Clone)]
pub struct FunctionHeader {
    return_type: CType,
    name: String,
    parameters: Parameters,
}

impl FunctionHeader {
    pub fn new(return_type: CType, name: String, parameters: Parameters) -> Self {
        Self {
            return_type,
            name,
            parameters,
        }
    }
}

impl Generator for FunctionHeader {
    fn generate(self) -> GeneratorResult {
        let parameters = self
            .parameters
            .into_iter()
            .map(|parameter| parameter.generate())
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", ");
        Ok(format!(
            "{} {}({})",
            self.return_type.generate()?,
            self.name,
            parameters
        ))
    }
}

pub type Parameters = Vec<FunctionParameter>;

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    parameter_type: CType,
    name: String,
}

impl FunctionParameter {
    pub fn new(parameter_type: CType, name: String) -> Self {
        Self {
            parameter_type,
            name,
        }
    }
}

impl Generator for FunctionParameter {
    fn generate(self) -> GeneratorResult {
        Ok(format!("{} {}", self.parameter_type.generate()?, self.name))
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::expression::CExpression;
    use crate::syntax::instruction::Instruction;
    use crate::syntax::TestResult;

    use super::*;
    use crate::syntax::c_type::CPrimitiveType;

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
            .generate()?
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
            .generate()?
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
            .generate()?
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
            .generate()?
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
            .generate()?
        );
        Ok(())
    }
}
