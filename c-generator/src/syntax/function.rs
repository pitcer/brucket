use crate::generator::{
    Generator, GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator,
};
use crate::syntax::c_type::CType;
use crate::syntax::instruction::Instructions;
use derive_more::Constructor;

#[derive(Debug, PartialEq, Constructor)]
pub struct FunctionDeclaration {
    header: FunctionHeader,
}

impl IndentedGenerator for FunctionDeclaration {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!("{}{};", state.indentation, self.header.generate()?))
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct FunctionDefinition {
    header: FunctionHeader,
    body: Instructions,
}

impl IndentedGenerator for FunctionDefinition {
    #[inline]
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

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct FunctionHeader {
    return_type: CType,
    name: String,
    parameters: Parameters,
}

impl Generator for FunctionHeader {
    #[inline]
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
    #[inline]
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", "))
    }
}

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct FunctionParameter {
    pub parameter_type: CType,
    pub name: String,
}

impl Generator for FunctionParameter {
    #[inline]
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
            FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "foobar".to_owned())
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
                "foobar".to_owned(),
                vec![
                    FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "foo".to_owned()),
                    FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "bar".to_owned())
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
                "foobar".to_owned(),
                vec![
                    FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "foo".to_owned()),
                    FunctionParameter::new(CType::Primitive(CPrimitiveType::Int), "bar".to_owned())
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
                    "foobar".to_owned(),
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
                    "foobar".to_owned(),
                    Parameters::default()
                ),
                vec![Instruction::Return(CExpression::NamedReference(
                    "bar".to_owned()
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
                    "foobar".to_owned(),
                    vec![FunctionParameter::new(
                        CType::Primitive(CPrimitiveType::Int),
                        "foo".to_owned()
                    )]
                ),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Return(CExpression::NamedReference("bar".to_owned()))
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
                    "foobar".to_owned(),
                    vec![
                        FunctionParameter::new(
                            CType::Primitive(CPrimitiveType::Int),
                            "foo".to_owned()
                        ),
                        FunctionParameter::new(
                            CType::Primitive(CPrimitiveType::Int),
                            "bar".to_owned()
                        )
                    ]
                ),
                vec![
                    Instruction::Expression(CExpression::NamedReference("foo".to_owned())),
                    Instruction::Return(CExpression::NamedReference("bar".to_owned()))
                ]
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
