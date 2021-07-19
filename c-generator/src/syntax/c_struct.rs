use crate::generator::{GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator};
use crate::syntax::instruction::VariableDeclaration;
use derive_more::Constructor;

#[derive(Debug, PartialEq, Constructor)]
pub struct CStruct {
    name: String,
    fields: Fields,
}

impl IndentedGenerator for CStruct {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        let indentation = &state.indentation;
        let incremented_indentation = state.indentation.to_incremented();
        let state = GeneratorState::new(incremented_indentation);
        Ok(format!(
            "{}struct {} {{\n{}\n{}}};",
            indentation,
            self.name,
            self.fields.generate_indented(&state)?,
            indentation
        ))
    }
}

pub type Fields = Vec<VariableDeclaration>;

impl IndentedGenerator for Fields {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(|field| field.generate_indented(state))
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n"))
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::c_type::{CPrimitiveType, CType};
    use crate::syntax::modifiers::Modifier;
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_generate_fields() -> TestResult {
        assert_eq!(
            "const int a;\nint b;\nint c;",
            vec![
                VariableDeclaration::new(
                    vec![Modifier::Const],
                    CType::Primitive(CPrimitiveType::Int),
                    "a".to_owned()
                ),
                VariableDeclaration::new(
                    vec![],
                    CType::Primitive(CPrimitiveType::Int),
                    "b".to_owned()
                ),
                VariableDeclaration::new(
                    vec![],
                    CType::Primitive(CPrimitiveType::Int),
                    "c".to_owned()
                )
            ]
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_generate_struct() -> TestResult {
        assert_eq!(
            "struct foobar {\n    const int a;\n    int b;\n    int c;\n};",
            CStruct::new(
                "foobar".to_owned(),
                vec![
                    VariableDeclaration::new(
                        vec![Modifier::Const],
                        CType::Primitive(CPrimitiveType::Int),
                        "a".to_owned()
                    ),
                    VariableDeclaration::new(
                        vec![],
                        CType::Primitive(CPrimitiveType::Int),
                        "b".to_owned()
                    ),
                    VariableDeclaration::new(
                        vec![],
                        CType::Primitive(CPrimitiveType::Int),
                        "c".to_owned()
                    )
                ]
            )
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
