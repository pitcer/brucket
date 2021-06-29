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

use crate::generator::{GeneratorError, GeneratorResult, GeneratorState, IndentedGenerator};
use crate::syntax::instruction::VariableDeclaration;
use derive_more::Constructor;

#[derive(Debug, Constructor)]
pub struct CStruct {
    name: String,
    fields: Fields,
}

impl IndentedGenerator for CStruct {
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
