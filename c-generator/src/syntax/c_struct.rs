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
use crate::syntax::instruction::VariableDeclaration;

#[derive(Debug)]
pub struct CStruct {
    name: String,
    fields: Fields,
}

impl CStruct {
    pub fn new(name: String, fields: Fields) -> Self {
        CStruct { name, fields }
    }
}

impl Generator for CStruct {
    fn generate(self) -> GeneratorResult {
        Ok(format!(
            "struct {} {{\n{}\n}};",
            self.name,
            self.fields.generate()?
        ))
    }
}

pub type Fields = Vec<VariableDeclaration>;

impl Generator for Fields {
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n"))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax::c_type::{CPrimitiveType, CType};
    use crate::syntax::modifiers::Modifier;
    use crate::syntax::TestResult;

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
            .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_generate_struct() -> TestResult {
        assert_eq!(
            "struct foobar {\nconst int a;\nint b;\nint c;\n};",
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
            .generate()?
        );
        Ok(())
    }
}
