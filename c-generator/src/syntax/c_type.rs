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
use derive_more::Constructor;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CType {
    Primitive(CPrimitiveType),
    Custom(String),
}

impl Generator for CType {
    fn generate(self) -> GeneratorResult {
        match self {
            CType::Primitive(primitive) => primitive.generate(),
            CType::Custom(custom) => Ok(custom),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Constructor)]
pub struct FunctionPointer {
    pub return_type: CType,
    pub name: String,
    pub parameters_types: Types,
}

impl Generator for FunctionPointer {
    fn generate(self) -> GeneratorResult {
        Ok(format!(
            "{} (*{})({})",
            self.return_type.generate()?,
            self.name,
            self.parameters_types.generate()?
        ))
    }
}

pub type Types = Vec<CType>;

impl Generator for Types {
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", "))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CPrimitiveType {
    Int,
    Long,
    Float,
    Double,
    Char,
    Void,
}

impl Generator for CPrimitiveType {
    fn generate(self) -> GeneratorResult {
        Ok(match self {
            CPrimitiveType::Int => "int",
            CPrimitiveType::Long => "long int",
            CPrimitiveType::Float => "float",
            CPrimitiveType::Double => "double",
            CPrimitiveType::Char => "char",
            CPrimitiveType::Void => "void",
        }
        .to_string())
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_primitive_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", CPrimitiveType::Int.generate()?);
        assert_eq!("long int", CPrimitiveType::Long.generate()?);
        assert_eq!("float", CPrimitiveType::Float.generate()?);
        assert_eq!("double", CPrimitiveType::Double.generate()?);
        assert_eq!("char", CPrimitiveType::Char.generate()?);
        assert_eq!("void", CPrimitiveType::Void.generate()?);
        Ok(())
    }

    #[test]
    fn test_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", CType::Primitive(CPrimitiveType::Int).generate()?);
        assert_eq!("FooBar", CType::Custom("FooBar".to_string()).generate()?);
        Ok(())
    }
}
