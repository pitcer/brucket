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

use crate::generator::{Generator, GeneratorResult};

pub mod c_macro;
pub mod expression;
pub mod function;
pub mod instruction;
pub mod module;

#[cfg(test)]
pub type TestResult = Result<(), String>;

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(PrimitiveType),
    Custom(String),
}

impl Generator for Type {
    fn generate(self) -> GeneratorResult {
        match self {
            Type::Primitive(primitive) => primitive.generate(),
            Type::Custom(custom) => Ok(custom),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PrimitiveType {
    Int,
    Long,
    Float,
    Double,
    Char,
    Void,
}

impl Generator for PrimitiveType {
    fn generate(self) -> GeneratorResult {
        Ok(match self {
            PrimitiveType::Int => "int",
            PrimitiveType::Long => "long int",
            PrimitiveType::Float => "float",
            PrimitiveType::Double => "double",
            PrimitiveType::Char => "char",
            PrimitiveType::Void => "void",
        }
        .to_string())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_primitive_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", PrimitiveType::Int.generate()?);
        assert_eq!("long int", PrimitiveType::Long.generate()?);
        assert_eq!("float", PrimitiveType::Float.generate()?);
        assert_eq!("double", PrimitiveType::Double.generate()?);
        assert_eq!("char", PrimitiveType::Char.generate()?);
        assert_eq!("void", PrimitiveType::Void.generate()?);
        Ok(())
    }

    #[test]
    fn test_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", Type::Primitive(PrimitiveType::Int).generate()?);
        assert_eq!("FooBar", Type::Custom("FooBar".to_string()).generate()?);
        Ok(())
    }
}
