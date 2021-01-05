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

#[derive(Debug, Clone)]
pub enum CExpression {
    Number(NumberExpression),
    String(String),
    NamedReference(String),
    FunctionCall(FunctionCallExpression),
}

impl Generator for CExpression {
    fn generate(self) -> GeneratorResult {
        match self {
            CExpression::Number(number) => number.generate(),
            CExpression::String(string) => Ok(format!("\"{}\"", string)),
            CExpression::NamedReference(name) => Ok(name),
            CExpression::FunctionCall(call) => call.generate(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NumberExpression {
    Integer(String),
    FloatingPoint(String),
}

impl Generator for NumberExpression {
    fn generate(self) -> GeneratorResult {
        match self {
            NumberExpression::Integer(integer) => Ok(integer),
            NumberExpression::FloatingPoint(float) => Ok(float),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    name: String,
    arguments: Arguments,
}

impl FunctionCallExpression {
    pub fn new(name: String, arguments: Arguments) -> Self {
        Self { name, arguments }
    }
}

impl Generator for FunctionCallExpression {
    fn generate(self) -> GeneratorResult {
        let arguments = self
            .arguments
            .into_iter()
            .map(|argument| argument.generate())
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", ");
        Ok(format!("{}({})", self.name, arguments))
    }
}

pub type Arguments = Vec<CExpression>;

#[cfg(test)]
mod test {
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_number_expressions_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "42",
            NumberExpression::Integer("42".to_string()).generate()?
        );
        assert_eq!(
            "42.42",
            NumberExpression::FloatingPoint("42.42".to_string()).generate()?
        );
        Ok(())
    }

    #[test]
    fn test_function_call_expressions_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "foobar()",
            FunctionCallExpression::new("foobar".to_string(), Arguments::default()).generate()?
        );
        assert_eq!(
            "foobar(foo)",
            FunctionCallExpression::new(
                "foobar".to_string(),
                vec![CExpression::NamedReference("foo".to_string())]
            )
            .generate()?
        );
        assert_eq!(
            "foobar(foo, bar)",
            FunctionCallExpression::new(
                "foobar".to_string(),
                vec![
                    CExpression::NamedReference("foo".to_string()),
                    CExpression::NamedReference("bar".to_string())
                ]
            )
            .generate()?
        );
        assert_eq!(
            "foobar(foo, bar, foobar)",
            FunctionCallExpression::new(
                "foobar".to_string(),
                vec![
                    CExpression::NamedReference("foo".to_string()),
                    CExpression::NamedReference("bar".to_string()),
                    CExpression::NamedReference("foobar".to_string())
                ]
            )
            .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_expressions_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "42",
            CExpression::Number(NumberExpression::Integer("42".to_string())).generate()?
        );
        assert_eq!(
            "42.42",
            CExpression::Number(NumberExpression::FloatingPoint("42.42".to_string())).generate()?
        );
        assert_eq!(
            "foobar",
            CExpression::NamedReference("foobar".to_string()).generate()?
        );
        assert_eq!("\"\"", CExpression::String("".to_string()).generate()?);
        assert_eq!(
            "\"foobar\"",
            CExpression::String("foobar".to_string()).generate()?
        );
        assert_eq!(
            "foobar(foo, bar, foobar)",
            CExpression::FunctionCall(FunctionCallExpression::new(
                "foobar".to_string(),
                vec![
                    CExpression::NamedReference("foo".to_string()),
                    CExpression::NamedReference("bar".to_string()),
                    CExpression::NamedReference("foobar".to_string())
                ]
            ))
            .generate()?
        );
        Ok(())
    }
}