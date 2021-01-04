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

pub enum Macro {
    Include(String),
    Define(DefineMacro),
}

impl Generator for Macro {
    fn generate(self) -> GeneratorResult {
        match self {
            Macro::Include(module) => Ok(format!("#include <{}>", module)),
            Macro::Define(define_macro) => define_macro.generate(),
        }
    }
}

pub struct DefineMacro {
    name: String,
    value: String,
}

impl DefineMacro {
    pub fn new(name: String, value: String) -> Self {
        Self { name, value }
    }
}

impl Generator for DefineMacro {
    fn generate(self) -> GeneratorResult {
        Ok(format!("#define {} {}", self.name, self.value))
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_define_macros_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "#define FOOBAR foo bar",
            DefineMacro::new("FOOBAR".to_string(), "foo bar".to_string()).generate()?
        );
        Ok(())
    }

    #[test]
    fn test_macros_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "#include <stdio.h>",
            Macro::Include("stdio.h".to_string()).generate()?
        );
        assert_eq!(
            "#define FOOBAR foo bar",
            Macro::Define(DefineMacro::new(
                "FOOBAR".to_string(),
                "foo bar".to_string()
            ))
            .generate()?
        );
        Ok(())
    }
}
