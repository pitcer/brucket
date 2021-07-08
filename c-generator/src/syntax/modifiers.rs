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

pub type Modifiers = Vec<Modifier>;

impl Generator for Modifiers {
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(" "))
    }
}

#[derive(Debug, PartialEq)]
pub enum Modifier {
    Const,
    Static,
    Volatile,
}

impl Generator for Modifier {
    fn generate(self) -> GeneratorResult {
        match self {
            Modifier::Const => Ok("const".to_owned()),
            Modifier::Static => Ok("static".to_owned()),
            Modifier::Volatile => Ok("volatile".to_owned()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax::TestResult;

    #[test]
    fn test_generate_modifier() -> TestResult {
        assert_eq!("const".to_owned(), Modifier::Const.generate()?);
        assert_eq!("static".to_owned(), Modifier::Static.generate()?);
        assert_eq!("volatile".to_owned(), Modifier::Volatile.generate()?);
        Ok(())
    }

    #[test]
    fn test_generate_modifiers() -> TestResult {
        assert_eq!("".to_owned(), Modifiers::default().generate()?);
        assert_eq!("const".to_owned(), vec![Modifier::Const].generate()?);
        assert_eq!(
            "const static volatile".to_owned(),
            vec![Modifier::Const, Modifier::Static, Modifier::Volatile].generate()?
        );
        Ok(())
    }
}
