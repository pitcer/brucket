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

use std::borrow::Cow;
use std::fmt::{Display, Formatter};

pub type GeneratorResult = Result<String, GeneratorError>;
pub type GeneratorError = Cow<'static, str>;

pub trait Generator {
    fn generate(self) -> GeneratorResult;
}

pub trait IndentedGenerator {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult;
}

pub struct GeneratorState {
    pub indentation: Indentation,
}

impl Default for GeneratorState {
    fn default() -> Self {
        Self::new(Indentation::default())
    }
}

impl GeneratorState {
    pub fn new(indentation: Indentation) -> Self {
        GeneratorState { indentation }
    }
}

pub struct Indentation {
    character: Cow<'static, str>,
    level: usize,
}

impl Default for Indentation {
    fn default() -> Self {
        Self::new("    ".into(), 0)
    }
}

impl Indentation {
    pub fn new(character: Cow<'static, str>, level: usize) -> Self {
        Indentation { character, level }
    }

    pub fn to_incremented(&self) -> Indentation {
        let character = self.character.clone();
        Indentation::new(character, self.level + 1)
    }
}

impl Display for Indentation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indentation = self.character.repeat(self.level);
        write!(f, "{}", indentation)
    }
}
