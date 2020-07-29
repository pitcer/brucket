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

pub use crate::evaluator::Value;

use crate::evaluator::{Environment, Evaluator};
use crate::lexer::Lexer;
use crate::parser::Parser;

type ValueResult = Result<Value, String>;

pub struct Interpreter {
    lexer: Lexer,
    parser: Parser,
    evaluator: Evaluator,
}

impl Interpreter {
    pub fn default() -> Self {
        let lexer = Lexer::default();
        let parser = Parser::default();
        let evaluator = Evaluator::default();
        Interpreter {
            lexer,
            parser,
            evaluator,
        }
    }

    pub fn with_library(library_syntax: &str) -> Result<Self, String> {
        let lexer = Lexer::default();
        let parser = Parser::default();
        let default_environment = Self::interpret_library(library_syntax)?;
        let evaluator = Evaluator::new(default_environment);
        Ok(Interpreter {
            lexer,
            parser,
            evaluator,
        })
    }

    fn interpret_library(library_syntax: &str) -> Result<Environment, String> {
        let interpreter = Self::default();
        let library = interpreter.interpret(library_syntax)?;
        if let Value::Module(_identifier, environment) = library {
            Ok(environment)
        } else {
            Err("Given library syntax is not a module".to_string())
        }
    }

    pub fn interpret(&self, syntax: &str) -> ValueResult {
        let tokenized = self.lexer.tokenize(syntax)?;
        let parsed = self.parser.parse(&tokenized)?;
        self.evaluator.evaluate(&parsed)
    }
}

#[cfg(test)]
mod test;
