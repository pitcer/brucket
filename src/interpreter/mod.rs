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

use std::collections::HashMap;

use crate::evaluator::environment::Environment;
use crate::evaluator::Evaluator;
pub use crate::evaluator::Value;
use crate::lexer::Lexer;
use crate::parser::{Expression, Parser};

pub type ModuleEnvironment = HashMap<String, Environment>;

type ValueResult = Result<Value, String>;

pub struct Interpreter {
    lexer: Lexer,
    parser: Parser,
    evaluator: Evaluator,
}

impl Default for Interpreter {
    fn default() -> Self {
        let lexer = Lexer::default();
        let parser = Parser::default();
        let evaluator = Evaluator::default();
        Interpreter::new(lexer, parser, evaluator)
    }
}

impl Interpreter {
    fn new(lexer: Lexer, parser: Parser, evaluator: Evaluator) -> Self {
        Self {
            lexer,
            parser,
            evaluator,
        }
    }

    pub fn interpret_with_modules(
        &self,
        endpoint_syntax: &str,
        modules_syntax: Vec<String>,
    ) -> ValueResult {
        let mut module_environment = ModuleEnvironment::new();
        for module_syntax in modules_syntax {
            let result =
                self.interpret_with_module_environment(&module_syntax, &module_environment)?;
            if let Value::Module(name, environment) = result {
                module_environment.insert(name, environment);
            } else {
                return Err("One of the given modules did not evaluate to module".to_string());
            }
        }
        self.interpret_with_module_environment(endpoint_syntax, &module_environment)
    }

    fn interpret_with_module_environment(
        &self,
        syntax: &str,
        module_environment: &ModuleEnvironment,
    ) -> ValueResult {
        let expression = self.parse_syntax(syntax)?;
        self.evaluator
            .evaluate_with_module_environment(&expression, module_environment)
    }

    pub fn interpret(&self, syntax: &str) -> ValueResult {
        let module_environment = ModuleEnvironment::new();
        self.interpret_with_module_environment(syntax, &module_environment)
    }

    fn parse_syntax(&self, syntax: &str) -> Result<Expression, String> {
        let tokens = self.lexer.tokenize(syntax)?;
        self.parser.parse(&tokens)
    }

    #[cfg(test)]
    pub fn interpret_with_base_library(&self, syntax: &str) -> ValueResult {
        self.interpret_with_base_library_and_modules(syntax, Vec::default())
    }

    #[cfg(test)]
    pub fn interpret_with_base_library_and_modules(
        &self,
        syntax: &str,
        modules: Vec<&str>,
    ) -> ValueResult {
        let mut library_file =
            std::fs::File::open("lib/base.bk").expect("Cannot open library file");
        let mut library_syntax = String::new();
        use std::io::Read;
        library_file
            .read_to_string(&mut library_syntax)
            .expect("Cannot read library file");
        let mut modules_vec = Vec::new();
        modules_vec.push(library_syntax);
        for module in modules {
            modules_vec.push(module.to_string())
        }
        self.interpret_with_modules(syntax, modules_vec)
    }
}

#[cfg(test)]
mod test;
