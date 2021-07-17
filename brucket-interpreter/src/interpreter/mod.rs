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

use crate::evaluator::Evaluator;
use crate::evaluator::EvaluatorState;
use crate::value::Value;
use brucket_analyzer::variables_analyzer::VariablesAnalyzer;
use brucket_ast::ast::Node;
use brucket_parser::lexer::Lexer;
use brucket_parser::parser::Parser;
use derive_more::Constructor;
use std::borrow::Cow;

#[cfg(test)]
mod test;

type ValueResult = Result<Value, Cow<'static, str>>;

#[derive(Default, Constructor)]
pub struct Interpreter {
    lexer: Lexer,
    parser: Parser,
    variables_analyzer: VariablesAnalyzer,
    evaluator: Evaluator,
}

impl Interpreter {
    pub fn interpret_with_modules(
        &mut self,
        endpoint_syntax: &str,
        modules_syntax: Vec<&str>,
    ) -> ValueResult {
        for module_syntax in modules_syntax {
            let result = self.interpret_with_module_environment(module_syntax)?;
            if let Value::Module(is_static, name, environment) = result {
                if is_static {
                    self.evaluator
                        .static_module_environment
                        .insert_all(&environment);
                    self.evaluator
                        .static_module_environment
                        .insert_all_weak(&environment);
                } else {
                    self.evaluator.module_environment.insert(name, environment);
                }
            } else {
                return Err(Cow::from(
                    "One of the given modules did not evaluate to module",
                ));
            }
        }
        self.interpret_with_module_environment(endpoint_syntax)
    }

    fn interpret_with_module_environment(&mut self, syntax: &str) -> ValueResult {
        let node = self.parse_syntax(syntax)?;
        let (_, variables) = self.variables_analyzer.analyze_variables(&node)?;
        let state = EvaluatorState::new(&variables);
        self.evaluator.evaluate(&node, &state)
    }

    pub fn interpret(&mut self, syntax: &str) -> ValueResult {
        self.interpret_with_module_environment(syntax)
    }

    fn parse_syntax(&mut self, syntax: &str) -> Result<Node, Cow<'static, str>> {
        let tokens = self.lexer.tokenize(syntax)?;
        self.parser.parse(tokens)
    }

    #[cfg(test)]
    pub fn interpret_with_base_library(&mut self, syntax: &str) -> ValueResult {
        self.interpret_with_base_library_and_modules(syntax, Vec::default())
    }

    #[cfg(test)]
    pub fn interpret_with_base_library_and_modules(
        &mut self,
        syntax: &str,
        modules: Vec<&str>,
    ) -> ValueResult {
        let path = "../lib/";
        let mut paths = std::fs::read_dir(path)
            .unwrap_or_else(|_| panic!("Cannot read library directory in a path '{}'", path))
            .map(|file| file.unwrap().path())
            .collect::<Vec<_>>();
        paths.sort_by(|first, second| first.cmp(second).reverse());
        use std::io::Read;
        let mut modules_vec = Vec::new();
        for path in paths {
            let mut library_file = std::fs::File::open(path).expect("Cannot open library file");
            let mut library_syntax = String::new();
            library_file
                .read_to_string(&mut library_syntax)
                .expect("Cannot read library file");
            modules_vec.push(library_syntax);
        }
        let mut modules_vec = modules_vec
            .iter()
            .map(String::as_str)
            .collect::<Vec<&str>>();
        for module in modules {
            modules_vec.push(module);
        }
        self.interpret_with_modules(syntax, modules_vec)
    }
}
