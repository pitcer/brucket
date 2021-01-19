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
use std::collections::HashMap;

use brucket_ast::lexer::Lexer;
use brucket_ast::parser::expression::Expression;
use brucket_ast::parser::Parser;

use crate::evaluator::environment::Environment;
use crate::evaluator::Evaluator;
use crate::value::Value;

#[cfg(test)]
mod test;

pub type ModuleEnvironment = HashMap<String, Environment>;

type ValueResult = Result<Value, Cow<'static, str>>;

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
        &mut self,
        endpoint_syntax: Cow<str>,
        modules_syntax: Vec<Cow<str>>,
    ) -> ValueResult {
        let static_module_environment = Environment::new();
        let mut module_environment = ModuleEnvironment::new();
        for module_syntax in modules_syntax {
            let result = self.interpret_with_module_environment(
                module_syntax,
                &static_module_environment,
                &module_environment,
            )?;
            if let Value::Module(is_static, name, environment) = result {
                if is_static {
                    static_module_environment.insert_all(&environment);
                    static_module_environment.insert_all_weak(&environment);
                } else {
                    module_environment.insert(name, environment);
                }
            } else {
                return Err(Cow::from(
                    "One of the given modules did not evaluate to module",
                ));
            }
        }
        self.interpret_with_module_environment(
            endpoint_syntax,
            &static_module_environment,
            &module_environment,
        )
    }

    fn interpret_with_module_environment(
        &mut self,
        syntax: Cow<str>,
        static_module_environment: &Environment,
        module_environment: &ModuleEnvironment,
    ) -> ValueResult {
        let expression = self.parse_syntax(syntax)?;
        self.evaluator.evaluate_with_module_environment(
            &expression,
            static_module_environment,
            module_environment,
        )
    }

    pub fn interpret(&mut self, syntax: Cow<str>) -> ValueResult {
        let module_environment = ModuleEnvironment::new();
        let static_module_environment = Environment::new();
        self.interpret_with_module_environment(
            syntax,
            &static_module_environment,
            &module_environment,
        )
    }

    fn parse_syntax(&self, syntax: Cow<str>) -> Result<Expression, Cow<'static, str>> {
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
        let mut modules_vec: Vec<Cow<str>> = Vec::new();
        for path in paths {
            let mut library_file = std::fs::File::open(path).expect("Cannot open library file");
            let mut library_syntax = String::new();
            library_file
                .read_to_string(&mut library_syntax)
                .expect("Cannot read library file");
            modules_vec.push(library_syntax.into());
        }
        for module in modules {
            modules_vec.push(module.into())
        }
        self.interpret_with_modules(syntax.into(), modules_vec)
    }
}
