use crate::evaluator::Evaluator;
use crate::evaluator::EvaluatorState;
use crate::value::Value;
use brucket_analyzer::variables_analyzer::VariablesAnalyzer;
use brucket_ast::Node;
use brucket_parser::lexer::Lexer;
use brucket_parser::parser::Parser;
use derive_more::Constructor;
use std::borrow::Cow;

#[cfg(test)]
mod tests;

type ValueError = Cow<'static, str>;
type ValueResult = Result<Value, ValueError>;

#[derive(Default, Constructor)]
pub struct Interpreter {
    lexer: Lexer,
    parser: Parser,
    variables_analyzer: VariablesAnalyzer,
    evaluator: Evaluator,
}

impl Interpreter {
    #[inline]
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

    #[inline]
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
            .map_err(|error| {
                format!(
                    "Cannot read library directory in a path '{}': {}",
                    path, error
                )
            })?
            .map(|file| file.map(|file| file.path()))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| format!("{}", error))?;
        paths.sort_by(|first, second| first.cmp(second).reverse());
        let modules_vec = paths
            .into_iter()
            .map(std::fs::read_to_string)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| format!("Cannot read library file: {}", error))?;
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
