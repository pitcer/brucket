use crate::generator::{GeneratorResult, GeneratorState, IndentedGenerator};
use derive_more::Constructor;

#[derive(Debug, PartialEq)]
pub enum Macro {
    Include(String),
    Define(DefineMacro),
}

impl IndentedGenerator for Macro {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        match self {
            Macro::Include(module) => Ok(format!("{}#include <{}>", state.indentation, module)),
            Macro::Define(define_macro) => define_macro.generate_indented(state),
        }
    }
}

#[derive(Debug, PartialEq, Constructor)]
pub struct DefineMacro {
    name: String,
    value: String,
}

impl IndentedGenerator for DefineMacro {
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!(
            "{}#define {} {}",
            state.indentation, self.name, self.value
        ))
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
            DefineMacro::new("FOOBAR".to_string(), "foo bar".to_string())
                .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }

    #[test]
    fn test_macros_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "#include <stdio.h>",
            Macro::Include("stdio.h".to_string()).generate_indented(&GeneratorState::default())?
        );
        assert_eq!(
            "#define FOOBAR foo bar",
            Macro::Define(DefineMacro::new(
                "FOOBAR".to_string(),
                "foo bar".to_string()
            ))
            .generate_indented(&GeneratorState::default())?
        );
        Ok(())
    }
}
