use crate::generator::{GeneratorResult, GeneratorState, IndentedGenerator};
use derive_more::Constructor;

#[derive(Debug, PartialEq, Constructor)]
pub struct Typedef {
    value: String,
}

impl IndentedGenerator for Typedef {
    #[inline]
    fn generate_indented(self, state: &GeneratorState) -> GeneratorResult {
        Ok(format!("{}typedef {};", state.indentation, self.value))
    }
}
