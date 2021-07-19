use derive_more::Constructor;
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

#[derive(Default, Constructor)]
pub struct GeneratorState {
    pub indentation: Indentation,
}

#[derive(Constructor)]
pub struct Indentation {
    character: Cow<'static, str>,
    level: usize,
}

impl Default for Indentation {
    #[inline]
    fn default() -> Self {
        Self::new("    ".into(), 0)
    }
}

impl Indentation {
    #[inline]
    #[must_use]
    pub fn to_incremented(&self) -> Indentation {
        let character = self.character.clone();
        Indentation::new(character, self.level + 1)
    }
}

impl Display for Indentation {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indentation = self.character.repeat(self.level);
        write!(f, "{}", indentation)
    }
}
