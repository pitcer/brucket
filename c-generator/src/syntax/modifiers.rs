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
