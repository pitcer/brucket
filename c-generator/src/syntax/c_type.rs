use crate::generator::{Generator, GeneratorError, GeneratorResult};
use derive_more::Constructor;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CType {
    Primitive(CPrimitiveType),
    Closure(String, Box<CType>, Vec<String>),
    Struct(String),
    Custom(String),
}

impl Generator for CType {
    #[inline]
    fn generate(self) -> GeneratorResult {
        match self {
            CType::Primitive(primitive) => primitive.generate(),
            CType::Struct(name) | CType::Closure(name, _, _) => Ok(format!("struct {}", name)),
            CType::Custom(name) => Ok(name),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Constructor)]
pub struct FunctionPointer {
    pub return_type: CType,
    pub name: String,
    pub parameters_types: Types,
}

impl Generator for FunctionPointer {
    #[inline]
    fn generate(self) -> GeneratorResult {
        Ok(format!(
            "{} (*{})({})",
            self.return_type.generate()?,
            self.name,
            self.parameters_types.generate()?
        ))
    }
}

pub type Types = Vec<CType>;

impl Generator for Types {
    #[inline]
    fn generate(self) -> GeneratorResult {
        Ok(self
            .into_iter()
            .map(Generator::generate)
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join(", "))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CPrimitiveType {
    Int,
    Long,
    Float,
    Double,
    Char,
    Void,
}

impl Generator for CPrimitiveType {
    #[inline]
    fn generate(self) -> GeneratorResult {
        Ok(match self {
            CPrimitiveType::Int => "int",
            CPrimitiveType::Long => "long int",
            CPrimitiveType::Float => "float",
            CPrimitiveType::Double => "double",
            CPrimitiveType::Char => "char",
            CPrimitiveType::Void => "void",
        }
        .to_owned())
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::TestResult;

    use super::*;

    #[test]
    fn test_primitive_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", CPrimitiveType::Int.generate()?);
        assert_eq!("long int", CPrimitiveType::Long.generate()?);
        assert_eq!("float", CPrimitiveType::Float.generate()?);
        assert_eq!("double", CPrimitiveType::Double.generate()?);
        assert_eq!("char", CPrimitiveType::Char.generate()?);
        assert_eq!("void", CPrimitiveType::Void.generate()?);
        Ok(())
    }

    #[test]
    fn test_types_are_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("int", CType::Primitive(CPrimitiveType::Int).generate()?);
        assert_eq!("FooBar", CType::Custom("FooBar".to_owned()).generate()?);
        assert_eq!(
            "struct FooBar",
            CType::Struct("FooBar".to_owned()).generate()?
        );
        Ok(())
    }
}
