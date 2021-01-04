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

use crate::generator::{Generator, GeneratorError, GeneratorResult};
use crate::syntax::c_macro::Macro;
use crate::syntax::function::Function;
use crate::syntax::instruction::VariableInstruction;

pub struct Module {
    members: ModuleMembers,
}

impl Module {
    pub fn new(members: ModuleMembers) -> Self {
        Self { members }
    }
}

impl Generator for Module {
    fn generate(self) -> GeneratorResult {
        Ok(self
            .members
            .into_iter()
            .map(|member| member.generate())
            .collect::<Result<Vec<String>, GeneratorError>>()?
            .join("\n"))
    }
}

pub type ModuleMembers = Vec<ModuleMember>;

pub enum ModuleMember {
    Macro(Macro),
    Function(Function),
    StaticVariable(VariableInstruction),
}

impl Generator for ModuleMember {
    fn generate(self) -> GeneratorResult {
        match self {
            ModuleMember::Macro(c_macro) => c_macro.generate(),
            ModuleMember::Function(function) => function.generate(),
            ModuleMember::StaticVariable(variable) => {
                Ok(format!("static {}", variable.generate()?))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::expression::Expression;
    use crate::syntax::function::Parameters;
    use crate::syntax::instruction::Instructions;
    use crate::syntax::{PrimitiveType, TestResult, Type};

    use super::*;

    #[test]
    fn test_module_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!("", Module::new(ModuleMembers::default()).generate()?);
        assert_eq!(
            r#"#include <test.h>
static int FOOBAR = test;
int foobar() {

}"#,
            Module::new(vec![
                ModuleMember::Macro(Macro::Include("test.h".to_string())),
                ModuleMember::StaticVariable(VariableInstruction::new(
                    Type::Primitive(PrimitiveType::Int),
                    "FOOBAR".to_string(),
                    Some(Expression::NamedReference("test".to_string()))
                )),
                ModuleMember::Function(Function::new(
                    Type::Primitive(PrimitiveType::Int),
                    "foobar".to_string(),
                    Parameters::default(),
                    Instructions::default()
                ))
            ])
            .generate()?
        );
        Ok(())
    }

    #[test]
    fn test_module_member_is_converted_to_c_syntax_correctly() -> TestResult {
        assert_eq!(
            "#include <test.h>",
            ModuleMember::Macro(Macro::Include("test.h".to_string())).generate()?
        );
        assert_eq!(
            r#"int foobar() {

}"#,
            ModuleMember::Function(Function::new(
                Type::Primitive(PrimitiveType::Int),
                "foobar".to_string(),
                Parameters::default(),
                Instructions::default()
            ))
            .generate()?
        );
        assert_eq!(
            "static int FOOBAR = test;",
            ModuleMember::StaticVariable(VariableInstruction::new(
                Type::Primitive(PrimitiveType::Int),
                "FOOBAR".to_string(),
                Some(Expression::NamedReference("test".to_string()))
            ))
            .generate()?
        );
        Ok(())
    }
}
