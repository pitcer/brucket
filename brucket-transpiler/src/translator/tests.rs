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

use super::*;
use brucket_analyzer::type_analyzer::TypeAnalyzer;
use brucket_ast::NodeId;
use c_generator::syntax::function::FunctionDefinition;
use c_generator::syntax::module::ModuleMember;

type TestResult = Result<(), TranslatorError>;

#[test]
fn translate_if() -> TestResult {
    let if_node = If::new(
        NodeId(0),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(1),
            ConstantVariant::Boolean(Boolean::True),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(2),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(3),
            ConstantVariant::Numeric(Number::Integer("24".to_owned())),
        ))),
    );

    let mut type_analyzer = TypeAnalyzer::default();
    let (_, types) = type_analyzer.analyze_types(&Node::If(if_node.clone()))?;
    let variables = Variables::default();
    let state = TranslationState::default();
    let mut environment = Environment::new(types, variables, state);
    let translator = Translator::default();

    let actual = translator.translate_if(if_node, &mut environment)?;
    let expected = Translation::new(
        Instructions::default(),
        CExpression::FunctionCall(FunctionCallExpression::new(
            FunctionIdentifier::NamedReference("__$if_0".to_owned()),
            Arguments::default(),
        )),
        CType::Primitive(CPrimitiveType::Int),
    );
    assert_eq!(expected, actual);

    let actual = &environment.state.members[0];
    let expected = ModuleMember::FunctionDefinition(FunctionDefinition::new(
        FunctionHeader::new(
            CType::Primitive(CPrimitiveType::Int),
            "__$if_0".to_owned(),
            Parameters::default(),
        ),
        vec![Instruction::IfElse(IfElseInstruction::new(
            CExpression::NamedReference("TRUE".to_owned()),
            vec![Instruction::Return(CExpression::Number(
                NumberExpression::Integer("42".to_owned()),
            ))],
            vec![Instruction::Return(CExpression::Number(
                NumberExpression::Integer("24".to_owned()),
            ))],
        ))],
    ));
    assert_eq!(expected, *actual);

    Ok(())
}
