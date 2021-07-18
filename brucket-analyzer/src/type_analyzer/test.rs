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

use brucket_quote::brucket;

use super::*;

type TestResult = Result<(), TestError>;
type TestError = Cow<'static, str>;

#[test]
fn test_let_types_are_evaluated_correctly() -> TestResult {
    let node = brucket! {
        (0: let foo: any (1: 1)
            (2: let bar: any (3: 1.1)
                (4: foo)))
    };
    let expected = NodeTypes(maplit::hashmap! {
        NodeId(0) => Type::Integer,
        NodeId(1) => Type::Integer,
        NodeId(2) => Type::Integer,
        NodeId(3) => Type::Float,
        NodeId(4) => Type::Integer,
    });
    let mut type_analyzer = TypeAnalyzer::default();
    let (node_type, node_types) = type_analyzer.analyze_types(&node)?;
    assert_eq!(Type::Integer, node_type);
    assert_eq!(expected, node_types);
    Ok(())
}
