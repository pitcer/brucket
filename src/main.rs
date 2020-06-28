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

use std::io::{Read, Stdin};
use std::io;
use std::io::stdin;

mod lexer;
mod parser;
mod evaluator;

fn main() {
    let mut input = stdin();
    let input_data = read(&mut input).expect("Cannot read syntax from stdin");
    let tokens = lexer::tokenize(&input_data).expect("Cannot lex syntax");
    println!("Tokens: {:?}", tokens);
    let expression = parser::parse(&tokens).expect("Cannot parse tokens");
    println!("Expression: {:?}", expression);
    let result = evaluator::evaluate(&expression).expect("Cannot evaluate expression");
    println!("Result: {:?}", result);
}

fn read(input: &mut Stdin) -> io::Result<String> {
    let mut result = String::new();
    input.read_to_string(&mut result)?;
    Ok(result)
}
