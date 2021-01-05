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

#![forbid(unsafe_code)]

use std::fs::File;
use std::io::Read;
use std::{env, io};

use brucket_interpreter::interpreter::Interpreter;

fn main() {
    let mut stdin = io::stdin();
    let input_syntax = read(&mut stdin).expect("Cannot read syntax from stdin");
    let mut args = env::args();
    args.next();
    let modules = args
        .map(|argument| {
            let mut file =
                File::open(&argument).unwrap_or_else(|_| panic!("Cannot open file {}", argument));
            read(&mut file)
                .unwrap_or_else(|_| panic!("Cannot read file {}", argument))
                .into()
        })
        .collect();
    let interpreter = Interpreter::default();
    let result = interpreter
        .interpret_with_modules(input_syntax.into(), modules)
        .expect("Cannot interpret input program");
    println!("Result: {:?}", result);
}

fn read(input: &mut impl Read) -> io::Result<String> {
    let mut result = String::new();
    input.read_to_string(&mut result)?;
    Ok(result)
}