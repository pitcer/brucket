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

use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::io::Read;

pub mod brucket;
pub mod compile;
pub mod interpret;

pub type CommandResult = Result<(), CommandError>;
type CommandError = Cow<'static, str>;

pub trait Execute {
    fn execute(self) -> CommandResult;
}

fn read_syntax_from_stdin() -> Result<(String, usize), CommandError> {
    let mut standard_input = io::stdin();
    read(&mut standard_input)
        .map_err(|error| Cow::from(format!("Cannot read syntax from standard input: {}", error)))
}

fn read_syntax_from_file(name: String) -> Result<String, CommandError> {
    let mut file = File::open(&name)
        .map_err(|error| Cow::from(format!("Cannot open file {}: {}", name, error)))?;
    read(&mut file)
        .map(|result| result.0)
        .map_err(|error| Cow::from(format!("Cannot read file {}: {}", name, error)))
}

fn read_syntax_from_files(names: Vec<String>) -> Result<Vec<String>, CommandError> {
    names
        .into_iter()
        .map(read_syntax_from_file)
        .collect::<Result<Vec<String>, CommandError>>()
}

fn read(input: &mut impl Read) -> io::Result<(String, usize)> {
    let mut result = String::new();
    let bytes_read = input.read_to_string(&mut result)?;
    Ok((result, bytes_read))
}
