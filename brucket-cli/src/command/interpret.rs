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

use crate::command;
use crate::command::{CommandError, CommandResult, Execute};
use brucket_interpreter::interpreter::Interpreter;
use clap::{AppSettings, Clap};
use std::borrow::Cow;
use std::fs::File;
use std::io;

#[derive(Clap)]
#[clap(
    about = "Interprets Brucket expressions",
    aliases = &["evaluate"],
    setting = AppSettings::ArgRequiredElseHelp
)]
pub struct Interpret {
    #[clap(long = "stdin", about = "Get expression from standard input")]
    input_from_stdin: bool,
    #[clap(short, long, about = "Expression to interpret")]
    expression: Option<String>,
    #[clap(short, long, about = "File to interpret")]
    file: Option<String>,
    #[clap(short, long, about = "Files containing library modules")]
    modules: Option<Vec<String>>,
}

impl Execute for Interpret {
    fn execute(self) -> CommandResult {
        fn read_syntax_from_stdin() -> Result<String, CommandError> {
            let mut standard_input = io::stdin();
            command::read(&mut standard_input).map_err(|error| {
                Cow::from(format!("Cannot read syntax from standard input: {}", error))
            })
        }

        fn read_syntax_from_file(name: String) -> Result<String, CommandError> {
            File::open(&name)
                .map_err(|error| Cow::from(format!("Cannot open file {}: {}", name, error)))
                .and_then(|mut file| {
                    command::read(&mut file)
                        .map_err(|error| Cow::from(format!("Cannot read file {}: {}", name, error)))
                })
        }

        fn read_modules_from_files(names: Vec<String>) -> Result<Vec<String>, CommandError> {
            names
                .into_iter()
                .map(read_syntax_from_file)
                .collect::<Result<Vec<String>, CommandError>>()
        }

        let syntax = if self.input_from_stdin {
            read_syntax_from_stdin()?
        } else {
            self.file
                .map(read_syntax_from_file)
                .transpose()?
                .or(self.expression)
                .ok_or_else(|| Cow::from("Expression is not provided"))?
        };

        let modules = self
            .modules
            .map(read_modules_from_files)
            .transpose()?
            .unwrap_or_default();
        let modules = modules.iter().map(String::as_str).collect();

        let mut interpreter = Interpreter::default();
        let result = interpreter
            .interpret_with_modules(&syntax, modules)
            .map_err(|error| format!("Cannot interpret input program: {}", error))?;
        println!("{}", result);
        Ok(())
    }
}
