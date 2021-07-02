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
use clap::Clap;
use std::borrow::Cow;
use std::fs::File;
use std::io;

#[derive(Clap)]
#[clap(
    about = "Interprets Brucket expressions",
    aliases = &["evaluate", "eval"]
)]
pub struct Interpret {
    #[clap(short, long)]
    modules: Option<Vec<String>>,
}

impl Execute for Interpret {
    fn execute(self) -> CommandResult {
        let mut standard_input = io::stdin();
        let input_syntax = command::read(&mut standard_input)
            .map_err(|error| format!("Cannot read syntax from standard input: {}", error))?;
        let modules = self
            .modules
            .unwrap_or_default()
            .into_iter()
            .map(|module| {
                File::open(&module)
                    .map_err(|error| Cow::from(format!("Cannot open file {}: {}", module, error)))
                    .and_then(|mut file| {
                        command::read(&mut file).map_err(|error| {
                            Cow::from(format!("Cannot read file {}: {}", module, error))
                        })
                    })
                    .map(Cow::from)
            })
            .collect::<Result<Vec<_>, CommandError>>()?;
        let mut interpreter = Interpreter::default();
        let result = interpreter
            .interpret_with_modules(input_syntax.into(), modules)
            .map_err(|error| format!("Cannot interpret input program: {}", error))?;
        println!("{}", result);
        Ok(())
    }
}
