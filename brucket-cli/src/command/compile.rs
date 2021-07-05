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
use crate::command::{CommandResult, Execute};
use brucket_transpiler::transpiler::Transpiler;
use clap::AppSettings;
use clap::Clap;
use std::borrow::Cow;
use std::fs::File;
use std::io::Write;
use std::process::{Command, Stdio};

#[derive(Clap)]
#[clap(
    about = "Compiles Brucket code",
    aliases = &["transpile"],
    setting = AppSettings::ArgRequiredElseHelp
)]
pub struct Compile {
    #[clap(
        short = 'i',
        long = "stdin",
        about = "Get expression from standard input"
    )]
    input_from_stdin: bool,
    #[clap(short, long, about = "Expression to compile")]
    expression: Option<String>,
    #[clap(short, long, about = "File to compile")]
    file: Option<String>,
    #[clap(short, long, about = "Output binary file name")]
    output: Option<String>,
    #[clap(short = 'S', about = "Save intermediate C code")]
    save_intermediate_code: bool,
    #[clap(short, long, about = "C compiler executable")]
    compiler: Option<String>,
    #[clap(short = 'F', long = "flags", about = "C compiler flags")]
    compiler_flags: Option<Vec<String>>,
}

impl Execute for Compile {
    fn execute(self) -> CommandResult {
        let syntax = if self.input_from_stdin {
            command::read_syntax_from_stdin()?.0
        } else {
            self.file
                .map(command::read_syntax_from_file)
                .transpose()?
                .or(self.expression)
                .ok_or_else(|| Cow::from("No expression is provided"))?
        };

        let transpiler = Transpiler::default();
        let transpiled = transpiler
            .transpile(&syntax)
            .map_err(|error| format!("Cannot transpile the given syntax: {}", error))?;

        let bytes = transpiled.as_bytes();
        if self.save_intermediate_code {
            let mut file = File::create("output.c").map_err(|error| error.to_string())?;
            file.write_all(bytes).map_err(|error| error.to_string())?;
        }

        let compiler = self.compiler.unwrap_or_else(|| "gcc".to_owned());
        let flags = self.compiler_flags.unwrap_or_default().into_iter();
        let output = self.output.unwrap_or_else(|| "output".to_owned());
        let mut compiler_process = Command::new(compiler)
            .stdin(Stdio::piped())
            .arg("-x")
            .arg("c")
            .arg("-")
            .arg("-o")
            .arg(output)
            .args(flags)
            .spawn()
            .map_err(|error| error.to_string())?;

        let mut compiler_stdin = compiler_process
            .stdin
            .take()
            .ok_or_else(|| Cow::from("Cannot take stdin from compiler process"))?;
        compiler_stdin
            .write_all(bytes)
            .map_err(|error| error.to_string())?;
        Ok(())
    }
}
