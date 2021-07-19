use crate::command;
use crate::command::{CommandResult, Execute};
use brucket_interpreter::interpreter::Interpreter;
use clap::{AppSettings, Clap};
use std::borrow::Cow;

#[derive(Clap)]
#[clap(
    about = "Interprets Brucket expressions",
    aliases = &["evaluate"],
    setting = AppSettings::ArgRequiredElseHelp
)]
pub struct Interpret {
    #[clap(
        short = 'i',
        long = "stdin",
        about = "Get expression from standard input"
    )]
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
        let syntax = if self.input_from_stdin {
            command::read_syntax_from_stdin()?.0
        } else {
            self.file
                .map(|file| command::read_syntax_from_file(&file))
                .transpose()?
                .or(self.expression)
                .ok_or_else(|| Cow::from("Expression is not provided"))?
        };

        let modules = self
            .modules
            .map(|module| command::read_syntax_from_files(&module))
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
