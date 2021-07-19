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

fn read_syntax_from_file(name: &str) -> Result<String, CommandError> {
    let mut file = File::open(name)
        .map_err(|error| Cow::from(format!("Cannot open file {}: {}", name, error)))?;
    read(&mut file)
        .map(|result| result.0)
        .map_err(|error| Cow::from(format!("Cannot read file {}: {}", name, error)))
}

fn read_syntax_from_files(names: &[String]) -> Result<Vec<String>, CommandError> {
    names
        .iter()
        .map(|name| read_syntax_from_file(&name))
        .collect::<Result<Vec<String>, CommandError>>()
}

fn read(input: &mut impl Read) -> io::Result<(String, usize)> {
    let mut result = String::new();
    let bytes_read = input.read_to_string(&mut result)?;
    Ok((result, bytes_read))
}
