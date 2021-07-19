use crate::command::compile::Compile;
use crate::command::interpret::Interpret;
use crate::command::{CommandResult, Execute};
use clap::AppSettings;
use clap::Clap;

#[derive(Clap)]
#[clap(
    version = "0.1.0",
    about = "Brucket command line interface",
    setting = AppSettings::DisableHelpSubcommand,
    setting = AppSettings::InferSubcommands,
    setting = AppSettings::SubcommandRequiredElseHelp,
    global_setting = AppSettings::ColoredHelp,
    global_setting = AppSettings::UnifiedHelpMessage,
    global_setting = AppSettings::VersionlessSubcommands
)]
pub enum Brucket {
    Interpret(Interpret),
    Compile(Compile),
}

impl Execute for Brucket {
    fn execute(self) -> CommandResult {
        match self {
            Brucket::Interpret(interpret) => interpret.execute(),
            Brucket::Compile(compile) => compile.execute(),
        }
    }
}
