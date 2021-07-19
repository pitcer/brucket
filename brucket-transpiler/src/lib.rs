#![warn(clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc
)]
#![warn(clippy::cargo)]
#![allow(clippy::cargo_common_metadata)]
#![forbid(unsafe_code)]

pub mod translator;
pub mod transpiler;
