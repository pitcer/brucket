pub mod c_macro;
pub mod c_struct;
pub mod c_type;
pub mod expression;
pub mod function;
pub mod instruction;
pub mod modifiers;
pub mod module;
pub mod typedef;

#[cfg(test)]
pub type TestResult = Result<(), String>;
