//! Tests for the parser module

// Import test modules from subdirectories
#[path = "../expressions/tests/mod.rs"]
mod expressions;

#[path = "../statements/tests/mod.rs"]
mod statements;

// These modules are now in their respective subdirectories
// and will be imported by the main modules

// Re-export test modules for easier access
pub use expressions::*;
pub use statements::*;
