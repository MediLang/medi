//! Nested expressions parsing module
//!
//! This module handles parsing of complex nested expressions with proper operator precedence
//! and associativity rules specific to the Medi language.

mod binary_expressions;
mod block_expressions;
mod error_handling;

pub use binary_expressions::parse_nested_binary_expression;
pub use block_expressions::parse_block_expression;
pub use error_handling::ExpressionError;

/// Maximum allowed nesting depth to prevent stack overflow
const MAX_NESTING_DEPTH: usize = 100;
