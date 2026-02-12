//! # Nested Expressions Parser
//!
//! This module implements the parsing of complex nested expressions with proper operator precedence
//! and associativity rules using the **precedence climbing** algorithm.
//!
//! ## Precedence Climbing Algorithm
//!
//! The algorithm works by recursively parsing expressions while respecting operator precedence and associativity.
//! It's more efficient than the shunting-yard algorithm and easier to implement recursively.
//!
//! ### Key Concepts:
//!
//! 1. **Precedence**: Determines the order of operations (e.g., `*` before `+`)
//! 2. **Associativity**: Determines the evaluation order for operators with the same precedence
//!    - Left-associative: `a + b + c` → `(a + b) + c`
//!    - Right-associative: `a ** b ** c` → `a ** (b ** c)`
//!
//! ### Algorithm Overview:
//!
//! 1. Parse the left-hand side (primary expression)
//! 2. While there are operators with sufficient precedence:
//!    - Get operator precedence and associativity
//!    - For right-associative operators, use same precedence for next level
//!    - For left-associative, use precedence + 1
//!    - Recursively parse the right-hand side
//!    - Combine into a binary expression node
//!
//! ### Example:
//!
//! For the expression `1 + 2 * 3`:
//! 1. Parse `1` (primary)
//! 2. See `+` (precedence 3), parse right with min_precedence = 4
//! 3. Parse `2` (primary)
//! 4. See `*` (precedence 4), which is >= min_precedence
//! 5. Parse `3` (primary)
//! 6. Combine `2 * 3`
//! 7. Combine `1 + (2 * 3)`

mod binary_expressions;
mod error_handling;

pub use binary_expressions::parse_nested_binary_expression;
pub use error_handling::ExpressionError;

/// Maximum allowed nesting depth to prevent stack overflow
/// This protects against stack overflow from deeply nested expressions
/// and helps detect potential infinite recursion.
const MAX_NESTING_DEPTH: usize = 100;
