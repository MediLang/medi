//! Abstract Syntax Tree (AST) for the Medi programming language.
//!
//! This crate defines the AST nodes used to represent Medi programs, along with
//! utilities for traversing and transforming the AST.

pub mod ast;
pub mod conversions;
pub mod visit;

// Re-export commonly used types
pub use ast::Spanned;

use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt;

/// A result type for AST operations.
pub type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

/// Serializes an AST node to a JSON string.
///
/// # Example
///
/// ```
/// use medic_ast::ast::*;
/// use medic_ast::to_json;
/// use medic_ast::visit::Span;
///
/// let span = Span { start: 0, end: 0, line: 1, column: 1 };
/// let expr = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(42), span));
/// let json = to_json(&expr).unwrap();
///
/// // Print the JSON for debugging
/// println!("Serialized JSON: {}", json);
///
/// // Check for key parts of the JSON in a more flexible way
/// assert!(json.contains(r#"type": "Literal"#), "JSON should contain type: Literal");
/// assert!(json.contains(r#"type": "Int"#), "JSON should contain Int type");
/// assert!(json.contains(r#"value": 42"#), "JSON should contain value 42");
/// assert!(json.contains(r#"start": 0"#), "JSON should contain start position");
/// assert!(json.contains(r#"end": 0"#), "JSON should contain end position");
/// assert!(json.contains(r#"line": 1"#), "JSON should contain line number");
/// assert!(json.contains(r#"column": 1"#), "JSON should contain column number");
/// ```
pub fn to_json<T: Serialize>(value: &T) -> Result<String> {
    Ok(serde_json::to_string_pretty(value)?)
}

/// Deserializes an AST node from a JSON string.
///
/// # Examples
///
/// ```no_run
/// use medic_ast::ast::*;
/// use medic_ast::from_json;
///
/// let json = r#"{"Literal":42}"#;
/// let expr: ExpressionNode = from_json(json).unwrap();
/// ```
pub fn from_json<T: for<'de> Deserialize<'de>>(json: &str) -> Result<T> {
    Ok(serde_json::from_str(json)?)
}

/// A utility for pretty-printing AST nodes.
#[derive(Default)]
pub struct AstPrinter {
    indent: usize,
}

impl AstPrinter {
    /// Creates a new `AstPrinter`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Prints an AST node to a string.
    pub fn print<T: fmt::Display>(&self, node: &T) -> String {
        format!("{node}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    use crate::visit::Span;

    #[test]
    fn test_serialization() -> Result<()> {
        let span = Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        };
        let expr = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), span)),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(2), span)),
            }),
            span,
        ));

        let json = to_json(&expr).map_err(|e| e.to_string())?;
        let deserialized: ExpressionNode = from_json(&json).map_err(|e| e.to_string())?;
        assert_eq!(expr, deserialized);
        Ok(())
    }
}
