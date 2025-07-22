//! Abstract Syntax Tree (AST) for the Medi programming language.
//!
//! This crate defines the AST nodes used to represent Medi programs, along with
//! utilities for traversing and transforming the AST.

pub mod ast;
pub mod conversions;
pub mod visit;

// Re-export commonly used types
pub use ast::Spanned;

use std::error::Error;
use std::fmt;
use serde::{Serialize, Deserialize};

/// A result type for AST operations.
pub type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

/// Serializes an AST node to a JSON string.
///
/// # Examples
///
/// ```no_run
/// use medic_ast::ast::*;
/// use medic_ast::to_json;
///
/// let expr = ExpressionNode::Literal(LiteralNode::Int(42));
/// let json = to_json(&expr).unwrap();
/// println!("{}", json);
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
        format!("{}", node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_serialization() -> Result<()> {
        let expr = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
            left: ExpressionNode::Literal(LiteralNode::Int(1)),
            operator: BinaryOperator::Add,
            right: ExpressionNode::Literal(LiteralNode::Int(2)),
        }));

        let json = to_json(&expr)?;
        let deserialized: ExpressionNode = from_json(&json)?;
        assert_eq!(expr, deserialized);
        Ok(())
    }
}
