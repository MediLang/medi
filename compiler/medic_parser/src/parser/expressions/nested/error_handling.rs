use std::fmt;
use thiserror::Error;

/// Errors that can occur during expression parsing
#[derive(Debug, Error, PartialEq)]
pub enum ExpressionError {
    /// Maximum nesting depth exceeded
    #[error("Maximum nesting depth of {0} exceeded")]
    MaxNestingDepthExceeded(usize),

    /// Unexpected token in expression
    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),

    /// Expected a different token
    #[error("Expected {0}")]
    Expected(String),

    /// Invalid operator for operation
    #[error("Invalid operator: {0}")]
    InvalidOperator(String),

    /// Type mismatch in expression
    #[error("Type mismatch: {0}")]
    TypeMismatch(String),

    /// Generic parsing error
    #[error("Failed to parse expression: {0}")]
    ParseError(String),
}

/// Result type for expression parsing operations
pub type ExpressionResult<T> = Result<T, ExpressionError>;

/// Helper function to create a parse error with context
pub(crate) fn parse_error<T, S: Into<String>>(msg: S) -> Result<T, ExpressionError> {
    Err(ExpressionError::ParseError(msg.into()))
}

/// Helper function to create an unexpected token error
pub(crate) fn unexpected_token<T, S: Into<String>>(token: S) -> Result<T, ExpressionError> {
    Err(ExpressionError::UnexpectedToken(token.into()))
}

/// Helper function to create an expected token error
pub(crate) fn expected<T, S: Into<String>>(expected: S) -> Result<T, ExpressionError> {
    Err(ExpressionError::Expected(expected.into()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_error_display() {
        assert_eq!(
            ExpressionError::MaxNestingDepthExceeded(100).to_string(),
            "Maximum nesting depth of 100 exceeded"
        );

        assert_eq!(
            ExpressionError::UnexpectedToken("+".to_string()).to_string(),
            "Unexpected token: +"
        );

        assert_eq!(
            ExpressionError::Expected("identifier".to_string()).to_string(),
            "Expected identifier"
        );
    }
}
