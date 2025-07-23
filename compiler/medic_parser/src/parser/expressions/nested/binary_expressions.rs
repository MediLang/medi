//! # Binary Expressions Parser
//!
//! This module implements the core of the precedence climbing algorithm for parsing
//! binary expressions with proper operator precedence and associativity.
//!
//! ## Implementation Details
//!
//! The algorithm is implemented in the `parse_nested_binary_expression` function, which:
//! 1. Parses the left-hand side (primary expression)
//! 2. While there are operators with sufficient precedence:
//!    - Gets the operator's precedence and associativity
//!    - For right-associative operators (like `**`), uses the same precedence for the next level
//!    - For left-associative operators (like `+`, `*`), uses precedence + 1
//!    - Recursively parses the right-hand side
//!    - Combines into a binary expression node
//!
//! ## Error Handling
//!
//! The parser enforces these rules:
//! - Chained comparisons (e.g., `a < b < c`) are not allowed
//! - All binary operators must have properly matched operands
//! - Operator precedence and associativity are strictly enforced

use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::expressions::nested::error_handling::ExpressionError;
use crate::parser::{
    get_binary_operator, get_operator_precedence, is_comparison_operator, BinaryExpressionNode,
    BinaryOperator, ExpressionNode, TokenSlice, TokenType,
};
use medic_ast::ast::Spanned;

/// Parses a binary expression with proper precedence and associativity handling.
///
/// This function implements the precedence climbing algorithm to parse binary expressions
/// while respecting operator precedence and associativity rules.
///
/// # Arguments
///
/// * `input` - The input token stream to parse
/// * `min_precedence` - The minimum operator precedence to consider (used for recursion)
/// * `in_comparison` - Whether we're already in a comparison expression (to prevent chaining)
///
/// # Returns
///
/// Returns a tuple containing:
/// 1. The remaining unparsed input
/// 2. The parsed expression as an `ExpressionNode`
///
/// # Errors
///
/// Returns an error if:
/// - The input cannot be parsed as a valid expression
/// - Chained comparisons are detected (e.g., `a < b < c`)
/// - The maximum nesting depth is exceeded
pub fn parse_nested_binary_expression(
    input: TokenSlice<'_>,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    use BinaryOperator::*;
    use ExpressionNode::*;

    // Parse the left-hand side (primary expression)
    let (mut input, mut left) = super::super::parse_primary(input)?;
    let mut left_span = left.span().clone();

    // Keep parsing binary operators as long as they have sufficient precedence
    while let Some(token) = input.peek() {
        // Check if the next token is a binary operator
        let (op, is_right_assoc) = match get_binary_operator(&token.token_type) {
            Some((op, is_right)) => (op, is_right),
            None => break, // Not a binary operator, we're done
        };

        // Check for invalid comparison chaining (e.g., a < b < c)
        if in_comparison && is_comparison_operator(&op) {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }

        // Get the operator's precedence
        let precedence = get_operator_precedence(&op);

        // Stop if the operator's precedence is too low
        if precedence < min_precedence {
            break;
        }

        // For right-associative operators, we use the same precedence for the next level
        // For left-associative operators, we use precedence + 1 to ensure left associativity
        let next_min_precedence = if is_right_assoc {
            precedence
        } else {
            precedence + 1
        };

        // Consume the operator token
        let operator_span = token.location.into();
        input = input.advance();

        // Parse the right-hand side with the appropriate precedence
        let (new_input, right) = parse_nested_binary_expression(
            input,
            next_min_precedence,
            in_comparison || is_comparison_operator(&op),
        )?;

        // Update the span to cover the entire binary expression
        let right_span = right.span();
        let span = Spanned::combine(&left_span, &right_span);

        // Create the binary expression node
        let bin_expr = BinaryExpressionNode {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };

        // Wrap in Spanned and update left for the next iteration
        left = Binary(Spanned::new(Box::new(bin_expr), span));
        left_span = span;
        input = new_input;
    }

    Ok((input, left))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::test_utils::tokenize;
    use crate::parser::TokenSlice;
    use nom::error::ErrorKind;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_chained_comparison_error() {
        // Test that chained comparisons like a < b < c are not allowed
        let input = "1 < 2 < 3";
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_nested_binary_expression(token_slice, 0, false);

        // Should return an error for chained comparison
        assert!(result.is_err(), "Expected error for chained comparison");

        // Check that it's the right kind of error
        if let Err(nom::Err::Error(e)) = result {
            assert_eq!(
                e.code,
                ErrorKind::Tag,
                "Expected Tag error for chained comparison"
            );
        } else {
            panic!("Expected nom::Err::Error, got {:?}", result);
        }
    }

    #[test]
    fn test_chained_comparison_with_expression() {
        // Test that chained comparisons with expressions like a < b + c < d are not allowed
        let input = "1 < 2 + 3 < 4";
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_nested_binary_expression(token_slice, 0, false);

        // Should return an error for chained comparison with expression
        assert!(
            result.is_err(),
            "Expected error for chained comparison with expression"
        );

        // Check that it's the right kind of error
        if let Err(nom::Err::Error(e)) = result {
            assert_eq!(
                e.code,
                ErrorKind::Tag,
                "Expected Tag error for chained comparison with expression"
            );
        } else {
            panic!("Expected nom::Err::Error, got {:?}", result);
        }
    }
}
