use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    get_binary_operator, get_operator_precedence, is_comparison_operator, BinaryExpressionNode,
    BinaryOperator, ExpressionNode, TokenSlice, TokenType,
};
use crate::parser::expressions::nested::error_handling::ExpressionError;

/// Parses a binary expression with proper precedence and associativity handling.
///
/// This function uses the precedence climbing algorithm to handle operator
/// precedence and associativity correctly.
pub fn parse_nested_binary_expression(
    input: TokenSlice<'_>,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    use BinaryOperator::*;
    use ExpressionNode::*;

    // Parse the left-hand side (primary expression)
    let (mut input, mut left) = super::super::parse_primary(input)?;

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
        input = input.advance();

        // For all operators, use the standard precedence climbing
        // The precedence is already handled by the min_precedence parameter
        let (new_input, right) = parse_nested_binary_expression(
            input,
            next_min_precedence,
            is_comparison_operator(&op),
        )?;

        input = new_input;

        // Create the binary expression node
        left = Binary(Box::new(BinaryExpressionNode {
            left,
            operator: op,
            right,
        }));
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
            assert_eq!(e.code, ErrorKind::Tag, "Expected Tag error for chained comparison");
        } else {
            panic!("Expected nom::Err::Error, got {:?}", result);
        }
    }
}
