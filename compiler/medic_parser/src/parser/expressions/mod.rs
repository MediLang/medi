use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    get_binary_operator, get_operator_precedence, take_token_if, BinaryExpressionNode,
    BinaryOperator, ExpressionNode, TokenSlice, TokenType,
};

// Import parse_expression with a different name to avoid conflict with our function
use crate::parser::parse_expression as parse_expression_global;

use super::{identifiers::parse_identifier, literals::parse_literal};

/// Check if an operator is a comparison operator
fn is_comparison_operator(op: &BinaryOperator) -> bool {
    matches!(
        op,
        BinaryOperator::Eq
            | BinaryOperator::Ne
            | BinaryOperator::Lt
            | BinaryOperator::Le
            | BinaryOperator::Gt
            | BinaryOperator::Ge
    )
}

/// Parse a binary expression with the given minimum precedence and a flag indicating if we're in a comparison context
pub fn parse_binary_expression(
    input: TokenSlice<'_>,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Parse the left-hand side
    let (mut input, mut left) = parse_primary(input)?;

    // Keep parsing binary operators as long as they have at least the minimum precedence
    loop {
        // Check if there's a binary operator next
        if let Some(token) = input.peek() {
            if let Some((op, is_right_assoc)) = get_binary_operator(&token.token_type) {
                let precedence = get_operator_precedence(&op);

                // If we're already in a comparison and this is another comparison operator, error out
                if in_comparison && is_comparison_operator(&op) {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        ErrorKind::Tag,
                    )));
                }

                // If the operator's precedence is less than the minimum, we're done
                if precedence < min_precedence {
                    break;
                }

                // For right-associative operators, use the current precedence
                // For left-associative, use current + 1
                let next_min_precedence = if is_right_assoc {
                    precedence
                } else {
                    precedence + 1
                };

                // Check if this is a comparison operator
                let is_comparison = is_comparison_operator(&op);

                // Consume the operator
                let (new_input, _) = take_token_if(
                    |t| {
                        matches!(
                            t,
                            TokenType::Or
                                | TokenType::And
                                | TokenType::EqualEqual
                                | TokenType::NotEqual
                                | TokenType::Less
                                | TokenType::LessEqual
                                | TokenType::Greater
                                | TokenType::GreaterEqual
                                | TokenType::Plus
                                | TokenType::Minus
                                | TokenType::Star
                                | TokenType::Slash
                                | TokenType::Percent
                        )
                    },
                    ErrorKind::Tag,
                )(input)?;
                input = new_input;

                // Parse the right-hand side with the appropriate precedence and comparison context
                let (new_input, right) = parse_binary_expression(
                    input,
                    next_min_precedence,
                    is_comparison || in_comparison,
                )?;
                input = new_input;

                // Create a binary expression
                left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left,
                    operator: op,
                    right,
                }));

                continue;
            }
        }
        break;
    }

    Ok((input, left))
}

/// Parse a primary expression (literals, identifiers, parenthesized expressions)
pub fn parse_primary(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Try to parse a literal (numbers, strings, etc.)
    if let Ok((input, lit)) = super::literals::parse_literal(input) {
        return Ok((input, ExpressionNode::Literal(lit)));
    }

    // Try to parse an identifier or member expression
    if let Ok((input, ident)) = super::identifiers::parse_identifier(input) {
        return Ok((input, ident));
    }

    // Try to parse a parenthesized expression
    if let Some(TokenType::LeftParen) = input.peek().map(|t| &t.token_type) {
        // Consume the left parenthesis
        let (input, _) = take_token_if(|t| t == &TokenType::LeftParen, ErrorKind::Tag)(input)?;

        // Parse the expression inside the parentheses
        let (input, expr) = parse_expression(input)?;

        // Consume the right parenthesis
        let (input, _) = take_token_if(|t| t == &TokenType::RightParen, ErrorKind::Tag)(input)?;

        return Ok((input, expr));
    }

    // If we get here, we couldn't parse any valid primary expression
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        ErrorKind::Tag,
    )))
}

/// Parse an expression
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    parse_binary_expression(input, 0, false)
}
