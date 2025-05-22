use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    get_binary_operator, get_operator_precedence, take_token_if, BinaryExpressionNode,
    BinaryOperator, ExpressionNode, TokenSlice, TokenType,
};

use super::{identifiers::parse_identifier, literals::parse_literal};

/// Parses an expression, which can be a binary expression or a primary expression.
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Start with the lowest precedence (0) and not in a comparison context
    parse_binary_expression(input, 0, false)
}

/// Parses a primary expression, which is an expression that can appear as an operand in other expressions.
/// This includes literals, identifiers, and parenthesized expressions.
pub fn parse_primary(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    if input.0.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    // First parse a primary expression (literal, identifier, or member expression)
    let (mut input, mut expr) = match input.0[0].token_type {
        // Handle literals
        TokenType::Integer(_) | TokenType::Float(_) | TokenType::String(_) | TokenType::Bool(_) => {
            let (input, lit) = parse_literal(input)?;
            (input, ExpressionNode::Literal(lit))
        }
        // Handle identifiers and member expressions
        TokenType::Identifier(_) | TokenType::Dot => {
            return parse_identifier(input);
        }
        // Handle parenthesized expressions
        TokenType::LeftParen => {
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)?;
            let (input, expr) = parse_expression(input)?;
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag)(input)?;
            (input, expr)
        }
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    };

    // Check for implicit multiplication in specific medical contexts (e.g., "2 mg" or "3 doses")
    // Only apply implicit multiplication if the primary is a number and the next token is an identifier
    if let (ExpressionNode::Literal(_), Some(TokenType::Identifier(_))) =
        (&expr, input.peek().map(|t| &t.token_type))
    {
        // Parse the identifier as a primary expression
        let (new_input, right) = parse_primary(input)?;
        input = new_input;

        // Create a multiplication expression
        expr = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
            left: expr,
            operator: BinaryOperator::Mul,
            right,
        }));
    }

    Ok((input, expr))
}

/// Returns `true` if the given operator is a comparison operator (`==`, `!=`, `<`, `<=`, `>`, or `>=`).
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

/// Parses a binary expression from the input, respecting operator precedence and associativity.
pub fn parse_binary_expression(
    input: TokenSlice<'_>,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Parse the left-hand side
    let (mut input, mut left) = parse_primary(input)?;
    println!("Initial left: {:?}", left);

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
                // For the 'of' and 'per' operators, use the same precedence to allow chaining
                let next_min_precedence = if is_right_assoc {
                    // For right-associative operators (like **), use the same precedence
                    // This ensures proper right-associativity: 2 ** 3 ** 4 parses as 2 ** (3 ** 4)
                    precedence
                } else if op == BinaryOperator::Of || op == BinaryOperator::Per {
                    // For 'of' and 'per' operators, use the same precedence to allow chaining
                    precedence
                } else {
                    // For left-associative operators, use precedence + 1
                    precedence + 1
                };

                println!(
                    "Operator: {:?}, precedence: {}, next_min_precedence: {}",
                    op, precedence, next_min_precedence
                );

                // Check if this is a comparison operator
                let is_comparison = is_comparison_operator(&op);

                // Consume the operator - we already checked it with get_binary_operator
                let (new_input, _) =
                    take_token_if(|t| get_binary_operator(t).is_some(), ErrorKind::Tag)(input)?;
                input = new_input;

                // Parse the right-hand side with the appropriate precedence and comparison context
                println!("Parsing RHS with min_precedence: {}", next_min_precedence);
                let (new_input, right) = parse_binary_expression(
                    input,
                    next_min_precedence,
                    is_comparison || in_comparison,
                )?;
                input = new_input;
                println!("Parsed RHS: {:?}", right);

                // Create a binary expression
                let new_expr = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left,
                    operator: op,
                    right,
                }));
                println!("Created binary expression: {:?}", new_expr);
                left = new_expr;

                continue;
            }
        }
        break;
    }

    Ok((input, left))
}
