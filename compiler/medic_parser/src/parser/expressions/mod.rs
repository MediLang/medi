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

                // For right-associative operators, use the current precedence - 1
                // For left-associative, use current + 1
                // For the 'of' operator, we want to parse the RHS with the same precedence
                // to allow for chained operations like '2 of 3 doses'
                let next_min_precedence = if is_right_assoc {
                    precedence.saturating_sub(1)
                } else if op == BinaryOperator::Of {
                    // For 'of' operator, use the same precedence to allow chaining
                    precedence
                } else if op == BinaryOperator::Per {
                    // For 'per' operator, use the same precedence to allow chaining
                    precedence
                } else {
                    precedence + 1
                };

                println!(
                    "Operator: {:?}, precedence: {}, next_min_precedence: {}",
                    op, precedence, next_min_precedence
                );

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
                                | TokenType::Arrow
                                | TokenType::Of
                                | TokenType::Per
                        )
                    },
                    ErrorKind::Tag,
                )(input)?;
                input = new_input;

                // Parse the right-hand side with the appropriate precedence and comparison context
                println!("Parsing RHS with min_precedence: {}", next_min_precedence);
                let (new_input, right) = if op == BinaryOperator::Of || op == BinaryOperator::Per {
                    // For 'of' and 'per' operators, parse the right-hand side as a full expression
                    // to allow for chained operations like '2 of 3 doses'
                    parse_expression(input)?
                } else {
                    parse_binary_expression(
                        input,
                        next_min_precedence,
                        is_comparison || in_comparison,
                    )?
                };
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

/// Parse an expression with the given minimum precedence
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // First, parse the left-hand side of the expression
    let (mut input, mut left) = parse_primary(input)?;

    // Special case: if the left is a literal and the next token is an identifier,
    // and the token after that is 'per', parse it as a multiplication
    if let ExpressionNode::Literal(_) = &left {
        // Check if the next token is an identifier and the one after is 'per'
        let mut peek_iter = input.0.iter();
        if let (Some(next_token), Some(next_next_token)) = (peek_iter.next(), peek_iter.next()) {
            if matches!(next_token.token_type, TokenType::Identifier(_))
                && matches!(next_next_token.token_type, TokenType::Per)
            {
                // Parse the identifier as the right-hand side of a multiplication
                let (new_input, right) = parse_primary(input)?;

                // Create a multiplication node for the left-hand side
                left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left,
                    operator: BinaryOperator::Mul,
                    right,
                }));

                input = new_input;
            }
        }
    }

    // Now parse any binary operators, including 'per' and unit conversion
    parse_binary_expression_with_left(input, left, 1, false)
}

/// Parse an expression with a minimum precedence and comparison context
fn parse_expression_with_min_precedence(
    input: TokenSlice<'_>,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // First parse a primary expression
    let (input, left) = parse_primary(input)?;

    // Parse binary expressions with the current left-hand side
    parse_binary_expression_with_left(input, left, min_precedence, in_comparison)
}

/// Parse a binary expression with a given left-hand side and minimum precedence
fn parse_binary_expression_with_left(
    input: TokenSlice<'_>,
    left: ExpressionNode,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    let (mut input, mut left) = (input, left);

    loop {
        // Check if there's a binary operator next
        if let Some(token) = input.peek() {
            if let Some((op, is_right_assoc)) = get_binary_operator(&token.token_type) {
                let precedence = get_operator_precedence(&op);

                // If the operator's precedence is less than our minimum, stop
                if precedence < min_precedence {
                    break;
                }

                // For right-associative operators, we need to use a lower precedence
                // to ensure proper right-associativity
                let next_min_precedence = if is_right_assoc {
                    precedence
                } else {
                    precedence + 1
                };

                // Consume the operator token
                let (new_input, _) = take_token_if(
                    |t| get_binary_operator(t).is_some_and(|(op2, _)| op == op2),
                    ErrorKind::Tag,
                )(input)?;
                input = new_input;

                // Special handling for 'of' and 'per' operators
                let (new_input, right) = if op == BinaryOperator::Of {
                    // For 'of', parse the right-hand side as a product (e.g., '3 doses')
                    let (new_input, first) = parse_primary(input)?;

                    // Check if the next token is an identifier (like 'doses')
                    if let Some(next_token) = new_input.peek() {
                        if let TokenType::Identifier(_) = next_token.token_type {
                            // Parse the identifier as the second part of the product
                            let (new_new_input, second) = parse_primary(new_input)?;

                            // Create a multiplication node for the right-hand side
                            let right = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                                left: first,
                                operator: BinaryOperator::Mul,
                                right: second,
                            }));

                            (new_new_input, right)
                        } else {
                            (new_input, first)
                        }
                    } else {
                        (new_input, first)
                    }
                } else if op == BinaryOperator::Per {
                    println!("Parsing 'per' operator. Left: {:?}", left);

                    // For 'per', ensure the left-hand side is a multiplication expression
                    // and parse the right-hand side as a single identifier (e.g., 'day')
                    let (new_input, new_left) = match left {
                        ExpressionNode::Literal(lit) => {
                            // If the left is a literal, check if the next token is an identifier
                            println!("Left is a literal: {:?}", lit);
                            if let Some(token) = input.peek() {
                                println!("Next token: {:?}", token);
                                if let TokenType::Identifier(_) = token.token_type {
                                    // Parse the identifier as the right-hand side of a multiplication
                                    println!("Parsing identifier after literal");
                                    let (new_input, right) = parse_primary(input)?;
                                    println!("Parsed right: {:?}", right);

                                    // Create a multiplication node for the left-hand side
                                    let new_left =
                                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                                            left: ExpressionNode::Literal(lit),
                                            operator: BinaryOperator::Mul,
                                            right,
                                        }));
                                    println!("Created new_left: {:?}", new_left);

                                    (new_input, new_left)
                                } else {
                                    println!("Next token is not an identifier");
                                    (input, ExpressionNode::Literal(lit))
                                }
                            } else {
                                println!("No more tokens after literal");
                                (input, ExpressionNode::Literal(lit))
                            }
                        }
                        _ => {
                            println!("Left is not a literal: {:?}", left);
                            (input, left)
                        }
                    };

                    input = new_input;
                    println!("After processing left, new_left: {:?}", new_left);

                    // Parse the right-hand side of the 'per' operator
                    println!("Parsing right-hand side of 'per' operator");
                    let (new_input, right) = parse_primary(input)?;
                    println!("Parsed right-hand side: {:?}", right);

                    // Create the binary expression with the new left-hand side
                    left = new_left;
                    (new_input, right)
                } else {
                    // For other operators, use the standard precedence rules
                    parse_expression_with_min_precedence(
                        input,
                        next_min_precedence,
                        in_comparison || is_comparison_operator(&op),
                    )?
                };
                input = new_input;

                // Create the binary expression
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

/// Helper function to parse binary expressions with a minimum precedence
fn parse_binary_expression_with_min_precedence(
    input: TokenSlice<'_>,
    left: ExpressionNode,
    min_precedence: u8,
    in_comparison: bool,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    let (mut input, mut left) = (input, left);

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
                let _next_min_precedence = if is_right_assoc {
                    precedence
                } else {
                    precedence + 1
                };

                // Check if this is a comparison operator
                let _is_comparison = is_comparison_operator(&op);

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
                                | TokenType::Arrow
                                | TokenType::Of
                                | TokenType::Per
                        )
                    },
                    ErrorKind::Tag,
                )(input)?;
                input = new_input;

                // Parse the right-hand side
                let (new_input, right) = parse_primary(input)?;
                input = new_input;

                // Create the binary expression
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
