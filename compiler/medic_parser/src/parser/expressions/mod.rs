use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::get_binary_operator;

use crate::parser::{
    take_token_if, BinaryExpressionNode, BinaryOperator, BlockNode, ExpressionNode, StatementNode,
    TokenSlice, TokenType,
};

use super::{identifiers::parse_identifier, literals::parse_literal, statements::parse_statement};

// Re-export nested expressions API
pub mod nested;
pub use nested::parse_nested_binary_expression;

/// Parses an expression, which can be a binary expression, primary expression, or block expression.
///
/// This function handles the parsing of all expression types in the language, including:
/// - Primary expressions (literals, identifiers, parenthesized expressions)
/// - Binary expressions with operator precedence and associativity
/// - Special handling for medical operators 'of' and 'per'
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Use the precedence climbing parser for the entire expression
    parse_nested_binary_expression(input, 0, false)
}

/// Parses a binary expression with special handling for 'of' and 'per' operators.
///
/// This is a helper function that wraps the standard binary expression parser
/// to handle the special precedence rules for 'of' and 'per'.
pub fn parse_binary_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // First, parse the left-hand side
    let (mut input, mut left) = parse_primary(input)?;

    // Check for 'of' or 'per' operators with special handling
    if !input.0.is_empty() {
        match input.0[0].token_type {
            TokenType::Of => {
                // Consume the 'of' token
                input = input.advance();

                // Parse the right-hand side with higher precedence for 'of'
                // This ensures '2 of 3 * doses' is parsed as '2 of (3 * doses)'
                let (new_input, right) = parse_nested_binary_expression(input, 1, false)?;
                input = new_input;

                // Create the 'of' expression
                left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left,
                    operator: BinaryOperator::Of,
                    right,
                }));

                // Continue parsing any remaining binary expressions
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed more operators, combine with the 'of' expression
                if let ExpressionNode::Binary(bin) = expr {
                    left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    }));
                }
            }
            TokenType::Per => {
                // Consume the 'per' token
                input = input.advance();

                // Parse the right-hand side with higher precedence for 'per'
                // This ensures '5 * mg per day' is parsed as '(5 * mg) per day'
                let (new_input, right) = parse_nested_binary_expression(input, 1, false)?;
                input = new_input;

                // Create the 'per' expression
                left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left,
                    operator: BinaryOperator::Per,
                    right,
                }));

                // Continue parsing any remaining binary expressions
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed more operators, combine with the 'per' expression
                if let ExpressionNode::Binary(bin) = expr {
                    left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    }));
                }
            }
            _ => {
                // For other operators, use the standard precedence climbing
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed a binary expression, combine it with the left-hand side
                if let ExpressionNode::Binary(bin) = expr {
                    left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    }));
                } else {
                    left = expr;
                }
            }
        }
    }

    Ok((input, left))
}

/// Parses a primary expression, which is an expression that can appear as an operand in other expressions.
/// This includes literals, identifiers, parenthesized expressions, and block expressions.
pub fn parse_primary(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    if input.0.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    match input.0[0].token_type {
        // Handle literals
        TokenType::Integer(_) | TokenType::Float(_) | TokenType::String(_) | TokenType::Bool(_) => {
            let (input, lit) = parse_literal(input)?;

            // Check for implicit multiplication with an identifier (e.g., "3 doses" or "5 mg")
            if !input.0.is_empty() {
                if let TokenType::Identifier(_) = input.0[0].token_type {
                    let (input, right) = parse_identifier(input)?;
                    return Ok((
                        input,
                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left: ExpressionNode::Literal(lit),
                            operator: BinaryOperator::Mul,
                            right,
                        })),
                    ));
                }
            }

            Ok((input, ExpressionNode::Literal(lit)))
        }
        // Handle identifiers and member expressions
        TokenType::Identifier(_) | TokenType::Dot => parse_identifier(input),
        // Handle parenthesized expressions
        TokenType::LeftParen => {
            // Consume the left parenthesis
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)?;

            // Parse the inner expression using parse_expression to handle binary operations
            let (input, expr) = parse_expression(input)?;

            // Consume the right parenthesis
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag)(input)?;

            // Return the inner expression - parse_expression will have already handled the binary operation
            Ok((input, expr))
        }
        // Block expressions are not directly part of the expression grammar in the AST
        // They are only allowed in statement context
        TokenType::LeftBrace => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Returns `true` if the given operator is a comparison operator (`==`, `!=`, `<`, `<=`, `>`, or `>=`).
pub(crate) fn is_comparison_operator(op: &BinaryOperator) -> bool {
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

/// Parses a block expression: `{ <statements> }`
pub fn parse_block_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    let (mut input, _) =
        take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;

    let mut statements = Vec::new();

    let mut found_brace = false;

    // Parse statements until we hit a right brace or end of input
    while !input.is_empty() {
        // Skip any leading semicolons before the next statement
        while matches!(
            input.peek().map(|t| &t.token_type),
            Some(TokenType::Semicolon)
        ) {
            let (new_input, _) =
                take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
            input = new_input;
        }

        // Check for right brace (after skipping semicolons)
        if let Some(TokenType::RightBrace) = input.peek().map(|t| &t.token_type) {
            let (new_input, _) =
                take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;
            input = new_input;
            found_brace = true;
            break;
        }

        // Parse a statement
        match parse_statement(input) {
            Ok((new_input, stmt)) => {
                input = new_input;
                statements.push(stmt);

                // Skip any semicolons after the statement
                while matches!(
                    input.peek().map(|t| &t.token_type),
                    Some(TokenType::Semicolon)
                ) {
                    let (next_input, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(input)?;
                    input = next_input;
                }
            }
            Err(e) => return Err(e),
        }
    }

    // Ensure we found a closing brace
    if !found_brace {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }

    Ok((input, BlockNode { statements }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::test_utils::tokenize;
    use medic_ast::ast::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_primary_literal() {
        let tokens = tokenize("42");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Literal(LiteralNode::Int(42)) => {}
            _ => panic!("Expected integer literal 42"),
        }
    }

    #[test]
    fn test_parse_primary_identifier() {
        let tokens = tokenize("x");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Identifier(ident) => assert_eq!(ident.name, "x"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_parse_primary_parens() {
        let tokens = tokenize("(x + 1)");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Binary(_) => {}
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_block_expression() {
        let tokens = tokenize("{ let x = 5; }");
        let (_, block) = parse_block_expression(TokenSlice::new(&tokens)).unwrap();
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_primary_block_expression_fails() {
        let tokens = tokenize("{ 42 }");
        assert!(parse_primary(TokenSlice::new(&tokens)).is_err());
    }

    #[test]
    fn test_parse_block_expression_missing_brace() {
        let input = "{ let x = 5 "; // Missing closing brace
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_block_expression(token_slice);

        // Should return an error about the missing closing brace
        assert!(result.is_err(), "Expected error for missing closing brace");
    }

    #[test]
    fn test_parse_block_expression_with_stray_semicolons() {
        // Test with semicolons in various positions
        let test_cases = [
            "{ ;let x = 1; let y = 2; }",
            "{ let x = 1;; let y = 2; }",
            "{ let x = 1; ;let y = 2; }",
            "{ ;;;let x = 1;;; let y = 2;;; }",
            "{ ; ; let x = 1; ; ; let y = 2; ; ; }",
        ];

        for input in test_cases {
            let tokens = tokenize(input);
            let result = parse_block_expression(TokenSlice::new(&tokens));
            assert!(result.is_ok(), "Failed to parse: {}", input);

            let (_, block) = result.unwrap();
            assert_eq!(
                block.statements.len(),
                2,
                "Incorrect number of statements in: {}",
                input
            );
        }
    }
}
