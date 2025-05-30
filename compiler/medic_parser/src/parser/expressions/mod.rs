use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::get_binary_operator;

use crate::parser::{
    take_token_if, BinaryExpressionNode, BinaryOperator, BlockNode, ExpressionNode, StatementNode,
    TokenSlice, TokenType,
};

use super::{
    identifiers::parse_identifier, literals::parse_literal, parse_block,
    statements::parse_statement,
};

// Re-export nested expressions API
pub mod nested;
pub use nested::{parse_block_expression, parse_nested_binary_expression};

/// Parses an expression, which can be a binary expression, primary expression, or block expression.
///
/// This function handles the parsing of all expression types in the language, including:
/// - Primary expressions (literals, identifiers, parenthesized expressions)
/// - Binary expressions with operator precedence and associativity
/// - Special handling for medical operators 'of' and 'per'
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_expression ===");
    log::debug!("Input length: {}", input.0.len());
    if !input.0.is_empty() {
        log::debug!("Next token: {:?}", input.0[0].token_type);
    }

    // Use the precedence climbing parser for the entire expression
    let result = parse_nested_binary_expression(input, 0, false);

    match &result {
        Ok((remaining, _)) => {
            log::debug!(
                "parse_expression success, remaining tokens: {}",
                remaining.0.len()
            );
            if !remaining.0.is_empty() {
                log::debug!(
                    "Next token after expression: {:?}",
                    remaining.0[0].token_type
                );
            }
        }
        Err(e) => {
            log::error!("parse_expression failed: {:?}", e);
        }
    }

    result
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
    log::debug!("=== parse_primary ===");
    log::debug!("Input length: {}", input.0.len());

    if input.0.is_empty() {
        log::error!("parse_primary: Empty input");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    log::debug!("Next token: {:?}", input.0[0].token_type);

    match input.0[0].token_type {
        // Handle literals
        TokenType::Integer(_)
        | TokenType::Float(_)
        | TokenType::String(_)
        | TokenType::Boolean(_) => {
            log::debug!("Parsing literal: {:?}", input.0[0].token_type);
            let (input, lit) = parse_literal(input)?;
            log::debug!("Successfully parsed literal: {:?}", lit);
            Ok((input, ExpressionNode::Literal(lit)))
        }
        // Handle identifiers and member expressions
        TokenType::Identifier(_) | TokenType::Dot => parse_identifier(input),
        // Handle parenthesized expressions
        TokenType::LeftParen => {
            log::debug!("Parsing parenthesized expression");
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)
                    .map_err(|e| {
                        log::error!("Failed to parse left parenthesis: {:?}", e);
                        e
                    })?;

            let (input, expr) = parse_expression(input).map_err(|e| {
                log::error!("Failed to parse expression inside parentheses: {:?}", e);
                e
            })?;

            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag)(input)
                    .map_err(|e| {
                        log::error!("Failed to parse right parenthesis: {:?}", e);
                        e
                    })?;

            log::debug!("Successfully parsed parenthesized expression");
            Ok((input, expr))
        }
        // Handle block expressions
        TokenType::LeftBrace => {
            log::debug!("Parsing block expression");
            // Parse the block as a statement first
            let (input, block) = parse_block(input).map_err(|e| {
                log::error!("Failed to parse block: {:?}", e);
                e
            })?;

            log::debug!(
                "Successfully parsed block with {} statements",
                block.statements.len()
            );

            // Create a block statement node
            let stmt = StatementNode::Block(block);

            // Convert the statement to an expression
            // This will create an expression containing the statement
            let expr = ExpressionNode::from_statement(stmt);
            log::debug!("Converted block to expression: {:?}", expr);

            Ok((input, expr))
        }
        _ => {
            log::error!(
                "Unexpected token in parse_primary: {:?}",
                input.0[0].token_type
            );
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
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
