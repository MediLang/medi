use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::get_binary_operator;

use crate::parser::{
    parse_block, take_token_if, BinaryExpressionNode, BinaryOperator, BlockNode, ExpressionNode,
    StatementNode, TokenSlice, TokenType,
};

use super::{identifiers::parse_identifier, literals::parse_literal, statements::parse_statement};

// Re-export nested expressions API
pub mod nested;
pub mod struct_literal;

pub use nested::parse_nested_binary_expression;
pub use struct_literal::parse_struct_literal;

// Note: Block expressions are parsed using the `parse_block` function from the parent module.
// This is the primary and recommended way to parse block expressions. The function handles
// all aspects of block parsing including statements, semicolons, and scoping.
//
// Example usage:
// ```rust
// let (remaining_input, block) = parse_block(input_tokens)?;
// let expr = ExpressionNode::from_statement(StatementNode::Block(block));
// ```

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
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("=== parse_primary ===");
        log::debug!("Input length: {}", input.0.len());
    }

    if input.0.is_empty() {
        if log::log_enabled!(log::Level::Error) {
            log::error!("parse_primary: Empty input");
        }
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    if log::log_enabled!(log::Level::Debug) {
        log::debug!("Next token: {:?}", input.0[0].token_type);
    }

    match input.0[0].token_type {
        // Handle literals
        TokenType::Integer(_) | TokenType::Float(_) => {
            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Parsing number literal: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
                log::debug!("Remaining tokens: {}", input.0.len());
            }
            let (mut input, lit) = parse_literal(input)?;

            // Check for implicit multiplication with an identifier (e.g., "5 mg")
            if !input.0.is_empty() {
                if let TokenType::Identifier(_) = input.0[0].token_type {
                    let (new_input, right) = parse_identifier(input)?;
                    input = new_input;
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

            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Successfully parsed number literal: {:?}", lit);
            }
            Ok((input, ExpressionNode::Literal(lit)))
        }
        TokenType::String(_) | TokenType::Boolean(_) => {
            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Parsing literal: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
                log::debug!("Remaining tokens: {}", input.0.len());
            }
            let (input, lit) = parse_literal(input)?;
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Successfully parsed literal: {:?}", lit);
            }
            Ok((input, ExpressionNode::Literal(lit)))
        }
        // Handle identifiers, member expressions, and implicit multiplication
        TokenType::Identifier(_) => {
            log::debug!(
                "Found identifier '{}' at {}:{}, checking for struct literal",
                match &input.0[0].token_type {
                    TokenType::Identifier(s) => s.to_string(),
                    _ => "".to_string(),
                },
                input.0[0].location.line,
                input.0[0].location.column
            );
            log::debug!("Remaining tokens: {}", input.0.len());
            
            // In the context of an if statement, we don't want to treat `x {}` as a struct literal
            // Instead, we'll just parse it as an identifier and let the statement parser handle the block
            let is_in_if_context = input.0.iter().any(|t| matches!(t.token_type, TokenType::If));
            
            if !is_in_if_context {
                // Only check for struct literals if we're not in an if statement context
                if input.0.len() > 1 {
                    match &input.0[1].token_type {
                        TokenType::LeftBrace => {
                            log::debug!(
                                "Found LeftBrace at {}:{}, parsing as struct literal",
                                input.0[1].location.line,
                                input.0[1].location.column
                            );
                            return parse_struct_literal(input);
                        },
                        _ => log::debug!("Next token is {:?}, not a struct literal", input.0[1].token_type)
                    }
                } else {
                    log::debug!("No more tokens after identifier");
                }
                
                log::debug!("Not a struct literal, parsing as regular identifier");
                // Look ahead to see if this is a struct literal
                if input.0.len() > 1 && matches!(input.0[1].token_type, TokenType::LeftBrace) {
                    return parse_struct_literal(input);
                }
            } else {
                log::debug!("In if statement context, not treating as struct literal");
            }
            
            // Otherwise parse as identifier or member expression
            let (mut input, left) = parse_identifier(input)?;

            // Check for implicit multiplication with a following number (e.g., "doses 3")
            if !input.0.is_empty() {
                if let TokenType::Integer(_) | TokenType::Float(_) = input.0[0].token_type {
                    let (new_input, right) = parse_primary(input)?;
                    input = new_input;
                    return Ok((
                        input,
                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left,
                            operator: BinaryOperator::Mul,
                            right,
                        })),
                    ));
                }
            }

            Ok((input, left))
        }
        TokenType::Dot => {
            // First parse the identifier or member expression
            let (mut input, left) = parse_identifier(input)?;

            // Check for implicit multiplication with a following number (e.g., "doses 3")
            if !input.0.is_empty() {
                if let TokenType::Integer(_) | TokenType::Float(_) = input.0[0].token_type {
                    let (new_input, right) = parse_primary(input)?;
                    input = new_input;
                    return Ok((
                        input,
                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left,
                            operator: BinaryOperator::Mul,
                            right,
                        })),
                    ));
                }
            }

            Ok((input, left))
        }
        // Handle parenthesized expressions
        TokenType::LeftParen => {
            log::debug!("Found LeftParen, parsing parenthesized expression");
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Parsing parenthesized expression");
            }
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)
                    .map_err(|e| {
                        if log::log_enabled!(log::Level::Error) {
                            log::error!("Failed to parse left parenthesis: {:?}", e);
                        }
                        e
                    })?;

            let (input, expr) = parse_expression(input).map_err(|e| {
                if log::log_enabled!(log::Level::Error) {
                    log::error!("Failed to parse expression inside parentheses: {:?}", e);
                }
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
            log::debug!("Found LeftBrace, parsing block expression");
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Parsing block expression");
            }
            // Parse the block as a statement first
            let (input, block) = parse_block(input).map_err(|e| {
                if log::log_enabled!(log::Level::Error) {
                    log::error!("Failed to parse block: {:?}", e);
                }
                e
            })?;

            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Successfully parsed block with {} statements",
                    block.statements.len()
                );
            }

            // Create a block statement node
            let stmt = StatementNode::Block(block);

            // Convert the statement to an expression
            // This will create an expression containing the statement
            let expr = ExpressionNode::from_statement(stmt);
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Converted block to expression: {:?}", expr);
            }

            Ok((input, expr))
        }
        _ => {
            if log::log_enabled!(log::Level::Error) {
                log::error!(
                    "Unexpected token in parse_primary: {:?}",
                    input.0[0].token_type
                );
            }
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
    use crate::parser::{parse_block, TokenSlice};
    use medic_ast::ast::*;
    use medic_lexer::token::Token;

    // Helper function to tokenize a string for testing
    fn tokenize(input: &str) -> Vec<Token> {
        use medic_lexer::Lexer;
        let lexer = Lexer::new(input);
        lexer.collect()
    }

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
        let (_, block) = parse_block(TokenSlice::new(&tokens)).unwrap();
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_primary_block_expression_fails() {
        // Block expressions are valid primary expressions, so this should succeed
        let tokens = tokenize("{ 42 }");
        let result = parse_primary(TokenSlice::new(&tokens));
        assert!(
            result.is_ok(),
            "Block expressions should be valid primary expressions"
        );

        // Verify the parsed block has one statement
        if let Ok((_, ExpressionNode::Statement(stmt))) = result {
            if let StatementNode::Block(block) = *stmt {
                assert_eq!(block.statements.len(), 1, "Block should have one statement");
            } else {
                panic!("Expected block statement, got {:?}", stmt);
            }
        } else {
            panic!("Expected Statement variant, got {:?}", result);
        }
    }

    #[test]
    fn test_parse_block_expression_missing_brace() {
        let input = "{ let x = 5 "; // Missing closing brace
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_block(token_slice);
        assert!(result.is_err(), "Expected error for missing closing brace");
    }

    #[test]
    fn test_parse_block_expression_with_stray_semicolons() {
        // Test with semicolons in various positions
        let test_cases = [
            ("{ ;let x = 1; let y = 2; }", 2),            // Leading semicolon
            ("{ let x = 1;; let y = 2; }", 2),            // Double semicolon
            ("{ let x = 1; ;let y = 2; }", 2),            // Semicolon with space
            ("{ ;;;let x = 1;;; let y = 2;;; }", 2),      // Multiple semicolons
            ("{ ; ; let x = 1; ; ; let y = 2; ; ; }", 2), // Mixed semicolons with spaces
            ("{ ; }", 0),                                 // Just semicolons
            ("{ ; ; }", 0),                               // Multiple semicolons
            ("{ let x = 1; }", 1),                        // Single statement
            ("{ let x = 1; let y = 2; }", 2),             // Multiple statements
        ];

        for (input, expected_count) in test_cases {
            let tokens = tokenize(input);
            let result = parse_block(TokenSlice::new(&tokens));
            assert!(result.is_ok(), "Failed to parse: {}", input);

            let (_, block) = result.unwrap();
            assert_eq!(
                block.statements.len(),
                expected_count,
                "Incorrect number of statements in: {}",
                input
            );
        }
    }
}
