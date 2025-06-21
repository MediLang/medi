use super::super::*;
use crate::parser::TokenSlice;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

/// Converts a string into a `TokenSlice` and its corresponding vector of tokens.
///
/// This function lexes the input string into tokens, creates a `TokenSlice` referencing the tokens with a static lifetime, and returns both the `TokenSlice` and the owned vector of tokens.
///
/// # Examples
///
/// ```
/// let (token_slice, tokens) = str_to_token_slice("let x = 42;");
/// assert!(!tokens.is_empty());
/// ```
fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let tokens_static = Box::new(tokens.clone());
    let tokens_ref = Box::leak(tokens_static);
    (TokenSlice(tokens_ref), tokens)
}

#[cfg(test)]
mod statements_test {
    use super::*;
    use crate::tests::init_test_logger;
    use medic_ast::ast::{ExpressionNode, LiteralNode, StatementNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_let_statement() {
        let input = "let x = 42;";
        let (token_slice, _tokens) = str_to_token_slice(input);

        let result = parse_statement(token_slice);
        assert!(result.is_ok(), "Failed to parse let statement");

        let (remaining, stmt) = result.unwrap();
        assert!(remaining.is_empty(), "Didn't consume all input");

        if let StatementNode::Let(let_stmt) = stmt {
            assert_eq!(let_stmt.name.name, "x");
            assert!(matches!(
                let_stmt.value,
                ExpressionNode::Literal(LiteralNode::Int(42))
            ));
        } else {
            panic!("Expected a let statement, got: {:?}", stmt);
        }
    }

    // Add other test functions here...

    #[test]
    fn test_parse_match_statement_with_various_expressions() {
        init_test_logger();
        use medic_ast::ast::{ExpressionNode, LiteralNode, PatternNode};

        log::info!("Starting test_parse_match_statement_with_various_expressions");

        // Test with identifier
        let input = "match x { 1 => 1, _ => 0 }";
        let (token_slice, tokens) = str_to_token_slice(input);

        // Print the tokens for debugging
        println!("Tokens:");
        for (i, token) in tokens.iter().enumerate() {
            println!("  {}: {:?}", i, token.token_type);
        }

        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with identifier"
        );

        // Verify AST structure for identifier match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            // Verify the matched expression is an identifier
            if let ExpressionNode::Identifier(ident) = &*match_stmt.expr {
                assert_eq!(ident.name, "x");
            } else {
                panic!(
                    "Expected identifier 'x' as match expression, got {:?}",
                    match_stmt.expr
                );
            }

            // Verify the match arms
            assert_eq!(match_stmt.arms.len(), 2, "Expected 2 match arms");

            // First arm: 1 => 1
            if let PatternNode::Literal(lit) = &match_stmt.arms[0].pattern {
                if let LiteralNode::Int(1) = lit {
                    // Pattern matches 1
                } else {
                    panic!("Expected literal 1 in first pattern");
                }
            } else {
                panic!("Expected literal pattern in first arm");
            }

            // Second arm: _ => 0 (wildcard pattern)
            if let PatternNode::Wildcard = match_stmt.arms[1].pattern {
                if let ExpressionNode::Literal(LiteralNode::Int(0)) = *match_stmt.arms[1].body {
                    // Body is 0
                } else {
                    panic!("Expected 0 as body of second arm");
                }
            } else {
                panic!("Expected wildcard pattern in second arm");
            }
        } else {
            panic!("Expected Match statement");
        }

        // Test with literal
        let input = "match 42 { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with literal"
        );

        // Verify AST structure for literal match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            if let ExpressionNode::Literal(LiteralNode::Int(42)) = &*match_stmt.expr {
                // Literal matches 42
            } else {
                panic!(
                    "Expected literal 42 as match expression, got {:?}",
                    match_stmt.expr
                );
            }
        } else {
            panic!("Expected Match statement");
        }

        // Test with binary expression
        let input = "match x + 1 { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with binary expression"
        );

        // Verify AST structure for binary expression match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            if let ExpressionNode::Binary(bin_expr) = match_stmt.expr.as_ref() {
                if let ExpressionNode::Identifier(ident) = &bin_expr.left {
                    assert_eq!(ident.name, "x");
                } else {
                    panic!("Expected 'x' as left operand of binary expression");
                }
                assert_eq!(bin_expr.operator.to_string(), "+");
                if let ExpressionNode::Literal(LiteralNode::Int(1)) = &bin_expr.right {
                    // Correct
                } else {
                    panic!("Expected 1 as right operand of binary expression");
                }
            } else {
                panic!(
                    "Expected binary expression as match expression, got {:?}",
                    match_stmt.expr
                );
            }
        } else {
            panic!("Expected Match statement");
        }

        // Test with function call
        let input = "match some_function() { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with function call"
        );

        // Verify AST structure for function call match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            if let ExpressionNode::Call(call_expr) = match_stmt.expr.as_ref() {
                if let ExpressionNode::Identifier(ident) = &call_expr.callee {
                    assert_eq!(ident.name, "some_function");
                } else {
                    panic!("Expected 'some_function' as callee");
                }
                assert!(
                    call_expr.arguments.is_empty(),
                    "Expected no arguments in function call"
                );
            } else {
                panic!(
                    "Expected function call as match expression, got {:?}",
                    match_stmt.expr
                );
            }
        } else {
            panic!("Expected Match statement");
        }

        // Test with member expression
        let input = "match some_object.property { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with member expression"
        );

        // Verify AST structure for member expression match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            if let ExpressionNode::Member(member_expr) = match_stmt.expr.as_ref() {
                if let ExpressionNode::Identifier(ident) = &member_expr.object {
                    assert_eq!(ident.name, "some_object");
                } else {
                    panic!("Expected 'some_object' as object in member expression");
                }
                assert_eq!(member_expr.property.name, "property");
            } else {
                panic!(
                    "Expected member expression as match expression, got {:?}",
                    match_stmt.expr
                );
            }
        } else {
            panic!("Expected Match statement");
        }
    }

    #[test]
    fn test_parse_match_statement_with_complex_expression() {
        // Enable logging for the test
        std::env::set_var("RUST_LOG", "debug");
        env_logger::init();

        // Test with a simple expression first to verify basic functionality
        let input = "match x { 1 => 1, _ => 0 }";
        let (token_slice, tokens) = str_to_token_slice(input);

        // Print the tokens for debugging
        println!("Tokens:");
        for (i, token) in tokens.iter().enumerate() {
            println!("  {}: {:?}", i, token);
        }

        let result = parse_statement(token_slice);

        // Print detailed error information if parsing failed
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }

        // First, verify parsing succeeded
        assert!(result.is_ok(), "Failed to parse basic match statement");

        // Then verify the AST structure
        let (_remaining_tokens, statement) = result.unwrap();
        if let StatementNode::Match(match_stmt) = statement {
            // Verify the matched expression is an identifier
            if let ExpressionNode::Identifier(ident) = &*match_stmt.expr {
                assert_eq!(ident.name, "x");
            } else {
                panic!(
                    "Expected identifier 'x' as match expression, got {:?}",
                    match_stmt.expr
                );
            }

            // Verify the match arms
            assert_eq!(match_stmt.arms.len(), 2, "Expected 2 match arms");

            // First arm: 1 => 1
            if let PatternNode::Literal(lit) = &match_stmt.arms[0].pattern {
                if let LiteralNode::Int(1) = lit {
                    // Pattern matches 1
                } else {
                    panic!("Expected literal 1 in first pattern");
                }
            } else {
                panic!("Expected literal pattern in first arm");
            }

            // Second arm: _ => 0 (wildcard pattern)
            if let PatternNode::Wildcard = match_stmt.arms[1].pattern {
                // Pattern is wildcard (_)
                if let ExpressionNode::Literal(LiteralNode::Int(0)) = *match_stmt.arms[1].body {
                    // Body is 0
                } else {
                    panic!("Expected 0 as body of second arm");
                }
            } else {
                panic!("Expected wildcard pattern in second arm");
            }
        } else {
            panic!("Expected Match statement, got {:?}", statement);
        }
    }
}
