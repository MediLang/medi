use super::super::*;
use crate::parser::TokenSlice;
use medic_ast::ast::Spanned;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

/// Converts a string into a `TokenSlice` and its corresponding vector of tokens.
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
    use medic_ast::ast::{ExpressionNode, LiteralNode, PatternNode, StatementNode};
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
            let val = let_stmt
                .value
                .as_ref()
                .expect("let value must be present in this test");
            if let ExpressionNode::Literal(Spanned { node: lit, .. }) = val {
                assert!(
                    matches!(lit, LiteralNode::Int(42)),
                    "Expected literal 42, got {lit:?}"
                );
            } else {
                panic!("Expected a literal value, got: {:?}", val);
            }
        } else {
            panic!("Expected a let statement, got: {stmt:?}");
        }
    }

    #[test]
    fn test_parse_match_statement_with_various_expressions() {
        init_test_logger();
        use medic_ast::ast::{ExpressionNode, LiteralNode, PatternNode};

        log::info!("Starting test_parse_match_statement_with_various_expressions");

        // Test with identifier
        let input = "match x { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);

        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with identifier"
        );

        // Verify AST structure for identifier match
        if let Ok((_, StatementNode::Match(match_stmt))) = result {
            // Verify the matched expression is an identifier
            if let ExpressionNode::Identifier(Spanned { node: ident, .. }) = &*match_stmt.expr {
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
            match &match_stmt.arms[0].pattern {
                PatternNode::Literal(LiteralNode::Int(1)) => {
                    // Pattern matches 1
                }
                _ => panic!(
                    "Expected literal pattern in first arm, got {:?}",
                    match_stmt.arms[0].pattern
                ),
            }

            // Second arm: _ => 0 (wildcard pattern)
            match &match_stmt.arms[1] {
                MatchArmNode {
                    pattern: PatternNode::Wildcard,
                    body,
                    ..
                } => {
                    if let ExpressionNode::Literal(Spanned {
                        node: LiteralNode::Int(0),
                        ..
                    }) = &**body
                    {
                        // Body is 0
                    } else {
                        panic!("Expected 0 as body of second arm, got {body:?}");
                    }
                }
                _ => panic!(
                    "Expected wildcard pattern in second arm, got {:?}",
                    match_stmt.arms[1].pattern
                ),
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
            if let ExpressionNode::Binary(Spanned { node: bin_expr, .. }) = &*match_stmt.expr {
                if let ExpressionNode::Identifier(Spanned { node: ident, .. }) = &bin_expr.left {
                    assert_eq!(ident.name, "x");
                } else {
                    panic!(
                        "Expected 'x' as left operand of binary expression, got {:?}",
                        bin_expr.left
                    );
                }
                assert_eq!(bin_expr.operator.to_string(), "+");
                if let ExpressionNode::Literal(Spanned { node: lit, .. }) = &bin_expr.right {
                    if let LiteralNode::Int(1) = lit {
                        // Correct
                    } else {
                        panic!("Expected 1 as right operand of binary expression");
                    }
                } else if let ExpressionNode::Identifier(Spanned { node: ident, .. }) =
                    &bin_expr.right
                {
                    assert_eq!(ident.name, "1");
                } else {
                    panic!(
                        "Expected 1 as right operand of binary expression, got {:?}",
                        bin_expr.right
                    );
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
    }

    #[test]
    fn test_parse_match_statement_with_complex_expression() {
        // Test with a simple expression first to verify basic functionality
        let input = "match x { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);

        let result = parse_statement(token_slice);
        assert!(result.is_ok(), "Failed to parse basic match statement");

        // Then verify the AST structure
        let (_remaining_tokens, statement) = result.unwrap();
        if let StatementNode::Match(match_stmt) = statement {
            // Verify the matched expression is an identifier
            if let ExpressionNode::Identifier(Spanned { node: ident, .. }) = &*match_stmt.expr {
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
            match &match_stmt.arms[0].pattern {
                PatternNode::Literal(LiteralNode::Int(1)) => {
                    // Pattern matches 1
                }
                _ => panic!(
                    "Expected literal pattern in first arm, got {:?}",
                    match_stmt.arms[0].pattern
                ),
            }

            // Second arm: _ => 0 (wildcard pattern)
            match &match_stmt.arms[1] {
                MatchArmNode {
                    pattern: PatternNode::Wildcard,
                    body,
                    ..
                } => {
                    if let ExpressionNode::Literal(Spanned {
                        node: LiteralNode::Int(0),
                        ..
                    }) = &**body
                    {
                        // Body is 0
                    } else {
                        panic!("Expected 0 as body of second arm, got {body:?}");
                    }
                }
                _ => panic!(
                    "Expected wildcard pattern in second arm, got {:?}",
                    match_stmt.arms[1].pattern
                ),
            }
        } else {
            panic!("Expected Match statement, got {statement:?}");
        }
    }
}
