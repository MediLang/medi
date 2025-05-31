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
        // Test with identifier
        let input = "match x { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with identifier"
        );

        // Test with literal
        let input = "match 42 { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with literal"
        );

        // Test with binary expression
        let input = "match x + 1 { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with binary expression"
        );

        // Test with function call
        let input = "match some_function() { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with function call"
        );

        // Test with member expression
        let input = "match some_object.property { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with member expression"
        );
    }

    #[test]
    fn test_parse_match_statement_with_complex_expression() {
        // Test with a complex expression
        let input = "match (x + y) * some_function(42, z) { 1 => 1, _ => 0 }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_statement(token_slice);
        assert!(
            result.is_ok(),
            "Failed to parse match statement with complex expression"
        );
    }
}
