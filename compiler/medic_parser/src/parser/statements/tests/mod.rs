use super::super::*;
use crate::parser::TokenSlice;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

// Helper function to convert a string to a TokenSlice
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
}
