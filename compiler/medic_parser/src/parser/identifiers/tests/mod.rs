use super::super::*;
use crate::parser::TokenSlice;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

/// Converts a string into a `TokenSlice` and its corresponding vector of tokens by lexing the input.
///
/// Returns a tuple containing a `TokenSlice` referencing the tokens and the owned vector of tokens.
///
/// # Examples
///
/// ```
/// let (slice, tokens) = str_to_token_slice("foo");
/// assert_eq!(tokens.len(), 1);
/// ```
fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let tokens_static = Box::new(tokens.clone());
    let tokens_ref = Box::leak(tokens_static);
    (TokenSlice(tokens_ref), tokens)
}

#[cfg(test)]
mod identifiers_test {
    use super::*;
    use medic_ast::ast::{ExpressionNode, IdentifierNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_identifier() {
        let (input, _) = str_to_token_slice("x");
        let (_, expr) = parse_identifier(input).unwrap();
        
        assert_eq!(
            expr,
            ExpressionNode::Identifier(IdentifierNode { name: "x".to_string() })
        );
    }
    
    // Add more test cases for identifiers and member expressions
}
