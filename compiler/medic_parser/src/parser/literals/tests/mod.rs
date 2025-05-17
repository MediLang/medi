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
mod literals_test {
    use super::*;
    use medic_ast::ast::LiteralNode;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_integer_literal() {
        let (input, _) = str_to_token_slice("42");
        let (_, lit) = parse_literal(input).unwrap();
        
        assert_eq!(lit, LiteralNode::Int(42));
    }
    
    #[test]
    fn test_parse_float_literal() {
        let (input, _) = str_to_token_slice("3.14");
        let (_, lit) = parse_literal(input).unwrap();
        
        assert_eq!(lit, LiteralNode::Float(3.14));
    }
    
    #[test]
    fn test_parse_string_literal() {
        let (input, _) = str_to_token_slice("\"hello\"");
        let (_, lit) = parse_literal(input).unwrap();
        
        assert_eq!(lit, LiteralNode::String("hello".to_string()));
    }
    
    #[test]
    fn test_parse_boolean_literal() {
        let (input, _) = str_to_token_slice("true");
        let (_, lit) = parse_literal(input).unwrap();
        assert_eq!(lit, LiteralNode::Bool(true));
        
        let (input, _) = str_to_token_slice("false");
        let (_, lit) = parse_literal(input).unwrap();
        assert_eq!(lit, LiteralNode::Bool(false));
    }
}
