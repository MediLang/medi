//! Test utilities for the parser

// This module is only used for testing purposes
#![allow(dead_code)]

use medic_lexer::string_interner::InternedString;
use medic_lexer::token::{Location, Token, TokenType};

/// Creates a token with default location for testing
pub fn create_token(token_type: TokenType, lexeme: &str) -> Token {
    Token::new(
        token_type,
        lexeme,
        Location {
            line: 1,
            column: 1,
            offset: 0,
        },
    )
}

/// Creates an identifier token for testing
pub fn create_identifier(name: &str) -> Token {
    create_token(TokenType::Identifier(InternedString::from(name)), name)
}

/// Creates a number token for testing
pub fn create_number(value: i64) -> Token {
    let s = value.to_string();
    create_token(TokenType::Integer(value), &s)
}

/// Creates an operator token for testing
pub fn create_operator(op: &str) -> Token {
    let token_type = match op {
        "+" => TokenType::Plus,
        "-" => TokenType::Minus,
        "*" => TokenType::Star,
        "/" => TokenType::Slash,
        "%" => TokenType::Percent,
        "==" => TokenType::EqualEqual,
        "!=" => TokenType::NotEqual,
        "<" => TokenType::Less,
        "<=" => TokenType::LessEqual,
        ">" => TokenType::Greater,
        ">=" => TokenType::GreaterEqual,
        "&&" => TokenType::BitAnd,
        "||" => TokenType::BitOr,
        "!" => TokenType::Not,
        "=" => TokenType::Equal,
        "(" => TokenType::LeftParen,
        ")" => TokenType::RightParen,
        "{" => TokenType::LeftBrace,
        "}" => TokenType::RightBrace,
        ";" => TokenType::Semicolon,
        "." => TokenType::Dot,
        "," => TokenType::Comma,
        _ => panic!("Unknown operator: {}", op),
    };
    create_token(token_type, op)
}

/// Tokenizes a string for testing
pub fn tokenize(input: &str) -> Vec<Token> {
    use medic_lexer::Lexer;
    Lexer::new(input).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_create_token() {
        use medic_lexer::string_interner::InternedString;
        let token = create_token(TokenType::Plus, "+");
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.lexeme, InternedString::from("+"));
    }

    #[test]
    fn test_tokenize() {
        let tokens = tokenize("1 + 2");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::Integer(1));
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Integer(2));
    }
}
