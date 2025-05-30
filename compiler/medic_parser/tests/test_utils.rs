//! Test utilities for the parser tests

use medic_lexer::string_interner::InternedString;
use medic_lexer::token::{Location, Token, TokenType};

/// Creates a new location with default values
fn default_location() -> Location {
    Location {
        line: 1,
        column: 1,
        offset: 0,
    }
}

/// Creates a new token with the given type and lexeme
fn create_token(token_type: TokenType, lexeme: &'static str) -> Token {
    Token::new(token_type, lexeme, default_location())
}

/// Creates a new identifier token with the given name
///
/// # Examples
/// ```
/// let token = create_identifier_token("x");
/// assert!(matches!(token.token_type, TokenType::Identifier(_)));
/// ```
pub fn create_identifier_token(name: &'static str) -> Token {
    create_token(TokenType::Identifier(InternedString::from(name)), name)
}

/// Creates a new dot token
///
/// # Examples
/// ```
/// let token = create_dot_token();
/// assert!(matches!(token.token_type, TokenType::Dot));
/// ```
pub fn create_dot_token() -> Token {
    create_token(TokenType::Dot, ".")
}

/// Creates a new equals token
///
/// # Examples
/// ```
/// let token = create_equals_token();
/// assert!(matches!(token.token_type, TokenType::Equal));
/// ```
pub fn create_equals_token() -> Token {
    create_token(TokenType::Equal, "=")
}

/// Creates a new semicolon token
///
/// # Examples
/// ```
/// let token = create_semicolon_token();
/// assert!(matches!(token.token_type, TokenType::Semicolon));
/// ```
pub fn create_semicolon_token() -> Token {
    create_token(TokenType::Semicolon, ";")
}

/// Creates a new integer token with the given value
///
/// # Examples
/// ```
/// let token = create_integer_token(42);
/// assert!(matches!(token.token_type, TokenType::Integer(42)));
/// ```
pub fn create_integer_token(value: i64) -> Token {
    let lexeme = value.to_string();
    let interned = InternedString::from(lexeme.as_str());
    Token::from_interned(TokenType::Integer(value), interned, default_location())
}

/// Creates a new plus token
///
/// # Examples
/// ```
/// let token = create_plus_token();
/// assert!(matches!(token.token_type, TokenType::Plus));
/// ```
pub fn create_plus_token() -> Token {
    create_token(TokenType::Plus, "+")
}

/// Creates a new left brace token
///
/// # Examples
/// ```
/// let token = create_left_brace_token();
/// assert!(matches!(token.token_type, TokenType::LeftBrace));
/// ```
pub fn create_left_brace_token() -> Token {
    create_token(TokenType::LeftBrace, "{")
}

/// Creates a new right brace token
///
/// # Examples
/// ```
/// let token = create_right_brace_token();
/// assert!(matches!(token.token_type, TokenType::RightBrace));
/// ```
pub fn create_right_brace_token() -> Token {
    create_token(TokenType::RightBrace, "}")
}

/// Creates a new operator token (generic version)
///
/// # Arguments
/// * `op` - The operator token type
/// * `lexeme` - The lexeme string representation
///
/// # Examples
/// ```
/// let token = create_operator_token(TokenType::Plus, "+");
/// assert!(matches!(token.token_type, TokenType::Plus));
/// ```
pub fn create_operator_token(op: TokenType, lexeme: &'static str) -> Token {
    create_token(op, lexeme)
}

/// Creates a new literal token (generic version)
///
/// # Arguments
/// * `token_type` - The token type
/// * `lexeme` - The lexeme string representation
///
/// # Examples
/// ```
/// let token = create_literal_token(TokenType::Integer(42), "42");
/// assert!(matches!(token.token_type, TokenType::Integer(42)));
/// ```
pub fn create_literal_token(token_type: TokenType, lexeme: &'static str) -> Token {
    create_token(token_type, lexeme)
}

/// Creates a new punctuation token (generic version)
///
/// # Arguments
/// * `token_type` - The token type
/// * `lexeme` - The lexeme string representation
///
/// # Examples
/// ```
/// let token = create_punctuation_token(TokenType::Comma, ",");
/// assert!(matches!(token.token_type, TokenType::Comma));
/// ```
pub fn create_punctuation_token(token_type: TokenType, lexeme: &'static str) -> Token {
    create_token(token_type, lexeme)
}
