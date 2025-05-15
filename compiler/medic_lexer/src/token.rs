use std::fmt;

/// Represents a token's location in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

/// Represents the type of a token
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    Module,
    Import,
    Fn,
    Let,
    Const,
    Type,
    Struct,
    Enum,
    Trait,
    Impl,
    Pub,
    Priv,
    Return,
    While,
    For,
    In,
    Match,

    // Healthcare-specific keywords
    Fhir,
    Query,
    Regulate,
    Scope,
    Federated,
    Safe,
    RealTime,

    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    DateTime(String), // ISO 8601 format

    // Healthcare-specific literals
    PatientId(String),
    ICD10(String),
    LOINC(String),
    SNOMED(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,
    Pipe,
    Arrow,
    FatArrow,

    // Delimiters
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Dot,
    Comma,
    Colon,
    Semicolon,
    At,
    Hash,
    Question,
    DotDot,
    DotDotDot,

    // Identifiers and special tokens
    Identifier(String),
    Comment(String),
    DocComment(String),
    Whitespace,
    Newline,
    EOF,

    // Error token
    Error(String),
}

/// Represents a single token in the source code
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub location: Location,
}

impl Token {
    /// Constructs a new `Token` with the specified type, lexeme, and location.
    ///
    /// # Examples
    ///
    /// ```
    /// let location = Location { line: 1, column: 5, offset: 4 };
    /// let token = Token::new(TokenType::Let, "let".to_string(), location);
    /// assert_eq!(token.lexeme, "let");
    /// assert_eq!(token.location.line, 1);
    /// ```    pub fn new(token_type: TokenType, lexeme: String, location: Location) -> Self {
        Token {
            token_type,
            lexeme,
            location,
        }
    }

    /// Returns `true` if the token represents a language keyword, including healthcare-specific keywords.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    ///
    /// let token = Token::new(TokenType::Fn, "fn".to_string(), Location { line: 1, column: 1, offset: 0 });
    /// assert!(token.is_keyword());
    ///
    /// let token = Token::new(TokenType::Identifier, "my_var".to_string(), Location { line: 1, column: 4, offset: 3 });
    /// assert!(!token.is_keyword());
    /// ```    pub fn is_keyword(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Module
                | TokenType::Import
                | TokenType::Fn
                | TokenType::Let
                | TokenType::Const
                | TokenType::Type
                | TokenType::Struct
                | TokenType::Enum
                | TokenType::Trait
                | TokenType::Impl
                | TokenType::Pub
                | TokenType::Priv
                | TokenType::Fhir
                | TokenType::Query
                | TokenType::Regulate
                | TokenType::Scope
                | TokenType::Federated
                | TokenType::Safe
                | TokenType::RealTime
        )
    }

    /// Determines whether the token represents a healthcare-specific keyword or literal.
    ///
    /// Returns `true` if the token type is one of the healthcare-related keywords (`Fhir`, `Query`, `Regulate`) or healthcare-specific literals (`PatientId`, `ICD10`, `LOINC`, `SNOMED`); otherwise, returns `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// use your_crate::{Token, TokenType, Location};
    ///
    /// let token = Token::new(TokenType::Fhir, "fhir".to_string(), Location { line: 1, column: 1, offset: 0 });
    /// assert!(token.is_healthcare_token());
    ///
    /// let token = Token::new(TokenType::Let, "let".to_string(), Location { line: 1, column: 1, offset: 0 });
    /// assert!(!token.is_healthcare_token());
    /// ```    pub fn is_healthcare_token(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Fhir
                | TokenType::Query
                | TokenType::Regulate
                | TokenType::PatientId(_)
                | TokenType::ICD10(_)
                | TokenType::LOINC(_)
                | TokenType::SNOMED(_)
        )
    }
}

impl fmt::Display for Token {
    /// Formats the token as a string showing its type, lexeme, and source location.
    ///
    /// # Examples
    ///
    /// ```
    /// use your_crate::{Token, TokenType, Location};
    /// let token = Token::new(TokenType::Let, "let".to_string(), Location { line: 1, column: 5, offset: 4 });
    /// assert_eq!(format!("{}", token), "Token(Let 'let' at 1:5)");
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token({:?} '{}' at {}:{})",
            self.token_type, self.lexeme, self.location.line, self.location.column
        )
    }
}

impl fmt::Display for Location {
    /// Formats the location as "line:column".
    ///
    /// # Examples
    ///
    /// ```
    /// let loc = Location { line: 3, column: 15, offset: 42 };
    /// assert_eq!(format!("{}", loc), "3:15");
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_creation() {
        let loc = Location {
            line: 1,
            column: 1,
            offset: 0,
        };
        let token = Token::new(
            TokenType::Identifier("test".to_string()),
            "test".to_string(),
            loc,
        );
        assert_eq!(token.lexeme, "test");
        assert_eq!(token.location.line, 1);
    }

    #[test]
    fn test_is_keyword() {
        let loc = Location {
            line: 1,
            column: 1,
            offset: 0,
        };
        let token = Token::new(TokenType::Module, "module".to_string(), loc);
        assert!(token.is_keyword());

        let token = Token::new(
            TokenType::Identifier("test".to_string()),
            "test".to_string(),
            loc,
        );
        assert!(!token.is_keyword());
    }

    #[test]
    fn test_is_healthcare_token() {
        let loc = Location {
            line: 1,
            column: 1,
            offset: 0,
        };
        let token = Token::new(TokenType::Fhir, "fhir".to_string(), loc);
        assert!(token.is_healthcare_token());

        let token = Token::new(
            TokenType::PatientId("PT123".to_string()),
            "PT123".to_string(),
            loc,
        );
        assert!(token.is_healthcare_token());
    }
}
