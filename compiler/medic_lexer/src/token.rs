use std::fmt;
use std::hash::{Hash, Hasher};

use crate::string_interner::InternedString;

/// Represents a token's location in the source code.
///
/// This struct tracks the position of a token in the source text, including
/// line and column numbers (1-based) and the byte offset (0-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Location {
    /// The 1-based line number in the source file
    pub line: usize,
    /// The 1-based column number in the source file
    pub column: usize,
    /// The 0-based byte offset from the start of the source
    pub offset: usize,
}

/// Represents the type of a token in the Medi programming language.
///
/// This enum includes all possible token types that can be produced by the lexer,
/// including keywords, literals, operators, and punctuation.
#[derive(Debug, Clone)]
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
    If,
    Else,
    Loop,
    Break,
    Continue,
    True,
    False,
    Nil,
    
    // Operators
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    EqualEqual,
    Not,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    AndAnd,
    Or,
    OrOr,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    BitNot,
    Shl,
    ShlAssign,
    Shr,
    ShrAssign,
    DoubleStar,
    DoubleStarAssign,
    Of,
    Per,
    
    // Punctuation
    Dot,
    DotDot,
    DotDotDot,
    DotDotEq,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Arrow,
    FatArrow,
    Underscore,
    At,
    Pound,
    Dollar,
    Question,
    QuestionQuestion,
    QuestionColon,
    Bang,
    Pipe,
    Range,
    RangeInclusive,
    
    // Healthcare-specific tokens
    FhirQuery,
    Query,
    Regulate,
    Scope,
    Federated,
    Safe,
    RealTime,
    Patient,
    Observation,
    Medication,
    
    // Literals and identifiers
    ICD10(InternedString),
    LOINC(InternedString),
    SNOMED(InternedString),
    CPT(InternedString),
    Identifier(InternedString),
    String(InternedString),
    Char(char),
    Byte(u8),
    ByteString(InternedString),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Error(InternedString),
    Comment(InternedString),
    LexerError,
    Whitespace,ive 
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Keywords
            (TokenType::Module, TokenType::Module) => true,
            (TokenType::Import, TokenType::Import) => true,
            (TokenType::Fn, TokenType::Fn) => true,
            (TokenType::Let, TokenType::Let) => true,
            (TokenType::Const, TokenType::Const) => true,
            (TokenType::Type, TokenType::Type) => true,
            (TokenType::Struct, TokenType::Struct) => true,
            (TokenType::Enum, TokenType::Enum) => true,
            (TokenType::Trait, TokenType::Trait) => true,
            (TokenType::Impl, TokenType::Impl) => true,
            (TokenType::Pub, TokenType::Pub) => true,
            (TokenType::Priv, TokenType::Priv) => true,
            (TokenType::Return, TokenType::Return) => true,
            (TokenType::While, TokenType::While) => true,
            (TokenType::For, TokenType::For) => true,
            (TokenType::In, TokenType::In) => true,
            (TokenType::Match, TokenType::Match) => true,
            (TokenType::If, TokenType::If) => true,
            (TokenType::Else, TokenType::Else) => true,
            (TokenType::Loop, TokenType::Loop) => true,
            (TokenType::Break, TokenType::Break) => true,
            (TokenType::Continue, TokenType::Continue) => true,
            (TokenType::True, TokenType::True) => true,
            (TokenType::False, TokenType::False) => true,
            (TokenType::Nil, TokenType::Nil) => true,
            
            // Operators
            (TokenType::Plus, TokenType::Plus) => true,
            (TokenType::PlusEqual, TokenType::PlusEqual) => true,
            (TokenType::Minus, TokenType::Minus) => true,
            (TokenType::MinusEqual, TokenType::MinusEqual) => true,
            (TokenType::Star, TokenType::Star) => true,
            (TokenType::StarEqual, TokenType::StarEqual) => true,
            (TokenType::Slash, TokenType::Slash) => true,
            (TokenType::SlashEqual, TokenType::SlashEqual) => true,
            (TokenType::Percent, TokenType::Percent) => true,
            (TokenType::PercentEqual, TokenType::PercentEqual) => true,
            (TokenType::Equal, TokenType::Equal) => true,
            (TokenType::EqualEqual, TokenType::EqualEqual) => true,
            (TokenType::Not, TokenType::Not) => true,
            (TokenType::NotEqual, TokenType::NotEqual) => true,
            (TokenType::Less, TokenType::Less) => true,
            (TokenType::LessEqual, TokenType::LessEqual) => true,
            (TokenType::Greater, TokenType::Greater) => true,
            (TokenType::GreaterEqual, TokenType::GreaterEqual) => true,
            (TokenType::And, TokenType::And) => true,
            (TokenType::AndAnd, TokenType::AndAnd) => true,
            (TokenType::Or, TokenType::Or) => true,
            (TokenType::OrOr, TokenType::OrOr) => true,
            (TokenType::BitAnd, TokenType::BitAnd) => true,
            (TokenType::BitAndAssign, TokenType::BitAndAssign) => true,
            (TokenType::BitOr, TokenType::BitOr) => true,
            (TokenType::BitOrAssign, TokenType::BitOrAssign) => true,
            (TokenType::BitXor, TokenType::BitXor) => true,
            (TokenType::BitXorAssign, TokenType::BitXorAssign) => true,
            (TokenType::BitNot, TokenType::BitNot) => true,
            (TokenType::Shl, TokenType::Shl) => true,
            (TokenType::ShlAssign, TokenType::ShlAssign) => true,
            (TokenType::Shr, TokenType::Shr) => true,
            (TokenType::ShrAssign, TokenType::ShrAssign) => true,
            (TokenType::DoubleStar, TokenType::DoubleStar) => true,
            (TokenType::DoubleStarAssign, TokenType::DoubleStarAssign) => true,
            (TokenType::Of, TokenType::Of) => true,
            (TokenType::Per, TokenType::Per) => true,
            
            // Punctuation
            (TokenType::Dot, TokenType::Dot) => true,
            (TokenType::DotDot, TokenType::DotDot) => true,
            (TokenType::DotDotDot, TokenType::DotDotDot) => true,
            (TokenType::DotDotEq, TokenType::DotDotEq) => true,
            (TokenType::Comma, TokenType::Comma) => true,
            (TokenType::Colon, TokenType::Colon) => true,
            (TokenType::ColonColon, TokenType::ColonColon) => true,
            (TokenType::Semicolon, TokenType::Semicolon) => true,
            (TokenType::LeftParen, TokenType::LeftParen) => true,
            (TokenType::RightParen, TokenType::RightParen) => true,
            (TokenType::LeftBrace, TokenType::LeftBrace) => true,
            (TokenType::RightBrace, TokenType::RightBrace) => true,
            (TokenType::LeftBracket, TokenType::LeftBracket) => true,
            (TokenType::RightBracket, TokenType::RightBracket) => true,
            (TokenType::Arrow, TokenType::Arrow) => true,
            (TokenType::FatArrow, TokenType::FatArrow) => true,
            (TokenType::Underscore, TokenType::Underscore) => true,
            (TokenType::At, TokenType::At) => true,
            (TokenType::Pound, TokenType::Pound) => true,
            (TokenType::Dollar, TokenType::Dollar) => true,
            (TokenType::Question, TokenType::Question) => true,
            (TokenType::QuestionQuestion, TokenType::QuestionQuestion) => true,
            (TokenType::QuestionColon, TokenType::QuestionColon) => true,
            (TokenType::Bang, TokenType::Bang) => true,
            (TokenType::Pipe, TokenType::Pipe) => true,
            (TokenType::Range, TokenType::Range) => true,
            (TokenType::RangeInclusive, TokenType::RangeInclusive) => true,
            
            // Healthcare-specific tokens
            (TokenType::FhirQuery, TokenType::FhirQuery) => true,
            (TokenType::Query, TokenType::Query) => true,
            (TokenType::Regulate, TokenType::Regulate) => true,
            (TokenType::Scope, TokenType::Scope) => true,
            (TokenType::Federated, TokenType::Federated) => true,
            (TokenType::Safe, TokenType::Safe) => true,
            (TokenType::RealTime, TokenType::RealTime) => true,
            (TokenType::Patient, TokenType::Patient) => true,
            (TokenType::Observation, TokenType::Observation) => true,
            (TokenType::Medication, TokenType::Medication) => true,
            
            // Literals and identifiers with data
            (TokenType::ICD10(a), TokenType::ICD10(b)) => a == b,
            (TokenType::LOINC(a), TokenType::LOINC(b)) => a == b,
            (TokenType::SNOMED(a), TokenType::SNOMED(b)) => a == b,
            (TokenType::CPT(a), TokenType::CPT(b)) => a == b,
            (TokenType::Identifier(a), TokenType::Identifier(b)) => a == b,
            (TokenType::String(a), TokenType::String(b)) => a == b,
            (TokenType::Char(a), TokenType::Char(b)) => a == b,
            (TokenType::Byte(a), TokenType::Byte(b)) => a == b,
            (TokenType::ByteString(a), TokenType::ByteString(b)) => a == b,
            (TokenType::Integer(a), TokenType::Integer(b)) => a == b,
            (TokenType::Float(a), TokenType::Float(b)) => a == b,
            (TokenType::Boolean(a), TokenType::Boolean(b)) => a == b,
            (TokenType::Error(a), TokenType::Error(b)) => a == b,
            (TokenType::Comment(a), TokenType::Comment(b)) => a == b,
            (TokenType::LexerError, TokenType::LexerError) => true,
            (TokenType::Whitespace, TokenType::Whitespace) => true,
            
            _ => false,
        }
    }
}

impl Eq for TokenType {}

impl Hash for TokenType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            TokenType::ICD10(s) | TokenType::LOINC(s) | TokenType::SNOMED(s) | TokenType::CPT(s) |
            TokenType::Identifier(s) | TokenType::String(s) | TokenType::ByteString(s) | 
            TokenType::Error(s) | TokenType::Comment(s) => {
                s.hash(state);
            }
            TokenType::Char(c) => c.hash(state),
            TokenType::Byte(b) => b.hash(state),
            TokenType::Integer(n) => n.hash(state),
            TokenType::Float(f) => f.to_bits().hash(state),
            TokenType::Boolean(b) => b.hash(state),
            _ => {} // All other variants have no associated data
        }
    }
}

/// Represents a token in the source code, including its type, lexeme, and location.
#[derive(Debug, Clone)]
pub struct Token {
    /// The type of the token
    pub token_type: TokenType,
    /// The original source text of the token
    pub lexeme: InternedString,
    /// The location of the token in the source code
    pub location: Location,
}

impl Token {
    /// Creates a new token from a string that can be converted to an InternedString.
    /// This will intern the string if it's not already interned.
    pub fn new<S: Into<InternedString>>(token_type: TokenType, lexeme: S, location: Location) -> Self {
        Self {
            token_type,
            lexeme: lexeme.into(),
            location,
        }
    }

    /// Creates a new token from an already-interned string.
    /// This is more efficient than `new` when you already have an InternedString.
    pub fn from_interned(token_type: TokenType, lexeme: InternedString, location: Location) -> Self {
        Self {
            token_type,
            lexeme,
            location,
        }
    }

    /// Returns true if this token is a keyword
    pub fn is_keyword(&self) -> bool {
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
                | TokenType::Return
                | TokenType::While
                | TokenType::For
                | TokenType::In
                | TokenType::Match
                | TokenType::If
                | TokenType::Else
                | TokenType::Loop
                | TokenType::Break
                | TokenType::Continue
                | TokenType::True
                | TokenType::False
                | TokenType::Nil
        )
    }

    /// Returns true if this token is a healthcare-specific token
    pub fn is_healthcare_token(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::FhirQuery
                | TokenType::Query
                | TokenType::Regulate
                | TokenType::Scope
                | TokenType::Federated
                | TokenType::Safe
                | TokenType::RealTime
                | TokenType::Patient
                | TokenType::Observation
                | TokenType::Medication
                | TokenType::ICD10(_)
                | TokenType::LOINC(_)
                | TokenType::SNOMED(_)
                | TokenType::CPT(_)
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{}", self.token_type, self.location)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.line, self.column, self.offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_creation() {
        let location = Location {
            line: 1,
            column: 1,
            offset: 0,
        };
        let token = Token::new(TokenType::Let, "let", location);
        assert_eq!(token.token_type, TokenType::Let);
        assert_eq!(token.lexeme, "let");
        assert_eq!(token.location, location);
    }

    #[test]
    fn test_is_keyword() {
        let location = Location::default();
        let keyword_token = Token::new(TokenType::Let, "let", location);
        let non_keyword_token = Token::new(TokenType::Identifier("foo".into()), "foo", location);

        assert!(keyword_token.is_keyword());
        assert!(!non_keyword_token.is_keyword());
    }

    #[test]
    fn test_is_healthcare_token() {
        let location = Location::default();
        let healthcare_token = Token::new(TokenType::Patient, "Patient", location);
        let non_healthcare_token = Token::new(TokenType::Let, "let", location);

        assert!(healthcare_token.is_healthcare_token());
        assert!(!non_healthcare_token.is_healthcare_token());
    }
}
