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
    /// `module` keyword - declares a module
    Module,
    /// `import` keyword - imports items from another module
    Import,
    /// `fn` keyword - declares a function
    Fn,
    /// `let` keyword - declares a mutable variable
    Let,
    /// `const` keyword - declares a compile-time constant
    Const,
    /// `type` keyword - declares a type alias
    Type,
    /// `struct` keyword - declares a structure
    Struct,
    /// `enum` keyword - declares an enumeration
    Enum,
    /// `trait` keyword - declares a trait
    Trait,
    /// `impl` keyword - implements functionality for a type
    Impl,
    /// `pub` keyword - marks an item as public
    Pub,
    /// `priv` keyword - marks an item as private
    Priv,
    /// `return` keyword - returns a value from a function
    Return,
    /// `while` keyword - starts a while loop
    While,
    /// `for` keyword - starts a for loop
    For,
    /// `in` keyword - used in for loops and pattern matching
    In,
    /// `match` keyword - starts a match expression
    Match,
    /// `if` keyword - starts an if expression
    If,
    /// `else` keyword - starts an else clause
    Else,
    /// `loop` keyword - starts an infinite loop
    Loop,
    /// `break` keyword - exits a loop
    Break,
    /// `continue` keyword - continues to the next iteration of a loop
    Continue,
    /// `true` boolean literal
    True,
    /// `false` boolean literal
    False,
    /// `nil` literal - represents a null or empty value
    Nil,

    // Operators
    /// `+` operator - addition or unary plus
    Plus,
    /// `+=` operator - addition assignment
    PlusEqual,
    /// `-` operator - subtraction or unary minus
    Minus,
    /// `-=` operator - subtraction assignment
    MinusEqual,
    /// `*` operator - multiplication
    Star,
    /// `*=` operator - multiplication assignment
    StarEqual,
    /// `/` operator - division
    Slash,
    /// `/=` operator - division assignment
    SlashEqual,
    /// `%` operator - remainder
    Percent,
    /// `%=` operator - remainder assignment
    PercentEqual,
    /// `=` operator - assignment
    Equal,
    /// `==` operator - equality comparison
    EqualEqual,
    /// `!` operator - logical NOT
    Not,
    /// `!=` operator - inequality comparison
    NotEqual,
    /// `<` operator - less than
    Less,
    /// `<=` operator - less than or equal to
    LessEqual,
    /// `>` operator - greater than
    Greater,
    /// `>=` operator - greater than or equal to
    GreaterEqual,
    /// `&&` operator - logical AND
    AndAnd,
    /// `||` operator - logical OR
    OrOr,
    /// `&` operator - bitwise AND
    BitAnd,
    /// `&=` operator - bitwise AND assignment
    BitAndAssign,
    /// `|` operator - bitwise OR
    BitOr,
    /// `|=` operator - bitwise OR assignment
    BitOrAssign,
    /// `^` operator - bitwise XOR
    BitXor,
    /// `^=` operator - bitwise XOR assignment
    BitXorAssign,
    /// `!` operator - bitwise NOT
    BitNot,
    /// `<<` operator - left shift
    Shl,
    /// `<<=` operator - left shift assignment
    ShlAssign,
    /// `>>` operator - right shift
    Shr,
    /// `>>=` operator - right shift assignment
    ShrAssign,
    /// `**` operator - exponentiation
    DoubleStar,
    /// `**=` operator - exponentiation assignment
    DoubleStarAssign,
    /// `of` operator - used in type expressions
    Of,
    /// `per` operator - used in unit expressions
    Per,

    // Punctuation
    /// `→` - unit conversion operator (Unicode U+2192)
    UnitConversionArrow,
    /// `.` operator - member access
    Dot,
    /// `..` operator - range (exclusive)
    DotDot,
    /// `...` operator - range (inclusive, deprecated)
    DotDotDot,
    /// `..=` operator - range (inclusive)
    DotDotEq,
    /// `,` - separates items
    Comma,
    /// `:` - type annotation
    Colon,
    /// `::` - path separator
    ColonColon,
    /// `;` - statement terminator
    Semicolon,
    /// `(` - left parenthesis
    LeftParen,
    /// `)` - right parenthesis
    RightParen,
    /// `{` - left brace
    LeftBrace,
    /// `}` - right brace
    RightBrace,
    /// `[` - left bracket
    LeftBracket,
    /// `]` - right bracket
    RightBracket,
    /// `->` - function return type
    Arrow,
    /// `=>` - match arm separator
    FatArrow,
    /// `_` - wildcard pattern
    Underscore,
    /// `@` - pattern binding
    At,
    /// `#` - attribute
    Pound,
    /// `$` - macro metavariable
    Dollar,
    /// `?` - error propagation
    Question,
    /// `??` - nullish coalescing
    QuestionQuestion,
    /// `?:` - elvis operator
    QuestionColon,
    /// `!` - macro or never type
    Bang,
    /// `..` - range pattern
    Range,
    /// `..=` - inclusive range pattern
    RangeInclusive,

    // Healthcare-specific tokens
    /// `fhir_query` - FHIR query expression
    FhirQuery,
    /// `query` - general query expression
    Query,
    /// `regulate` - regulation expression
    Regulate,
    /// `scope` - scope definition
    Scope,
    /// `federated` - federated query
    Federated,

    /// Partial token that spans multiple chunks
    PartialToken,
    /// `safe` - safe call operator
    Safe,
    /// `realtime` - real-time data access
    RealTime,
    /// `patient` - patient context
    Patient,
    /// `observation` - clinical observation
    Observation,
    /// `medication` - medication reference
    Medication,

    // Literals and identifiers
    /// Patient ID literal (e.g., pid("PT-12345"))
    PatientId(InternedString),
    /// ICD-10 code literal
    ICD10(InternedString),
    /// LOINC code literal
    LOINC(InternedString),
    /// SNOMED CT code literal
    SNOMED(InternedString),
    /// CPT code literal
    CPT(InternedString),
    /// Identifier (variable/function name)
    Identifier(InternedString),
    /// String literal
    String(InternedString),
    /// Character literal
    Char(char),
    /// Byte literal
    Byte(u8),
    /// Byte string literal
    ByteString(InternedString),
    /// Integer literal (e.g., `42`)
    Integer(i64),
    /// Negative integer literal (e.g., `-42`)
    NegativeInteger(i64),
    /// Floating-point literal
    Float(f64),
    /// Boolean literal
    Boolean(bool),
    /// Error token with message
    Error(InternedString),
    /// Comment
    Comment(InternedString),
    /// Lexer error
    LexerError,
    /// Whitespace (usually discarded)
    Whitespace,
    /// I've (contraction, used in natural language processing)
    Ive,
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
            (TokenType::AndAnd, TokenType::AndAnd) => true,
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
            (TokenType::UnitConversionArrow, TokenType::UnitConversionArrow) => true,
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
            (TokenType::PatientId(a), TokenType::PatientId(b)) => a == b,
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
            (TokenType::NegativeInteger(a), TokenType::NegativeInteger(b)) => a == b,
            (TokenType::Float(a), TokenType::Float(b)) => a == b,
            (TokenType::Boolean(a), TokenType::Boolean(b)) => a == b,
            (TokenType::Error(a), TokenType::Error(b)) => a == b,
            (TokenType::Comment(a), TokenType::Comment(b)) => a == b,
            (TokenType::LexerError, TokenType::LexerError) => true,
            (TokenType::Whitespace, TokenType::Whitespace) => true,
            (TokenType::PartialToken, TokenType::PartialToken) => true,

            _ => false,
        }
    }
}

impl Eq for TokenType {}

impl Hash for TokenType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            TokenType::PatientId(s)
            | TokenType::ICD10(s)
            | TokenType::LOINC(s)
            | TokenType::SNOMED(s)
            | TokenType::CPT(s)
            | TokenType::Identifier(s)
            | TokenType::String(s)
            | TokenType::ByteString(s)
            | TokenType::Error(s)
            | TokenType::Comment(s) => {
                s.hash(state);
            }
            TokenType::Char(c) => c.hash(state),
            TokenType::Byte(b) => b.hash(state),
            TokenType::Integer(n) => n.hash(state),
            TokenType::NegativeInteger(n) => n.hash(state),
            TokenType::Float(f) => f.to_bits().hash(state),
            TokenType::Boolean(b) => b.hash(state),
            _ => {} // All other variants have no associated data
        }
    }
}

/// Represents a token in the source code, including its type, lexeme, and location.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn new<S: Into<InternedString>>(
        token_type: TokenType,
        lexeme: S,
        location: Location,
    ) -> Self {
        Self {
            token_type,
            lexeme: lexeme.into(),
            location,
        }
    }

    /// Creates a new token from an already-interned string.
    /// This is more efficient than `new` when you already have an InternedString.
    pub fn from_interned(
        token_type: TokenType,
        lexeme: InternedString,
        location: Location,
    ) -> Self {
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
                | TokenType::PatientId(_)
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
