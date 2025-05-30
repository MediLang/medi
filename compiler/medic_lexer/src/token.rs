use std::fmt;

use crate::string_interner::InternedString;

/// Represents a token's location in the source code.
///
/// This struct tracks the position of a token in the source text, including
/// line and column numbers (1-based) and the byte offset (0-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    /// `module` - Defines a module
    Module,
    /// `import` - Imports a module or item
    Import,
    /// `fn` - Defines a function
    Fn,
    /// `let` - Binds a value to a variable
    Let,
    /// `const` - Defines a constant
    Const,
    /// `type` - Defines a type alias
    Type,
    /// `struct` - Defines a structure
    Struct,
    /// `enum` - Defines an enumeration
    Enum,
    /// `trait` - Defines a trait
    Trait,
    /// `impl` - Implements functionality for a type
    Impl,
    /// `pub` - Makes an item public
    Pub,
    /// `priv` - Makes an item private (default)
    Priv,
    /// `return` - Returns a value from a function
    Return,
    /// `while` - Defines a while loop
    While,
    /// `for` - Defines a for loop
    For,
    /// `in` - Used in for loops and patterns
    In,
    /// `match` - Pattern matching
    Match,
    /// `if` - Conditional execution
    If,
    /// `else` - Alternative execution path
    Else,
    /// An unsupported or invalid keyword
    UnsupportedKeyword(InternedString),

    // Healthcare-specific keywords
    /// `fhir_query` - FHIR query operation
    FhirQuery,
    /// `query` - General query operation
    Query,
    /// `regulate` - Data regulation directive
    Regulate,
    /// `scope` - Defines data access scope
    Scope,
    /// `federated` - Federated data source
    Federated,
    /// `safe` - Safety qualifier
    Safe,
    /// `realtime` - Real-time data access
    RealTime,
    /// `Patient` - FHIR Patient resource
    Patient,
    /// `Observation` - FHIR Observation resource
    Observation,
    /// `Medication` - FHIR Medication resource
    Medication,

    // Medical operators
    /// `of` - Used in medical expressions (e.g., '2 of 3')
    Of,
    /// `per` - Used in medical expressions (e.g., 'mg per day')
    Per,

    // Literals
    /// Integer literal (e.g., `42`)
    Integer(i64),
    /// Floating-point literal (e.g., `3.14`)
    Float(f64),
    /// String literal (e.g., `"hello"`)
    String(InternedString),
    /// Boolean literal (`true` or `false`)
    Boolean(bool),
    /// Date/time literal in ISO 8601 format
    DateTime(InternedString),
    /// `null` literal
    Null,
    /// `none` literal (alternative to null)
    None,

    // Healthcare-specific literals
    /// Patient identifier (e.g., `PatientId("12345"))`
    PatientId(InternedString),
    /// ICD-10 code (e.g., `ICD10("E11.65")`)
    ICD10(InternedString),
    /// LOINC code (e.g., `LOINC("2160-0")`)
    LOINC(InternedString),
    /// SNOMED CT code (e.g., `SNOMED("44054006")`)
    SNOMED(InternedString),
    /// CPT code (e.g., `CPT("99213")`)
    CPT(InternedString),

    // Operators - Grouped by precedence (lowest to highest)
    /// Logical OR (`||`)
    Or,
    /// Logical AND (`&&`)
    And,
    /// Equality (`==`)
    EqualEqual,
    /// Inequality (`!=`)
    NotEqual,
    /// Less than (`<`)
    Less,
    /// Less than or equal (`<=`)
    LessEqual,
    /// Greater than (`>`)
    Greater,
    /// Greater than or equal (`>=`)
    GreaterEqual,
    /// Bitwise OR (`|`)
    BitOr,
    /// Bitwise XOR (`^`)
    BitXor,
    /// Bitwise AND (`&`)
    BitAnd,
    /// Left shift (`<<`)
    Shl,
    /// Right shift (`>>`)
    Shr,
    /// Addition (`+`)
    Plus,
    /// Subtraction (`-`)
    Minus,
    /// Multiplication (`*`)
    Star,
    /// Division (`/`)
    Slash,
    /// Modulo (`%`)
    Percent,
    /// Exponentiation (`**`)
    DoubleStar,
    /// Range (`..`)
    Range,
    /// Inclusive range (`..=`)
    RangeInclusive,
    /// Nullish coalescing (`??`)
    QuestionQuestion,
    /// Elvis operator (`?:`)
    QuestionColon,
    /// Assignment (`=`)
    Equal,
    /// Addition assignment (`+=`)
    PlusEqual,
    /// Subtraction assignment (`-=`)
    MinusEqual,
    /// Multiplication assignment (`*=`)
    StarEqual,
    /// Division assignment (`/=`)
    SlashEqual,
    /// Modulo assignment (`%=`)
    PercentEqual,
    /// Exponentiation assignment (`**=`)
    DoubleStarAssign,
    /// Integer division assignment (`//=`)
    DoubleSlashAssign,
    /// Bitwise AND assignment (`&=`)
    BitAndAssign,
    /// Bitwise OR assignment (`|=`)
    BitOrAssign,
    /// Bitwise XOR assignment (`^=`)
    BitXorAssign,
    /// Left shift assignment (`<<=`)
    ShlAssign,
    /// Right shift assignment (`>>=`)
    ShrAssign,
    /// Logical NOT (`!`)
    Not,
    /// Bitwise NOT (`~`)
    BitNot,
    /// Pattern match pipe (`|`)
    Pipe,
    /// Function/method return type (`->`)
    Arrow,
    /// Closure/pattern match arm (`=>`)
    FatArrow,
    /// Wildcard pattern (`_`)
    Underscore,

    // Literals
    // Note: Boolean literals are handled by the Boolean variant

    // Punctuation and delimiters
    /// Lexer error (contains error message)
    LexerError,
    /// Left brace (`{`)
    LeftBrace,
    /// Right brace (`}`)
    RightBrace,
    /// Left parenthesis (`(`)
    LeftParen,
    /// Right parenthesis (`)`)
    RightParen,
    /// Left bracket (`[`)
    LeftBracket,
    /// Right bracket (`]`)
    RightBracket,
    /// Dot (`.`)
    Dot,
    /// Comma (`,`)
    Comma,
    /// Colon (`:`)
    Colon,
    /// Double colon (`::`)
    ColonColon,
    /// Semicolon (`;`)
    Semicolon,
    /// At symbol (`@`)
    At,
    /// Dollar sign (`$`)
    Dollar,
    /// Backslash (`\`)
    Backslash,
    /// Double colon (`::` - alternative to ColonColon)
    DoubleColon,
    // Special tokens
    /// Identifier (variable/function/type name)
    Identifier(InternedString),
    /// Line or block comment
    Comment(InternedString),
    /// Documentation comment
    DocComment(InternedString),
    /// Whitespace (only included if configured)
    Whitespace,
    /// Newline (only included if configured)
    Newline,
    /// End of file marker
    EOF,
    // Error token
    /// Error token with message
    Error(InternedString),
}

/// Represents a single token in the source code along with its location and original lexeme.
///
/// A token is the smallest meaningful unit of code that the parser can understand.
/// It combines the token type with the actual text that was matched and its location
/// in the source code for error reporting and debugging purposes.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The type of the token (e.g., keyword, identifier, literal, etc.)
    pub token_type: TokenType,
    /// The original text that was matched to form this token
    pub lexeme: InternedString,
    /// The location in the source code where this token appears
    pub location: Location,
}

impl Token {
    /// Creates a new token from a string that can be converted to an InternedString.
    /// This will intern the string if it's not already interned.
    ///
    /// For cases where you already have an InternedString, use `from_interned` instead.
    pub fn new<S: Into<InternedString>>(
        token_type: TokenType,
        lexeme: S,
        location: Location,
    ) -> Self {
        Token::from_interned(token_type, lexeme.into(), location)
    }

    /// Creates a new token from an already-interned string.
    /// This is more efficient than `new` when you already have an InternedString.
    pub fn from_interned(
        token_type: TokenType,
        lexeme: InternedString,
        location: Location,
    ) -> Self {
        Token {
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
                | TokenType::FhirQuery
                | TokenType::Query
                | TokenType::Regulate
                | TokenType::Scope
                | TokenType::Federated
                | TokenType::Safe
                | TokenType::RealTime
                | TokenType::Patient
                | TokenType::Observation
                | TokenType::Medication
                | TokenType::UnsupportedKeyword(_)
        )
    }

    /// Returns true if this token is a healthcare-specific token (e.g., Patient, Observation, Medication, PatientId, ICD10, LOINC, SNOMED, CPT)
    pub fn is_healthcare_token(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::FhirQuery
                | TokenType::Query
                | TokenType::Regulate
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token({:?} '{}' at {}:{})",
            self.token_type, self.lexeme, self.location.line, self.location.column
        )
    }
}

impl fmt::Display for Location {
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
        let test_str = "test";
        let interned = InternedString::from(test_str);
        let token = Token::new(TokenType::Identifier(interned.clone()), interned, loc);
        assert_eq!(token.lexeme.as_str(), test_str);
        assert_eq!(token.location.line, 1);
    }

    #[test]
    fn test_is_keyword() {
        let loc = Location {
            line: 1,
            column: 1,
            offset: 0,
        };
        let token = Token::new(TokenType::Module, "module", loc);
        assert!(token.is_keyword());

        let token = Token::new(
            TokenType::Identifier(InternedString::from("test")),
            "test",
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
        let token = Token::new(TokenType::FhirQuery, "fhir", loc);
        assert!(token.is_healthcare_token());

        let token = Token::new(TokenType::PatientId("PT123".into()), "PT123", loc);
        assert!(token.is_healthcare_token());
    }
}
