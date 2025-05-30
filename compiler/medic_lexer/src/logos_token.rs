use logos::Logos;

/// Token types used by the Logos lexer.
///
/// This enum defines all the possible tokens that can be produced by the lexer,
/// including keywords, literals, operators, and punctuation. Each variant corresponds
/// to a specific token type with an associated regex pattern or exact string match.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum LogosToken {
    // Range operators (higher priority to match before float literals)
    /// Inclusive range operator (`..=`)
    #[token("..=", priority = 1000)]
    RangeInclusive,
    /// Range operator (`..`)
    #[token("..", priority = 999)]
    Range,

    // Keywords
    /// `module` keyword
    #[token("module")]
    Module,
    /// `import` keyword
    #[token("import")]
    Import,
    /// `fn` keyword
    #[token("fn")]
    Fn,
    /// `let` keyword
    #[token("let")]
    Let,
    /// `const` keyword
    #[token("const")]
    Const,
    /// `type` keyword
    #[token("type")]
    Type,
    /// `struct` keyword
    #[token("struct")]
    Struct,
    /// `enum` keyword
    #[token("enum")]
    Enum,
    /// `trait` keyword
    #[token("trait")]
    Trait,
    /// `impl` keyword
    #[token("impl")]
    Impl,
    /// `pub` visibility modifier
    #[token("pub")]
    Pub,
    /// `priv` visibility modifier
    #[token("priv")]
    Priv,
    /// `return` keyword
    #[token("return")]
    Return,
    /// `while` loop keyword
    #[token("while")]
    While,
    /// `for` loop keyword
    #[token("for")]
    For,
    /// `in` keyword (used in for loops and patterns)
    #[token("in")]
    In,
    /// `match` expression keyword
    #[token("match")]
    Match,
    /// `if` conditional keyword
    #[token("if")]
    If,
    /// `else` conditional keyword
    #[token("else")]
    Else,

    /// Member access operator (`.`)
    #[token(".")]
    Dot,

    /// Integer literal (e.g., `42`, `-10`)
    ///
    /// Matches optional negative sign followed by one or more digits.
    /// We'll validate in the lexer that it's not followed by letters.
    #[regex(r"-?[0-9]+", |lex| {
        let slice = lex.slice();
        // Only parse as negative if the minus is immediately followed by a digit
        if !slice.starts_with('-') || (slice.len() > 1 && slice.chars().nth(1).unwrap().is_ascii_digit()) {
            slice.parse().ok()
        } else {
            None
        }
    }, priority = 10)]
    Integer(i64),

    /// Floating-point literal with optional exponent
    ///
    /// Matches:
    /// 1. Numbers with decimal point (e.g., `3.14`)
    /// 2. Numbers with leading decimal (e.g., `.5`)
    /// 3. Scientific notation (e.g., `1e10`, `2.5e-3`)
    /// 4. Negative versions of the above
    #[regex(r"-?(?:[0-9]+\.[0-9]+(?:[eE][+-]?[0-9]+)?|\.[0-9]+(?:[eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)", |lex| {
        let slice = lex.slice();
        // Only parse as negative if the minus is immediately followed by a digit or decimal point
        if slice.starts_with('-') && slice.len() > 1 && (slice.chars().nth(1).unwrap().is_ascii_digit() || slice.chars().nth(1).unwrap() == '.') {
            slice.parse().map_err(|_| ())
        } else if !slice.starts_with('-') {
            slice.parse().map_err(|_| ())
        } else {
            Err(())
        }
    }, priority = 10)]
    Float(f64),

    /// String literal with escaped quotes
    ///
    /// Matches text between double quotes, with support for escaped quotes.
    /// The stored value has the surrounding quotes removed and escape sequences processed.
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, |lex| {
        let s = lex.slice();
        // Remove the surrounding quotes and unescape the string
        Result::<_, ()>::Ok(s[1..s.len()-1].replace("\\\"", "\""))
    })]
    String(String),
    /// Boolean literal (`true` or `false`)
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

    // Medical operators (must come before Identifier to avoid shadowing)
    /// `of` operator (e.g., '2 of 3' criteria)
    #[token("of")]
    Of,
    /// `per` operator (e.g., 'mg per day')
    #[token("per")]
    Per,

    /// Identifier for variables, functions, and types
    ///
    /// Matches:
    /// - First character: Any Unicode letter or underscore
    /// - Following characters: Any Unicode letter, number, or underscore
    #[regex(r"[\p{L}_][\p{L}0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    /// ICD-10 medical code
    ///
    /// Format: `ICD10:` followed by an uppercase letter, two digits,
    /// and optionally a decimal point with 1-2 more digits.
    #[regex(r"ICD10:[A-Z]\d{2}(?:\.\d{1,2})?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    ICD10(String),

    /// LOINC medical code
    ///
    /// Format: `LOINC:` followed by numbers, optionally with a hyphen
    /// and more numbers (e.g., `LOINC:12345-6`).
    #[regex(r"LOINC:[0-9]+(?:-[0-9]+)?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    LOINC(String),

    /// SNOMED CT medical code
    ///
    /// Format: `SNOMED:` followed by numbers.
    #[regex(r"SNOMED:[0-9]+", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    SNOMED(String),

    /// CPT medical procedure code
    ///
    /// Format: `CPT:` followed by 4-5 digits, an optional letter,
    /// and an optional hyphen with more alphanumeric characters.
    #[regex(r"CPT:[0-9]{4,5}(?:[A-Z])?(?:-[0-9A-Z]+)?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    CPT(String),

    // Healthcare keywords
    /// `patient` keyword for patient data
    #[token("patient")]
    Patient,
    /// `observation` keyword for medical observations
    #[token("observation")]
    Observation,
    /// `medication` keyword for medication data
    #[token("medication")]
    Medication,

    // Medical keywords
    /// `fhir_query` keyword for FHIR queries
    #[token("fhir_query")]
    FhirQuery,
    /// `query` keyword for general queries
    #[token("query")]
    Query,
    /// `regulate` keyword for data regulation
    #[token("regulate")]
    Regulate,
    /// `scope` keyword for defining access scopes
    #[token("scope")]
    Scope,
    /// `federated` keyword for federated data sources
    #[token("federated")]
    Federated,
    /// `safe` keyword for safety annotations
    #[token("safe")]
    Safe,
    /// `real_time` keyword for real-time data access
    #[token("real_time")]
    RealTime,

    /// Error token for unrecognized input
    ///
    /// This token is produced when the lexer encounters input that doesn't match
    /// any of the defined token patterns.
    Error,

    // Operators - Grouped by precedence (lowest to highest)
    /// Logical OR operator (`||`)
    #[token("||")]
    Or,

    /// Logical NOT operator (`!`)
    #[token("!")]
    Not,

    /// Logical AND operator (`&&`)
    #[token("&&")]
    And,

    /// Nullish coalescing operator (`??`)
    #[token("??")]
    QuestionQuestion,

    /// Elvis operator (`?:`)
    #[token("?:")]
    QuestionColon,

    /// Equality operator (`==`)
    #[token("==")]
    EqualEqual,
    /// Inequality operator (`!=`)
    #[token("!=")]
    NotEqual,

    /// Less than operator (`<`)
    #[token("<")]
    Less,
    /// Less than or equal operator (`<=`)
    #[token("<=")]
    LessEqual,
    /// Greater than operator (`>`)
    #[token(">")]
    Greater,
    /// Greater than or equal operator (`>=`)
    #[token(">=")]
    GreaterEqual,

    /// Bitwise OR operator (`|`)
    /// Also used as pattern matching pipe
    #[token("|")]
    BitOr,

    /// Bitwise XOR operator (`^`)
    #[token("^")]
    BitXor,

    /// Bitwise AND operator (`&`)
    #[token("&")]
    BitAnd,

    /// Left shift operator (`<<`)
    #[token("<<")]
    Shl,
    /// Right shift operator (`>>`)
    #[token(">>")]
    Shr,

    // Addition/Subtraction
    /// Addition operator (`+`)
    #[token("+")]
    Plus,
    /// Subtraction operator (`-`)
    #[token("-")]
    Minus,

    // Multiplication/Division/Modulo
    /// Multiplication operator (`*`)
    #[token("*")]
    Star,
    /// Division operator (`/`)
    #[token("/")]
    Slash,
    /// Modulo operator (`%`)
    #[token("%")]
    Percent,

    // Exponentiation (right-associative)
    /// Exponentiation operator (`**`)
    #[token("**")]
    DoubleStar,

    // Assignment operators
    /// Assignment operator (`=`)
    #[token("=")]
    Equal,
    /// Addition assignment operator (`+=`)
    #[token("+=")]
    PlusEqual,
    /// Subtraction assignment operator (`-=`)
    #[token("-=")]
    MinusEqual,
    /// Multiplication assignment operator (`*=`)
    #[token("*=")]
    StarEqual,
    /// Division assignment operator (`/=`)
    #[token("/=")]
    SlashEqual,
    /// Modulo assignment operator (`%=`)
    #[token("%=")]
    PercentEqual,
    /// Exponentiation assignment operator (`**=`)
    #[token("**=")]
    DoubleStarAssign,
    /// Bitwise AND assignment operator (`&=`)
    #[token("&=")]
    BitAndAssign,
    /// Bitwise OR assignment operator (`|=`)
    #[token("|=")]
    BitOrAssign,
    /// Bitwise XOR assignment operator (`^=`)
    #[token("^=")]
    BitXorAssign,
    /// Left shift assignment operator (`<<=`)
    #[token("<<=")]
    ShlAssign,
    /// Right shift assignment operator (`>>=`)
    #[token(">>=")]
    ShrAssign,
    // Range operators are now defined at the top with higher priority

    // Delimiters
    /// Left parenthesis (`(`)
    #[token("(")]
    LeftParen,
    /// Right parenthesis (`)`)
    #[token(")")]
    RightParen,
    /// Left curly brace (`{`)
    #[token("{")]
    LeftBrace,
    /// Right curly brace (`}`)
    #[token("}")]
    RightBrace,
    /// Left square bracket (`[`)
    #[token("[")]
    LeftBracket,
    /// Right square bracket (`]`)
    #[token("]")]
    RightBracket,
    /// Comma (`,`)
    #[token(",")]
    Comma,
    /// Colon (`:`)
    #[token(":")]
    Colon,
    /// Semicolon (`;`)
    #[token(";")]
    Semicolon,
    /// Arrow (`->`)
    #[token("->")]
    Arrow,
    /// Underscore (`_`)
    #[token("_", priority = 1)]
    Underscore,

    // Whitespace and comments (skipped)
    /// Whitespace (spaces, tabs, newlines)
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,
    /// Single-line comment starting with `//`
    #[regex(r"//[^\n]*", logos::skip)]
    LineComment,
    /// Multi-line comment between `/*` and `*/`
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    BlockComment,
}
