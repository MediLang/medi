use logos::Logos;

use crate::chunked_lexer::Position;

/// Token types used by the Logos lexer.
///
/// This enum defines all the possible tokens that can be produced by the lexer,
/// including keywords, literals, operators, and punctuation. Each variant corresponds
/// to a specific token type with an associated regex pattern or exact string match.
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = Position)]
pub enum LogosToken {
    // Range operators (higher priority to match before float literals)
    /// Inclusive range operator (`..=`)
    #[token("..=", priority = 1000)]
    RangeInclusive,
    /// Range operator (`..`)
    ///
    /// This has higher priority than float literals to ensure that `1..10` is tokenized as `1`, `..`, `10`
    /// rather than `1.`, `.10`
    ///
    /// Note: We use a lower priority than the inclusive range operator (`..=`) to ensure that
    /// `1..=10` is tokenized as `1`, `..=`, `10` rather than `1`, `..`, `=`, `10`
    #[token("..", priority = 40)] // Higher than both Integer and Float
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
    ///
    /// This has a higher priority than the float regex to ensure that integers followed by a dot
    /// are tokenized as separate tokens (e.g., `42.` becomes `[Integer(42), Dot]`).
    #[token(".", priority = 150)]
    Dot,

    // Note: We can't use negative lookahead in the regex pattern because it's not supported by the logos crate
    // Instead, we'll handle this case in the token processing logic
    // This is a placeholder variant that won't be used directly
    #[doc(hidden)]
    IntegerWithTrailingDot(i64),

    /// Positive integer literal (e.g., `42`)
    ///
    /// Matches one or more digits that are not part of a negative number.
    /// The lexer will validate that the number is not followed by invalid characters (like letters).
    #[regex(r"[0-9]+", |lex| {
        use log::debug;
        let slice = lex.slice();

        // Don't match if this is part of a negative number
        if lex.span().start > 0 {
            let prev_char = &lex.source()[lex.span().start - 1..lex.span().start];
            if prev_char == "-" {
                debug!("POSITIVE INTEGER PARSING - Skipping negative number part: {slice}");
                return None;
            }
        }

        debug!("POSITIVE INTEGER PARSING - Raw slice: '{slice}' at span {:?}", lex.span());

        // Check if the number is followed by an identifier character (letter or underscore)
        // which would make it an invalid numeric literal
        if let Some(next_char) = lex.remainder().chars().next() {
            if next_char.is_alphabetic() || next_char == '_' {
                debug!("POSITIVE INTEGER PARSING - Invalid numeric literal: {slice} followed by identifier character");
                return None;
            }

            // Check if this is a range operator (.. or ..=)
            let next_next_char = lex.remainder().chars().nth(1);

            if next_next_char == Some('.') || next_next_char == Some('=') {
                // It's a range operator, so this is an integer
                debug!("POSITIVE INTEGER PARSING - Integer before range operator: {slice}");
                return slice.parse().ok();
            }


            // If the next character is a dot, this is an integer followed by a dot
            // We'll return the integer and let the dot be matched as a separate token
            if next_char == '.' {
                debug!("POSITIVE INTEGER PARSING - Integer with trailing dot: {slice}");
                return slice.parse().ok();
            }
        }

        // For any other case, it's a valid positive integer
        debug!("POSITIVE INTEGER PARSING - Parsing positive integer: {slice}");
        match slice.parse::<i64>() {
            Ok(n) => {
                debug!("POSITIVE INTEGER PARSING - Successfully parsed: {n}");
                Some(n)
            },
            Err(e) => {
                debug!("POSITIVE INTEGER PARSING - Failed to parse '{slice}' as i64: {e:?}");
                None
            }
        }
    }, priority = 100)]
    Integer(i64),

    /// Negative integer literal (e.g., `-42`)
    ///
    /// Matches a minus sign followed by one or more digits.
    /// The lexer will validate that the number is not followed by invalid characters.
    ///
    /// This has higher priority than the Integer variant to ensure negative numbers
    /// are properly tokenized as a single token.
    #[regex(r"-[0-9]+", |lex| {
        use log::debug;
        let slice = lex.slice();
        let span = lex.span();

        debug!("NEGATIVE INTEGER PARSING - Raw slice: '{slice}' at span {span:?}");
        debug!("NEGATIVE INTEGER PARSING - Full source: '{:?}'", lex.source());
        debug!("NEGATIVE INTEGER PARSING - Remainder: '{:?}'", lex.remainder());

        // Parse the entire string as i64 (including the minus sign)
        match slice.parse::<i64>() {
            Ok(value) => {
                debug!("NEGATIVE INTEGER PARSING - Successfully parsed value: {value}");
                debug!("NEGATIVE INTEGER PARSING - Bytes: {:?}", slice.as_bytes());

                // Return the parsed value (should be negative)
                Some(value)
            },
            Err(e) => {
                debug!("NEGATIVE INTEGER PARSING - Failed to parse '{slice}' as i64: {e:?}");
                debug!("NEGATIVE INTEGER PARSING - Bytes that failed to parse: {:?}", slice.as_bytes());
                None
            }
        }
    }, priority = 200)]
    // Higher priority than Integer to ensure negative numbers are matched first
    NegativeInteger(i64),

    /// Floating-point literal with optional exponent
    ///
    /// Matches:
    /// 1. Numbers with decimal point and fractional part (e.g., `3.14`)
    /// 2. Numbers with leading decimal (e.g., `.5`)
    /// 3. Scientific notation (e.g., `1e10`, `2.5e-3`)
    ///
    /// This does NOT match:
    /// - Numbers followed by a dot that's part of a range operator (e.g., `1..`)
    /// - Integers followed by a dot (e.g., `42.` should be tokenized as `42` and `.`)
    /// - Just a single dot (e.g., `.` should be a Dot token)
    /// - Numbers with a trailing dot (e.g., `42.` should be tokenized as `42` and `.`)
    #[regex(r"-?(?:(?:[0-9]+\.[0-9]+(?:[eE][+-]?[0-9]+)?)|(?:\.[0-9]+(?:[eE][+-]?[0-9]+)?)|(?:[0-9]+[eE][+-]?[0-9]+))", |lex| {
        let slice = lex.slice();

        // For negative numbers, ensure the minus is followed by a digit or decimal point
        if let Some(rest) = slice.strip_prefix('-') {
            if rest.is_empty() {
                return Err(());
            }
            let next_char = rest.chars().next().unwrap();
            if !next_char.is_ascii_digit() && next_char != '.' {
                return Err(());
            }
            // If we have a negative number, make sure it's not being split
            if !rest.starts_with(|c: char| c.is_ascii_digit() || c == '.') {
                return Err(());
            }
        }

        // Don't match if this is just a dot (e.g., ".")
        if slice == "." {
            return Err(());
        }

        // Parse the float value
        slice.parse().map_err(|_| ())
    }, priority = 20)] // Lower priority than Integer to prefer integers when possible
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

    // Function-like medical literals (must come before Identifier to avoid shadowing)
    /// Patient ID function-like literal: pid("...")
    #[regex(r#"pid\(\s*\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\"\s*\)"#, |lex| {
        let s = lex.slice();
        // Extract the inner quoted string
        if let (Some(start), Some(end)) = (s.find('"'), s.rfind('"')) {
            if end > start { return Ok::<_, ()>(s[start+1..end].replace("\\\"", "\"")); }
        }
        Err(())
    })]
    PatientIdFunc(String),

    /// ICD-10 function-like literal: icd10("A00.0")
    #[regex(r#"icd10\(\s*\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\"\s*\)"#, |lex| {
        let s = lex.slice();
        if let (Some(start), Some(end)) = (s.find('"'), s.rfind('"')) {
            if end > start { return Ok::<_, ()>(s[start+1..end].replace("\\\"", "\"")); }
        }
        Err(())
    })]
    ICD10Func(String),

    // Medical operators (must come before Identifier to avoid shadowing)
    /// `of` operator (e.g., '2 of 3' criteria)
    #[token("of")]
    Of,
    /// `per` operator (e.g., 'mg per day')
    #[token("per")]
    Per,

    /// Underscore (`_`) - used for wildcard patterns
    /// This must come before the Identifier pattern to ensure it takes precedence
    #[token("_", priority = 100)]
    Underscore,

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
    ///
    /// This is used for the subtraction operator. Negative numbers are handled by the Integer token.
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

    /// Fat arrow (`=>`)
    #[token("=>")]
    FatArrow,
    // Underscore token moved to before the Identifier pattern for proper precedence

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
