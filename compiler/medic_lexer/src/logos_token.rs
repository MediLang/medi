use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum LogosToken {
    // Keywords
    #[token("module")]
    Module,
    #[token("import")]
    Import,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("type")]
    Type,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("pub")]
    Pub,
    #[token("priv")]
    Priv,
    #[token("return")]
    Return,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("match")]
    Match,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    // Literals
    #[regex(r"[0-9]+", |lex| {
        lex.slice().parse::<i64>().map_err(|_| ())
    })]
    Integer(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| {
        lex.slice().parse::<f64>().map_err(|_| ())
    })]
    Float(f64),
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#, |lex| {
        let s = lex.slice();
        // Remove the surrounding quotes and unescape the string
        Result::<_, ()>::Ok(s[1..s.len()-1].replace("\\\"", "\""))
    })]
    String(String),
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

    // Identifiers - support Unicode characters
    // First character: Any Unicode letter or underscore
    // Following characters: Any Unicode letter, number, or underscore
    // Note: Using a simpler pattern that should match our test case
    #[regex(r"[\p{L}_][\p{L}0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Medical codes (including the prefix in the token)
    #[regex(r"ICD10:[A-Z]\d{2}(?:\.\d{1,2})?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    ICD10(String),

    #[regex(r"LOINC:[0-9]+(?:-[0-9]+)?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    LOINC(String),

    #[regex(r"SNOMED:[0-9]+", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    SNOMED(String),

    #[regex(r"CPT:[0-9]{4,5}(?:[A-Z])?(?:-[0-9A-Z]+)?", |lex| {
        let s = lex.slice();
        Ok::<_, ()>(s.to_string())
    })]
    CPT(String),

    // Dot token
    #[token(".")]
    Dot,

    // Healthcare keywords
    #[token("patient")]
    Patient,
    #[token("observation")]
    Observation,
    #[token("medication")]
    Medication,
    #[token("fhir_query")]
    FhirQuery,
    #[token("query")]
    Query,
    #[token("regulate")]
    Regulate,
    #[token("scope")]
    Scope,
    #[token("federated")]
    Federated,
    #[token("safe")]
    Safe,
    #[token("real_time")]
    RealTime,

    // Error token (with lower priority)
    Error,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("=")]
    Equal,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("%=")]
    PercentEqual,
    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEqual,

    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("->")]
    Arrow,
    #[token("|")]
    Pipe,
    #[token("_", priority = 1)]
    Underscore,

    // Whitespace and comments (skipped)
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,
    #[regex(r"//[^\n]*", logos::skip)]
    LineComment,
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    BlockComment,
}
