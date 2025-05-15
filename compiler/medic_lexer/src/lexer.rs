//! Lexer for Medi language using the industry-standard 'logos' crate
//! Recognizes healthcare keywords, identifiers, literals, operators, and delimiters

use crate::token::{Location, Token, TokenType};
use logos::Logos;

/// Raw token type used by the logos lexer
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum LogosToken {
    // --- Healthcare-specific keywords ---
    #[token("patient")]
    Patient,
    #[token("observation")]
    Observation,
    #[token("medication")]
    Medication,
    #[token("fhir_query")]
    FhirQuery,
    #[token("kaplan_meier")]
    KaplanMeier,
    #[token("regulate")]
    Regulate,
    #[token("report")]
    Report,

    // --- Medical Codes ---
    #[regex(r"ICD10:[A-Z][0-9][0-9AB](\.[0-9A-Z]{1,4})?", |lex| lex.slice().to_string())]
    IcdCode(String),
    #[regex(r"CPT:[0-9]{5}", |lex| lex.slice().to_string())]
    CptCode(String),
    #[regex(r"SNOMED:[0-9]{6,9}", |lex| lex.slice().to_string())]
    SnomedCode(String),

    // --- General keywords ---
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("match")]
    Match,
    #[token("case")]
    Case,
    #[token("yield")]
    Yield,
    #[token("await")]
    Await,
    #[token("async")]
    Async,
    #[token("in")]
    In,
    #[token("not")]
    Not,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("is")]
    Is,
    #[token("as")]
    As,
    #[token("from")]
    From,
    #[token("import")]
    Import,
    #[token("with")]
    With,
    #[token("try")]
    Try,
    #[token("except")]
    Except,
    #[token("raise")]
    Raise,
    #[token("finally")]
    Finally,
    #[token("pass")]
    Pass,
    #[token("del")]
    Del,
    #[token("global")]
    Global,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("impl")]
    Impl,
    #[token("trait")]
    Trait,
    #[token("type")]
    Type,
    #[token("mod")]
    Mod,
    #[token("pub")]
    Pub,
    #[token("export")]
    Export,

    // --- Identifiers ---
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    // --- Literals ---
    #[regex(r"0b[01_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 2).ok())]
    BinLiteral(i64),
    #[regex(r"0o[0-7_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 8).ok())]
    OctLiteral(i64),
    #[regex(r"0x[0-9a-fA-F_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 16).ok())]
    HexLiteral(i64),
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().replace('_', "").parse().ok())]
    IntLiteral(i64),
    #[regex(r"[0-9][0-9_]*\.[0-9_]+([eE][+-]?[0-9_]+)?", |lex| lex.slice().replace('_', "").parse().ok())]
    FloatLiteral(f64),
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice().to_string())]
    SingleQuotedString(String),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    DoubleQuotedString(String),
    #[regex(r#"r"[^"]*""#, |lex| lex.slice().to_string())]
    RawString(String),
    #[regex(r#"b"[^"]*""#, |lex| lex.slice().to_string())]
    ByteString(String),
    #[regex(r#"'''[^']*'''"#, |lex| lex.slice().to_string())]
    #[regex(r#"""[^"]*"""#, |lex| lex.slice().to_string())]
    TripleQuotedString(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,
    #[token("None")]
    NoneVal,

    // --- Operators (multi-char first, Rust best practice) ---
    #[token("**=")]
    DoubleStarAssign,
    #[token("//=")]
    DoubleSlashAssign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("**")]
    DoubleStar,
    #[token("//")]
    DoubleSlash,
    #[token("==")]
    EqEq,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("->")]
    Arrow,
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
    #[token("=")]
    Assign,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!")]
    Bang,
    #[token("&")]
    BitAnd,
    #[token("|")]
    BitOr,
    #[token("^")]
    BitXor,
    #[token("~")]
    BitNot,

    // --- Delimiters ---
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("@")]
    At,
    #[token("$")]
    Dollar,
    #[token("\\")]
    Backslash,
    #[token("_")]
    Underscore,

    // --- Whitespace and comments (skipped) ---
    #[regex(r"[ \t\n\r]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip, priority = 2)]
    #[regex(r"#.*", logos::skip)]
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    // --- Error ---
    Error,
}

/// Medi language lexer
pub struct Lexer<'source> {
    /// The logos lexer instance
    logos_lexer: logos::Lexer<'source, LogosToken>,
    /// Current line number (1-based)
    line: usize,
    /// Current column number (1-based)
    column: usize,
    /// Current byte offset in source
    offset: usize,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source text
    pub fn new(source: &'source str) -> Self {
        Self {
            logos_lexer: LogosToken::lexer(source),
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    /// Convert a LogosToken to our semantic Token type
    fn convert_token(&self, logos_token: LogosToken, lexeme: &str) -> Token {
        let location = Location {
            line: self.line,
            column: self.column,
            offset: self.offset,
        };

        let token_type = match logos_token {
            // Healthcare keywords
            LogosToken::Patient => TokenType::Identifier("patient".to_string()),
            LogosToken::Observation => TokenType::Identifier("observation".to_string()),
            LogosToken::Medication => TokenType::Identifier("medication".to_string()),
            LogosToken::FhirQuery => TokenType::Fhir,
            LogosToken::Regulate => TokenType::Regulate,

            // Medical codes
            LogosToken::IcdCode(code) => TokenType::ICD10(code),
            LogosToken::SnomedCode(code) => TokenType::SNOMED(code),

            // Keywords
            LogosToken::Let => TokenType::Let,
            LogosToken::Fn => TokenType::Fn,
            LogosToken::Const => TokenType::Const,
            LogosToken::Type => TokenType::Type,
            LogosToken::Struct => TokenType::Struct,
            LogosToken::Enum => TokenType::Enum,
            LogosToken::Impl => TokenType::Impl,
            LogosToken::Trait => TokenType::Trait,
            LogosToken::Mod => TokenType::Module,
            LogosToken::Pub => TokenType::Pub,
            LogosToken::Return => TokenType::Return,
            LogosToken::While => TokenType::While,
            LogosToken::For => TokenType::For,
            LogosToken::In => TokenType::In,
            LogosToken::If => TokenType::Identifier("if".to_string()),
            LogosToken::Else => TokenType::Identifier("else".to_string()),
            LogosToken::Match => TokenType::Match,

            // Literals
            LogosToken::IntLiteral(n) => TokenType::Integer(n),
            LogosToken::FloatLiteral(n) => TokenType::Float(n),
            LogosToken::SingleQuotedString(s) | LogosToken::DoubleQuotedString(s) => {
                TokenType::String(s)
            }
            LogosToken::True => TokenType::Boolean(true),
            LogosToken::False => TokenType::Boolean(false),

            // Operators
            LogosToken::Plus => TokenType::Plus,
            LogosToken::Minus => TokenType::Minus,
            LogosToken::Star => TokenType::Star,
            LogosToken::Slash => TokenType::Slash,
            LogosToken::Percent => TokenType::Percent,
            LogosToken::Assign => TokenType::Equal,
            LogosToken::EqEq => TokenType::EqualEqual,
            LogosToken::Neq => TokenType::NotEqual,
            LogosToken::Lt => TokenType::Less,
            LogosToken::Gt => TokenType::Greater,
            LogosToken::Le => TokenType::LessEqual,
            LogosToken::Ge => TokenType::GreaterEqual,
            LogosToken::Arrow => TokenType::Arrow,

            // Delimiters
            LogosToken::Dot => TokenType::Dot,
            LogosToken::Comma => TokenType::Comma,
            LogosToken::Colon => TokenType::Colon,
            LogosToken::Semicolon => TokenType::Semicolon,
            LogosToken::LParen => TokenType::LeftParen,
            LogosToken::RParen => TokenType::RightParen,
            LogosToken::LBracket => TokenType::LeftBracket,
            LogosToken::RBracket => TokenType::RightBracket,
            LogosToken::LBrace => TokenType::LeftBrace,
            LogosToken::RBrace => TokenType::RightBrace,

            // Other
            LogosToken::Identifier => TokenType::Identifier(lexeme.to_string()),
            LogosToken::Error => {
                TokenType::Error(format!("Invalid token at {}:{}", self.line, self.column))
            }
            _ => TokenType::Error(format!(
                "Unhandled token type at {}:{}",
                self.line, self.column
            )),
        };

        Token::new(token_type, lexeme.to_string(), location)
    }

    /// Update line and column numbers based on lexeme
    fn update_position(&mut self, lexeme: &str) {
        for c in lexeme.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.offset += c.len_utf8();
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let logos_token = self.logos_lexer.next()?;
        let lexeme = self.logos_lexer.slice();
        let token = match logos_token {
            Ok(token) => self.convert_token(token, lexeme),
            Err(_) => Token::new(
                TokenType::Error(format!("Invalid token at {}:{}", self.line, self.column)),
                lexeme.to_string(),
                Location {
                    line: self.line,
                    column: self.column,
                    offset: self.offset,
                },
            ),
        };
        self.update_position(lexeme);
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let source = "let x = 42";
        let mut lexer = Lexer::new(source);

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Let));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Identifier(ref s) if s == "x"));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Equal));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Integer(42)));
    }

    #[test]
    fn test_lexer_healthcare() {
        let source = "patient.fhir_query ICD10:A01.1";
        let mut lexer = Lexer::new(source);

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Identifier(ref s) if s == "patient"));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Dot));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Fhir));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::ICD10(ref s) if s == "ICD10:A01.1"));
    }
}
