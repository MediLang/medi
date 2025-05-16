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
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("match")]
    Match,

    // --- Numbers ---
    #[regex(r"[0-9]+", |lex| lex.slice().parse().unwrap_or(0))]
    Integer(i64),
    #[regex(r"[0-9]+\\.[0-9]+", |lex| lex.slice().parse().unwrap_or(0.0))]
    Float(f64),

    // --- Identifiers ---
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // --- Literals ---
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
    /// Full source text
    source: &'source str,
    /// Current line number (1-based)
    line: usize,
    /// Current column number (1-based)
    column: usize,
    /// Current byte offset in source
    offset: usize,
    /// Previous span end position
    prev_span_end: usize,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source text
    pub fn new(source: &'source str) -> Self {
        Self {
            logos_lexer: LogosToken::lexer(source),
            source,
            line: 1,
            column: 1,
            offset: 0,
            prev_span_end: 0,
        }
    }

    /// Convert a LogosToken to our semantic Token type
    /// Synchronize lexer position to match the given span from logos lexer
    fn sync_position_to(&mut self, span: &logos::Span) {
        let range = span.clone();
        let range_clone = range.clone();
        let text = &self.source[range_clone];

        // Update offset
        self.offset = range.end;

        // Update line and column
        for c in text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    /// Convert a LogosToken to our semantic Token type
    fn convert_token(
        &mut self,
        logos_token: LogosToken,
        lexeme: &str,
        span: &logos::Span,
    ) -> Token {
        // Sync position before creating token
        self.sync_position_to(span);

        let location = Location {
            line: self.line,
            column: self.column,
            offset: self.offset,
        };

        let token_type = match logos_token {
            // Healthcare-specific keywords
            LogosToken::Patient => TokenType::Patient,
            LogosToken::Observation => TokenType::Observation,
            LogosToken::Medication => TokenType::Medication,
            LogosToken::FhirQuery => TokenType::FhirQuery,
            LogosToken::Regulate => TokenType::Regulate,
            LogosToken::Scope => TokenType::Scope,
            LogosToken::Federated => TokenType::Federated,
            LogosToken::Safe => TokenType::Safe,
            LogosToken::RealTime => TokenType::RealTime,

            // Medical codes
            LogosToken::IcdCode(code) => TokenType::ICD10(code),
            LogosToken::CptCode(code) => TokenType::CPT(code),
            LogosToken::SnomedCode(code) => TokenType::SNOMED(code),

            // General keywords
            LogosToken::Let => TokenType::Let,
            LogosToken::Fn => TokenType::Fn,
            LogosToken::If => TokenType::If,
            LogosToken::Else => TokenType::Else,
            LogosToken::For => TokenType::For,
            LogosToken::While => TokenType::While,
            LogosToken::Return => TokenType::Return,
            LogosToken::Match => TokenType::Match,
            LogosToken::Identifier(name) => TokenType::Identifier(name),
            LogosToken::Integer(num) => TokenType::Integer(num),
            LogosToken::Float(num) => TokenType::Float(num),

            // Literals
            LogosToken::True => TokenType::Boolean(true),
            LogosToken::False => TokenType::Boolean(false),
            LogosToken::Null => TokenType::Null,
            LogosToken::NoneVal => TokenType::None,
            LogosToken::SingleQuotedString(s) => TokenType::String(s),
            LogosToken::DoubleQuotedString(s) => TokenType::String(s),
            LogosToken::RawString(s) => TokenType::String(s),
            LogosToken::ByteString(s) => TokenType::String(s),
            LogosToken::TripleQuotedString(s) => TokenType::String(s),

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
            LogosToken::AndAnd => TokenType::And,
            LogosToken::OrOr => TokenType::Or,
            LogosToken::Shl => TokenType::Shl,
            LogosToken::Shr => TokenType::Shr,
            LogosToken::Arrow => TokenType::Arrow,
            LogosToken::DoubleStar => TokenType::DoubleStar,
            LogosToken::DoubleSlash => TokenType::DoubleSlash,
            LogosToken::PlusAssign => TokenType::PlusAssign,
            LogosToken::MinusAssign => TokenType::MinusAssign,
            LogosToken::StarAssign => TokenType::StarAssign,
            LogosToken::SlashAssign => TokenType::SlashAssign,
            LogosToken::DoubleStarAssign => TokenType::DoubleStarAssign,
            LogosToken::DoubleSlashAssign => TokenType::DoubleSlashAssign,
            LogosToken::PercentAssign => TokenType::PercentAssign,
            LogosToken::BitAnd => TokenType::BitAnd,
            LogosToken::BitOr => TokenType::BitOr,
            LogosToken::BitXor => TokenType::BitXor,
            LogosToken::BitNot => TokenType::BitNot,
            LogosToken::Bang => TokenType::Not,
            LogosToken::Dot => TokenType::Dot,
            LogosToken::At => TokenType::At,

            // Delimiters
            LogosToken::LParen => TokenType::LeftParen,
            LogosToken::RParen => TokenType::RightParen,
            LogosToken::LBrace => TokenType::LeftBrace,
            LogosToken::RBrace => TokenType::RightBrace,
            LogosToken::LBracket => TokenType::LeftBracket,
            LogosToken::RBracket => TokenType::RightBracket,
            LogosToken::Comma => TokenType::Comma,
            LogosToken::Colon => TokenType::Colon,
            LogosToken::DoubleColon => TokenType::ColonColon,
            LogosToken::Semicolon => TokenType::Semicolon,
            LogosToken::Dollar => TokenType::Dollar,
            LogosToken::Backslash => TokenType::Backslash,
            LogosToken::Underscore => TokenType::Identifier("_".to_string()),

            // Error case
            LogosToken::Error => TokenType::Error(lexeme.to_string()),
        };

        Token::new(token_type, lexeme.to_string(), location)
    }

    /// Update line and column numbers based on the span from logos lexer
    fn update_position(&mut self) {
        // Get the current span from the logos lexer
        let span = self.logos_lexer.span();

        // Get the text between the previous span end and the current span end
        // This includes any skipped whitespace and comments
        let text_to_process = &self.source[self.prev_span_end..span.start];

        // Update line and column for all characters including skipped ones
        for c in text_to_process.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.offset += c.len_utf8();
        }
        self.prev_span_end = span.end;
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.update_position();
        let logos_token = self.logos_lexer.next()?.unwrap();
        let lexeme = self.logos_lexer.slice();
        let span = self.logos_lexer.span();
        let token = self.convert_token(logos_token, lexeme, &span);
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
        assert!(matches!(token.token_type, TokenType::Patient));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Dot));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::FhirQuery));

        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::ICD10(ref s) if s == "ICD10:A01.1"));
    }
}
