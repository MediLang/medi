//! Lexer implementation for the Medi language
//! Converts source code into a stream of tokens for the parser

use logos::Logos;
use std::collections::VecDeque;
use std::ops::Range;

use crate::string_interner::InternedString;
use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// The main lexer struct that holds the state of the lexing process
pub struct Lexer<'a> {
    /// The inner Logos lexer
    inner: logos::Lexer<'a, LogosToken>,

    /// The source code being lexed
    source: &'a str,

    /// Current byte offset in the source
    offset: usize,

    /// Current line number (1-based)
    line: u32,

    /// Current column number (1-based, in characters, not bytes)
    column: u32,

    /// Tokens that have been generated but not yet returned
    pending_tokens: VecDeque<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'a str) -> Self {
        let inner = LogosToken::lexer(source);
        Self {
            inner,
            source,
            line: 1,
            column: 1,
            offset: 0,
            pending_tokens: VecDeque::new(),
        }
    }

    /// Update the lexer's position based on the current span
    /// This handles multi-byte characters correctly by counting characters, not bytes
    fn sync_position_to(&mut self, span: &Range<usize>) {
        // The text between the last token and the current one
        let text = &self.source[self.offset..span.start];

        // Count newlines and update position
        for c in text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else if c == '\r' {
                // Handle Windows line endings
                continue;
            } else {
                self.column += 1;
            }
        }

        // Update the offset to the start of the current token
        self.offset = span.start;
    }

    /// Create a location from a span
    fn location_from_span(&self, span: &Range<usize>) -> Location {
        // For simplicity, we'll use the current line/column as a base
        // A more sophisticated implementation would track line/column for each span
        Location {
            line: self.line as usize,
            column: self.column as usize,
            offset: span.start,
        }
    }

    /// Converts a `LogosToken` and its lexeme into a semantic `Token`.
    ///
    /// Maps the given `LogosToken` variant and lexeme string to the corresponding `TokenType`,
    /// handling keywords, literals, operators, delimiters, medical codes, and healthcare-specific tokens.
    /// Also updates the lexer's position tracking to reflect the token's location in the source code.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::lexer::Lexer;
    /// use medic_lexer::LogosToken;
    /// use medic_lexer::token::TokenType;
    ///
    /// let mut lexer = Lexer::new("let x = 42");
    /// let logos_token = LogosToken::Let;
    /// let token = lexer.convert_token(logos_token, "let", &(0..3));
    /// assert_eq!(token.token_type, TokenType::Let);
    /// assert_eq!(token.lexeme, "let");
    /// ```
    pub fn convert_token(
        &mut self,
        logos_token: LogosToken,
        lexeme: &str,
        span: &Range<usize>,
    ) -> Token {
        // Update our position tracking
        self.sync_position_to(span);

        // Convert LogosToken to TokenType
        let token_type = match logos_token {
            // Keywords
            LogosToken::Module => TokenType::Module,
            LogosToken::Import => TokenType::Import,
            LogosToken::Fn => TokenType::Fn,
            LogosToken::Let => TokenType::Let,
            LogosToken::Const => TokenType::Const,
            LogosToken::Type => TokenType::Type,
            LogosToken::Struct => TokenType::Struct,
            LogosToken::Enum => TokenType::Enum,
            LogosToken::Trait => TokenType::Trait,
            LogosToken::Impl => TokenType::Impl,
            LogosToken::Pub => TokenType::Pub,
            LogosToken::Priv => TokenType::Priv,
            LogosToken::Return => TokenType::Return,
            LogosToken::While => TokenType::While,
            LogosToken::For => TokenType::For,
            LogosToken::In => TokenType::In,
            LogosToken::Match => TokenType::Match,
            LogosToken::If => TokenType::If,
            LogosToken::Else => TokenType::Else,

            // Literals
            LogosToken::Integer(i) => TokenType::Integer(i),
            LogosToken::IntegerWithTrailingDot(i) => TokenType::Integer(i), // Handle integer with trailing dot
            LogosToken::NegativeInteger(i) => TokenType::NegativeInteger(i),
            LogosToken::Float(f) => TokenType::Float(f),
            LogosToken::String(s) => TokenType::String(InternedString::from(&s[..])),
            LogosToken::Bool(b) => TokenType::Boolean(b), // Using Boolean variant for consistency
            LogosToken::Identifier(ident) => {
                TokenType::Identifier(InternedString::from(&ident[..]))
            }

            // Medical operators
            LogosToken::Of => TokenType::Of,
            LogosToken::Per => TokenType::Per,

            // Medical codes
            LogosToken::ICD10(code) => TokenType::ICD10(InternedString::from(&code[..])),
            LogosToken::LOINC(code) => TokenType::LOINC(InternedString::from(&code[..])),
            LogosToken::SNOMED(code) => TokenType::SNOMED(InternedString::from(&code[..])),

            LogosToken::CPT(code) => TokenType::CPT(InternedString::from(&code[..])),

            // Operators
            LogosToken::Plus => TokenType::Plus,
            LogosToken::Minus => TokenType::Minus,
            LogosToken::Star => TokenType::Star,
            LogosToken::Slash => TokenType::Slash,
            LogosToken::Percent => TokenType::Percent,
            LogosToken::EqualEqual => TokenType::EqualEqual,
            LogosToken::NotEqual => TokenType::NotEqual,
            LogosToken::Less => TokenType::Less,
            LogosToken::LessEqual => TokenType::LessEqual,
            LogosToken::Greater => TokenType::Greater,
            LogosToken::GreaterEqual => TokenType::GreaterEqual,
            LogosToken::And => TokenType::BitAnd,
            LogosToken::Or => TokenType::BitOr,
            LogosToken::Not => TokenType::Not,
            LogosToken::Equal => TokenType::Equal,
            LogosToken::PlusEqual => TokenType::PlusEqual,
            LogosToken::MinusEqual => TokenType::MinusEqual,
            LogosToken::StarEqual => TokenType::StarEqual,
            LogosToken::SlashEqual => TokenType::SlashEqual,
            LogosToken::PercentEqual => TokenType::PercentEqual,
            LogosToken::DoubleStar => TokenType::DoubleStar,
            LogosToken::DoubleStarAssign => TokenType::DoubleStarAssign,
            LogosToken::BitAnd => TokenType::BitAnd,
            LogosToken::BitAndAssign => TokenType::BitAndAssign,
            // Bitwise OR is used as pipe in patterns
            LogosToken::BitOr => TokenType::BitOr,
            LogosToken::BitOrAssign => TokenType::BitOrAssign,
            LogosToken::BitXor => TokenType::BitXor,
            LogosToken::BitXorAssign => TokenType::BitXorAssign,
            LogosToken::Shl => TokenType::Shl,
            LogosToken::ShlAssign => TokenType::ShlAssign,
            LogosToken::Shr => TokenType::Shr,
            LogosToken::ShrAssign => TokenType::ShrAssign,
            LogosToken::QuestionQuestion => TokenType::QuestionQuestion,
            LogosToken::QuestionColon => TokenType::QuestionColon,
            LogosToken::Range => TokenType::Range,
            LogosToken::RangeInclusive => TokenType::RangeInclusive,

            // Delimiters
            LogosToken::LeftParen => TokenType::LeftParen,
            LogosToken::RightParen => TokenType::RightParen,
            LogosToken::LeftBrace => TokenType::LeftBrace,
            LogosToken::RightBrace => TokenType::RightBrace,
            LogosToken::LeftBracket => TokenType::LeftBracket,
            LogosToken::RightBracket => TokenType::RightBracket,
            LogosToken::Comma => TokenType::Comma,
            LogosToken::Dot => TokenType::Dot,
            LogosToken::Colon => TokenType::Colon,
            LogosToken::Semicolon => TokenType::Semicolon,
            LogosToken::Arrow => TokenType::Arrow,
            LogosToken::Underscore => TokenType::Underscore,
            LogosToken::FatArrow => TokenType::FatArrow,

            // Error
            LogosToken::Error => TokenType::LexerError,

            // Handle healthcare keywords
            LogosToken::Patient => TokenType::Patient,
            LogosToken::Observation => TokenType::Observation,
            LogosToken::Medication => TokenType::Medication,
            LogosToken::FhirQuery => TokenType::FhirQuery,
            LogosToken::Query => TokenType::Query,
            LogosToken::Regulate => TokenType::Regulate,
            LogosToken::Scope => TokenType::Scope,
            LogosToken::Federated => TokenType::Federated,
            LogosToken::Safe => TokenType::Safe,
            LogosToken::RealTime => TokenType::RealTime,

            // Skip whitespace and comments
            LogosToken::Whitespace | LogosToken::LineComment | LogosToken::BlockComment => {
                TokenType::Whitespace
            }
        };

        // Create and return the token
        Token {
            token_type,
            lexeme: InternedString::from(lexeme),
            location: Location {
                line: self.line as usize,
                column: self.column as usize,
                offset: self.offset,
            },
        }
    }

    /// Get the next token from the input stream
    pub fn next_token(&mut self) -> Option<Token> {
        // If we have pending tokens, return them first
        if let Some(token) = self.pending_tokens.pop_front() {
            return Some(token);
        }

        // Get the next token from Logos
        let (logos_token, span) = loop {
            match self.inner.next()? {
                Ok(token) => {
                    // Skip whitespace and comments
                    if !matches!(
                        &token,
                        LogosToken::Whitespace | LogosToken::LineComment | LogosToken::BlockComment
                    ) {
                        break (token, self.inner.span());
                    }
                }
                Err(_) => {
                    let span = self.inner.span();
                    let lexeme = &self.source[span.clone()];
                    return Some(Token {
                        token_type: TokenType::LexerError,
                        lexeme: InternedString::from(lexeme),
                        location: self.location_from_span(&span),
                    });
                }
            }
        };

        let lexeme = &self.source[span.clone()];

        // Handle range operators after integer literals
        if let LogosToken::Integer(_) = logos_token {
            // Check if this is followed by a range operator
            let remaining = &self.source[span.end..];
            if remaining.starts_with("..") {
                // Check for inclusive range
                let (range_type, range_len) = if remaining.starts_with("..=") {
                    (TokenType::RangeInclusive, 3)
                } else {
                    (TokenType::Range, 2)
                };

                // Create range token
                let range_span = span.end..(span.end + range_len);
                let range_lexeme = &self.source[range_span.clone()];
                let range_token = Token {
                    token_type: range_type,
                    lexeme: InternedString::from(range_lexeme),
                    location: self.location_from_span(&range_span),
                };

                // Queue the range token for the next call
                self.pending_tokens.push_back(range_token);

                // Update the inner lexer's position by consuming the range operator
                // This is crucial to prevent infinite loops
                for _ in 0..range_len {
                    self.inner.bump(1);
                }
            }
        }

        // Convert and return the current token
        let token = self.convert_token(logos_token, lexeme, &span);
        Some(token)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

// Include the test module
#[cfg(test)]
mod tests;
