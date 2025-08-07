use std::collections::VecDeque;

use logos::{Lexer as LogosLexer, Logos};

use crate::string_interner::InternedString;
use crate::token::{Token, TokenType};
use crate::LogosToken;

/// Configuration for the streaming lexer
#[derive(Debug, Clone, Copy)]
pub struct LexerConfig {
    /// Maximum number of tokens to buffer at once
    pub max_buffer_size: usize,
    /// Whether to include whitespace tokens in the output
    pub include_whitespace: bool,
    /// Whether to include comment tokens in the output
    pub include_comments: bool,
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self {
            // Further reduce default buffer size to minimize memory usage
            max_buffer_size: 16, // Reduced from 64 to 16
            include_whitespace: false,
            include_comments: false,
        }
    }
}

/// A streaming lexer that processes source code in chunks
pub struct StreamingLexer<'a> {
    /// The inner Logos lexer
    inner: LogosLexer<'a, LogosToken>,
    /// The source code being lexed
    source: &'a str,
    /// Configuration for the lexer
    config: LexerConfig,
    /// Buffer for tokens that have been lexed but not yet consumed
    buffer: VecDeque<Token>,
    /// Current position in the source
    position: Position,
    /// Whether we've reached the end of the input
    eof: bool,
}

/// Tracks the current position in the source code
#[derive(Debug, Clone, Copy, Default)]
struct Position {
    /// Current byte offset
    offset: usize,
    /// Current line number (1-based)
    line: u32,
    /// Current column number (1-based, in characters, not bytes)
    column: u32,
}

impl Position {
    /// Convert to a Location
    fn to_location(self) -> crate::token::Location {
        crate::token::Location {
            line: self.line as usize,
            column: self.column as usize,
            offset: self.offset,
        }
    }
}

impl<'a> StreamingLexer<'a> {
    /// Create a new streaming lexer with default configuration
    pub fn new(source: &'a str) -> Self {
        Self::with_config(source, LexerConfig::default())
    }

    /// Create a new streaming lexer with custom configuration
    pub fn with_config(source: &'a str, config: LexerConfig) -> Self {
        // Use a very small buffer to minimize memory usage
        // honour the requested size but keep a lower guard of 8
        let buffer_capacity = config.max_buffer_size.max(8);

        Self {
            source,
            inner: LogosToken::lexer(source),
            position: Position::default(),
            buffer: VecDeque::with_capacity(buffer_capacity),
            eof: false,
            config, // No need to clone, LexerConfig is Copy
        }
    }

    /// Get the next token from the input stream
    pub fn next_token(&mut self) -> Option<Token> {
        // If we have buffered tokens, return the next one
        if let Some(token) = self.buffer.pop_front() {
            return Some(token);
        }

        // If we've already reached EOF, return None
        if self.eof {
            return None;
        }

        // Fill the buffer with more tokens
        if self.fill_buffer().is_some() {
            self.buffer.pop_front()
        } else {
            self.eof = true;
            None
        }
    }

    /// Fill the buffer with tokens from the input
    fn fill_buffer(&mut self) -> Option<()> {
        if self.eof || self.buffer.len() >= self.config.max_buffer_size {
            return None;
        }

        // Process tokens in very small chunks to minimize memory usage
        let target_size = 4; // Process only 4 tokens at a time
        let mut processed = 0;
        let mut tokens_added = 0;

        // Clear any excess capacity in the buffer to free up memory
        // Only shrink if buffer is significantly larger than needed
        if self.buffer.capacity() > self.buffer.len() * 2 {
            self.buffer.shrink_to_fit();
        }

        while processed < target_size {
            // Get the next token from the Logos lexer
            let token = match self.inner.next() {
                Some(Ok(token)) => {
                    processed += 1;
                    token
                }
                Some(Err(_)) => continue, // Skip lexer errors for now
                None => {
                    self.eof = true;
                    break;
                }
            };

            let span = self.inner.span();

            // Update position before converting token
            self.update_position(&span);

            // Convert the token and add to buffer if not None
            if let Some(converted) = self.convert_token(token, span) {
                // Reserve space for the new token to avoid multiple allocations
                if self.buffer.capacity() == self.buffer.len() {
                    self.buffer.reserve(1);
                }
                self.buffer.push_back(converted);
                tokens_added += 1;
            }
        } // End of while loop

        if tokens_added > 0 {
            Some(())
        } else {
            None
        }
    }

    /// Update the current position based on the span of the current token
    fn update_position(&mut self, span: &std::ops::Range<usize>) {
        // The text between the last token and the current one
        let text = &self.source[self.position.offset..span.start];

        // Initialize line and column if this is the first token
        if self.position.line == 0 {
            self.position.line = 1;
            self.position.column = 1;
        }

        // Count newlines and update position
        for c in text.chars() {
            if c == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else if c == '\r' {
                // Handle Windows line endings
                continue;
            } else {
                self.position.column += 1;
            }
            // Update offset for each character to handle multi-byte characters correctly
            self.position.offset += c.len_utf8();
        }
    }

    /// Convert a Logos token to our internal Token type
    fn convert_token(&self, token_type: LogosToken, span: std::ops::Range<usize>) -> Option<Token> {
        // Skip whitespace and comments if not included
        match token_type {
            LogosToken::Whitespace if !self.config.include_whitespace => {
                return None;
            }
            LogosToken::LineComment | LogosToken::BlockComment if !self.config.include_comments => {
                return None;
            }
            _ => {}
        }

        // Get the lexeme from the source and intern it once
        let lexeme = InternedString::from(&self.source[span.clone()]);

        // Convert token type, reusing the already interned string where possible
        let token_type = match token_type {
            // Handle errors and special tokens
            LogosToken::Error => return None,
            LogosToken::Whitespace => TokenType::Whitespace,
            LogosToken::LineComment | LogosToken::BlockComment => {
                TokenType::Comment(lexeme.clone())
            }

            // Literals
            LogosToken::String(s) => TokenType::String(InternedString::from(&s[..])),
            LogosToken::Integer(n) => TokenType::Integer(n),
            LogosToken::Float(f) => TokenType::Float(f),
            LogosToken::Bool(b) => TokenType::Boolean(b),

            // Identifiers and medical codes - all can reuse the same interned string
            LogosToken::Identifier(_) => TokenType::Identifier(lexeme.clone()),
            LogosToken::ICD10(code) => TokenType::ICD10(InternedString::from(&code[..])),
            LogosToken::LOINC(code) => TokenType::LOINC(InternedString::from(&code[..])),
            LogosToken::SNOMED(code) => TokenType::SNOMED(InternedString::from(&code[..])),
            LogosToken::CPT(code) => TokenType::CPT(InternedString::from(&code[..])),

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

            // Medical keywords (treated as identifiers)
            LogosToken::Patient => TokenType::Identifier(InternedString::from("patient")),
            LogosToken::Observation => TokenType::Identifier(InternedString::from("observation")),
            LogosToken::Medication => TokenType::Identifier(InternedString::from("medication")),
            LogosToken::FhirQuery => TokenType::Identifier(InternedString::from("fhir_query")),
            LogosToken::Query => TokenType::Identifier(InternedString::from("query")),
            LogosToken::Regulate => TokenType::Identifier(InternedString::from("regulate")),
            LogosToken::Scope => TokenType::Identifier(InternedString::from("scope")),
            LogosToken::Federated => TokenType::Identifier(InternedString::from("federated")),
            LogosToken::Safe => TokenType::Identifier(InternedString::from("safe")),
            LogosToken::RealTime => TokenType::Identifier(InternedString::from("real_time")),
            LogosToken::Of => TokenType::Identifier(InternedString::from("of")),
            LogosToken::Per => TokenType::Identifier(InternedString::from("per")),

            // Operators and punctuation
            LogosToken::Equal => TokenType::Equal,
            LogosToken::Or => TokenType::BitOr,
            LogosToken::And => TokenType::BitAnd,
            LogosToken::Not => TokenType::Not,
            LogosToken::EqualEqual => TokenType::EqualEqual,
            LogosToken::NotEqual => TokenType::NotEqual,
            LogosToken::Less => TokenType::Less,
            LogosToken::LessEqual => TokenType::LessEqual,
            LogosToken::Greater => TokenType::Greater,
            LogosToken::GreaterEqual => TokenType::GreaterEqual,
            LogosToken::BitOr => TokenType::BitOr,
            LogosToken::BitXor => TokenType::BitXor,
            LogosToken::BitAnd => TokenType::BitAnd,
            LogosToken::Shl => TokenType::Shl,
            LogosToken::Shr => TokenType::Shr,
            LogosToken::Plus => TokenType::Plus,
            LogosToken::Minus => TokenType::Minus,
            LogosToken::Star => TokenType::Star,
            LogosToken::Slash => TokenType::Slash,
            LogosToken::Percent => TokenType::Percent,
            LogosToken::DoubleStar => TokenType::DoubleStar,
            LogosToken::Range => TokenType::Range,
            LogosToken::RangeInclusive => TokenType::RangeInclusive,
            LogosToken::QuestionQuestion => TokenType::QuestionQuestion,
            LogosToken::QuestionColon => TokenType::QuestionColon,

            // Assignment operators
            LogosToken::PlusEqual => TokenType::PlusEqual,
            LogosToken::MinusEqual => TokenType::MinusEqual,
            LogosToken::StarEqual => TokenType::StarEqual,
            LogosToken::SlashEqual => TokenType::SlashEqual,
            LogosToken::PercentEqual => TokenType::PercentEqual,
            LogosToken::DoubleStarAssign => TokenType::DoubleStarAssign,
            LogosToken::BitAndAssign => TokenType::BitAndAssign,
            LogosToken::BitOrAssign => TokenType::BitOrAssign,
            LogosToken::BitXorAssign => TokenType::BitXorAssign,
            LogosToken::ShlAssign => TokenType::ShlAssign,
            LogosToken::ShrAssign => TokenType::ShrAssign,

            // Punctuation
            LogosToken::Semicolon => TokenType::Semicolon,
            LogosToken::Colon => TokenType::Colon,
            LogosToken::Comma => TokenType::Comma,
            LogosToken::Dot => TokenType::Dot,
            LogosToken::LeftParen => TokenType::LeftParen,
            LogosToken::RightParen => TokenType::RightParen,
            LogosToken::LeftBrace => TokenType::LeftBrace,
            LogosToken::RightBrace => TokenType::RightBrace,
            LogosToken::LeftBracket => TokenType::LeftBracket,
            LogosToken::RightBracket => TokenType::RightBracket,
            LogosToken::Arrow => TokenType::Arrow,
            LogosToken::Underscore => TokenType::Underscore,

            // All LogosToken variants should be handled above.
            // If we reach here, it means a new variant was added to LogosToken but not handled here.
            // This is a bug that should be fixed by adding the new variant to the match statement.
            #[allow(unreachable_patterns)]
            _ => unreachable!("Unhandled LogosToken variant. This is a bug. Please report it."),
        };

        // Create token with position information
        // Create the token using the already interned string
        Some(Token::from_interned(
            token_type,
            lexeme, // Already interned above
            self.position.to_location(),
        ))
    }
}

impl Iterator for StreamingLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn test_streaming_lexer() {
        // Enable logging for the test
        std::env::set_var("RUST_LOG", "debug");
        let _ = env_logger::builder().is_test(true).try_init();

        let source = "let x = 42;\nlet y = 'test';";
        println!("Source: {source:?}");

        // Create a lexer with debug output
        let config = LexerConfig {
            max_buffer_size: 10, // Small buffer to test chunking
            include_whitespace: true,
            include_comments: true,
        };
        let mut lexer = StreamingLexer::with_config(source, config);

        // First token: 'let'
        println!("\n--- First token ---");
        let token = match lexer.next() {
            Some(t) => t,
            None => panic!("Failed to get first token"),
        };
        println!("Got first token: {token:?}");
        assert!(
            matches!(token.token_type, TokenType::Let),
            "Expected Let, got {:?}",
            token.token_type
        );
        assert_eq!(token.lexeme.as_str(), "let");
        assert_eq!(token.location.line, 1);
        assert_eq!(token.location.column, 1);

        // Second token: 'x' (identifier)
        println!("\n--- Second token ---");
        let token = match lexer.next() {
            Some(t) => t,
            None => panic!("Failed to get second token"),
        };
        println!("Got second token: {token:?}");
        assert!(
            matches!(&token.token_type, TokenType::Identifier(_)),
            "Expected Identifier, got {:?}",
            token.token_type
        );
        assert_eq!(token.lexeme.as_str(), "x");
        assert_eq!(token.location.column, 5);

        // Continue with the rest of the tokens...
        println!("\n--- Collecting remaining tokens ---");
        let tokens: Vec<_> = lexer.collect();
        println!("Number of remaining tokens: {}", tokens.len());

        // Print all tokens for debugging
        for (i, token) in tokens.iter().enumerate() {
            println!("Token {}: {:?}", i + 1, token);
        }

        // Verify we got the expected number of tokens
        // Expected tokens: =, 42, ;, let, y, =, 'test', ;
        assert!(!tokens.is_empty(), "No tokens were collected");
        assert!(
            tokens.len() >= 6,
            "Expected at least 6 tokens, got {}",
            tokens.len()
        );

        // Verify some key tokens
        assert!(matches!(tokens[0].token_type, TokenType::Equal));
        assert!(matches!(tokens[1].token_type, TokenType::Integer(42)));
        assert!(matches!(tokens[2].token_type, TokenType::Semicolon));
        assert!(matches!(tokens[3].token_type, TokenType::Let));
        assert!(matches!(tokens[4].token_type, TokenType::Identifier(_)));
        assert_eq!(tokens[4].lexeme.as_str(), "y");
    }
}
