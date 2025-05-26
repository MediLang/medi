use std::collections::VecDeque;
use std::sync::Arc;

use log::{debug, trace};
use logos::{Lexer as LogosLexer, Logos};

use crate::string_interner::InternedString;
use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// Configuration for the streaming lexer
#[derive(Debug, Clone, Copy)]
pub struct LexerConfig {
    /// Maximum number of tokens to buffer internally
    pub max_buffer_size: usize,
    /// Whether to include whitespace tokens
    pub include_whitespace: bool,
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self {
            max_buffer_size: 1024,
            include_whitespace: false,
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

impl<'a> StreamingLexer<'a> {
    /// Create a new streaming lexer with default configuration
    pub fn new(source: &'a str) -> Self {
        Self::with_config(source, LexerConfig::default())
    }

    /// Create a new streaming lexer with custom configuration
    pub fn with_config(source: &'a str, config: LexerConfig) -> Self {
        let inner = LogosToken::lexer(source);
        
        Self {
            inner,
            source,
            config,
            buffer: VecDeque::with_capacity(config.max_buffer_size.min(128)),
            position: Position::default(),
            eof: false,
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
        self.fill_buffer()?;
        
        // Return the next token (or None if we're at EOF)
        self.buffer.pop_front()
    }

    /// Fill the buffer with tokens from the input
    fn fill_buffer(&mut self) -> Option<()> {
        while !self.eof && self.buffer.len() < self.config.max_buffer_size {
            // Get the next token from the Logos lexer
            let token = match self.inner.next() {
                Some(token) => token,
                None => {
                    self.eof = true;
                    return None;
                }
            };
            
            let span = self.inner.span();
            
            match token {
                Ok(LogosToken::Error) => {
                    // Skip invalid tokens for now
                    continue;
                }
                Ok(token_type) => {
                    self.update_position(&span);
                    
                    if let Some(token) = self.convert_token(token_type, span) {
                        self.buffer.push_back(token);
                    }
                }
                Err(_) => {
                    // Skip any lexing errors for now
                    continue;
                }
            }
        }
        
        // Check if we've reached the end of the source
        if self.inner.span().end >= self.source.len() {
            self.eof = true;
        }
        
        Some(())
    }

    /// Update the current position based on the span of the current token
    fn update_position(&mut self, span: &std::ops::Range<usize>) {
        // The text between the last token and the current one
        let text = &self.source[self.position.offset..span.start];

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
        }

        // Update the offset to the start of the current token
        self.position.offset = span.start;
    }

    /// Convert a LogosToken to our internal Token type
    fn convert_token(&self, token_type: LogosToken, span: std::ops::Range<usize>) -> Option<Token> {
        let lexeme = InternedString::new(&self.source[span.clone()]);
        let location = Location {
            line: self.position.line as usize,
            column: self.position.column as usize,
            offset: span.start,
        };

        // Skip whitespace unless explicitly requested
        if matches!(token_type, LogosToken::Whitespace) && !self.config.include_whitespace {
            return None;
        }

        // Convert the LogosToken to our TokenType
        let token_type = match token_type {
            LogosToken::Identifier(_) => TokenType::Identifier(lexeme.clone()),
            LogosToken::String(_) => {
                // Remove the surrounding quotes and unescape the string
                let s = &self.source[span.clone()];
                let s = &s[1..s.len()-1]; // Remove quotes
                TokenType::String(InternedString::new(s.replace("\\\"", "\"").as_str()))
            },
            LogosToken::ICD10(_) => TokenType::ICD10(lexeme.clone()),
            LogosToken::LOINC(_) => TokenType::LOINC(lexeme.clone()),
            LogosToken::SNOMED(_) => TokenType::SNOMED(lexeme.clone()),
            LogosToken::CPT(_) => TokenType::CPT(lexeme.clone()),
            // Handle numeric literals
            LogosToken::Integer(n) => TokenType::Integer(n),
            LogosToken::Float(f) => TokenType::Float(f),
            LogosToken::Bool(b) => TokenType::Boolean(b),
            // Handle keywords
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
            // Handle operators
            LogosToken::Or => TokenType::Or,
            LogosToken::And => TokenType::And,
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
            LogosToken::Range => TokenType::Range,
            LogosToken::RangeInclusive => TokenType::RangeInclusive,
            LogosToken::QuestionQuestion => TokenType::QuestionQuestion,
            LogosToken::QuestionColon => TokenType::QuestionColon,
            // Default case for any unhandled tokens
            _ => TokenType::Error(lexeme.clone()),
        };

        Some(Token::new(token_type, lexeme, location))
    }
}

impl<'a> Iterator for StreamingLexer<'a> {
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
        let source = "let x = 42;\nlet y = 'test';";
        let mut lexer = StreamingLexer::new(source);

        // First token: 'let'
        let token = lexer.next().unwrap();
        assert!(matches!(token.token_type, TokenType::Let));
        assert_eq!(token.lexeme.as_str(), "let");
        assert_eq!(token.location.line, 1);
        assert_eq!(token.location.column, 1);

        // Second token: 'x' (identifier)
        let token = lexer.next().unwrap();
        assert!(matches!(&token.token_type, TokenType::Identifier(_)));
        assert_eq!(token.lexeme.as_str(), "x");
        assert_eq!(token.location.column, 5);

        // Continue with the rest of the tokens...
        let tokens: Vec<_> = lexer.collect();
        assert!(!tokens.is_empty());
    }
}
