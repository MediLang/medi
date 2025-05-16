//! Lexer implementation for the Medi language
//! Converts source code into a stream of tokens for the parser

use logos::Logos;
use std::ops::Range;

use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// The main lexer struct that holds the state of the lexing process
pub struct Lexer<'a> {
    /// The source code being lexed
    source: &'a str,
    /// The current line number (1-based)
    line: u32,
    /// The current column number (1-based)
    column: u32,
    /// The current byte offset in the source
    offset: usize,
    /// The inner Logos lexer
    inner: logos::Lexer<'a, LogosToken>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'a str) -> Self {
        let inner = LogosToken::lexer(source);
        let line = 1;
        let column = 1;
        let offset = 0;

        Self {
            source,
            line,
            column,
            offset,
            inner,
        }
    }

    /// Update the lexer's position based on the current span
    /// This handles multi-byte characters correctly by counting characters, not bytes
    fn sync_position_to(&mut self, span: &Range<usize>) {
        // The text between the last token and the current one
        let text = &self.source[self.offset..span.start];

        // Count newlines and update position
        let mut iter = text.chars().peekable();
        while let Some(c) = iter.next() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else if c == '\r' {
                // Handle Windows line endings (\r\n)
                if iter.peek() == Some(&'\n') {
                    // Skip the next character since we've already handled this newline
                    iter.next();
                }
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        // Update the offset to the start of the current token
        self.offset = span.start;
    }

    /// Convert a LogosToken to our semantic TokenType
    fn convert_token(
        &mut self,
        logos_token: LogosToken,
        lexeme: &str,
        span: &Range<usize>,
    ) -> Token {
        // Capture position *before* we advance
        let start_location = crate::token::Location {
            line: self.line as usize,
            column: self.column as usize,
            offset: self.offset,
        };

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
            LogosToken::Float(f) => TokenType::Float(f), // f32 to f64 conversion is handled in the LogosToken definition
            LogosToken::String(s) => TokenType::String(s),
            LogosToken::Bool(b) => TokenType::Bool(b),
            LogosToken::Identifier(ident) => TokenType::Identifier(ident), // Standard keywords are already handled by LogosToken variants

            // Medical codes
            LogosToken::ICD10(code) => TokenType::ICD10(code),
            LogosToken::LOINC(code) => TokenType::LOINC(code),
            LogosToken::SNOMED(code) => TokenType::SNOMED(code),
            LogosToken::CPT(code) => TokenType::CPT(code),

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
            LogosToken::And => TokenType::And,
            LogosToken::Or => TokenType::Or,
            LogosToken::Not => TokenType::Not,
            LogosToken::Equal => TokenType::Equal,
            LogosToken::PlusEqual => TokenType::PlusEqual,
            LogosToken::MinusEqual => TokenType::MinusEqual,
            LogosToken::StarEqual => TokenType::StarEqual,
            LogosToken::SlashEqual => TokenType::SlashEqual,
            LogosToken::PercentEqual => TokenType::PercentEqual,
            LogosToken::DotDot => TokenType::DotDot,
            LogosToken::DotDotEqual => TokenType::DotDotEqual,

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
            LogosToken::Pipe => TokenType::Pipe,
            LogosToken::Underscore => TokenType::Underscore,

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
                // Skip these tokens as they don't affect the syntax
                if let Some(token) = self.next_token() {
                    return token;
                } else {
                    // If there are no more tokens, return an error token
                    return Token {
                        token_type: TokenType::LexerError,
                        lexeme: "".to_string(),
                        location: Location {
                            line: self.line as usize,
                            column: self.column as usize,
                            offset: self.offset,
                        },
                    };
                }
            }
        };

        // Create the token with the correct location information
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            location: start_location,
        }
    }
}

impl Lexer<'_> {
    /// Get the next token from the source code
    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            // Get the next token from the inner lexer
            let logos_token = match self.inner.next()? {
                Ok(token) => token,
                Err(e) => {
                    println!("Lexer error: {:?}", e);
                    return None;
                }
            };

            // Get the span of the current token
            let span = self.inner.span();

            // Get the lexeme from the source
            let lexeme = &self.source[span.clone()];

            // Debug output
            println!(
                "Processing token: {:?} '{}' at {:?} (span: {:?})",
                logos_token,
                lexeme,
                span,
                lexeme.as_bytes()
            );

            // Update position tracking
            self.sync_position_to(&span);

            // Handle whitespace and comments (skip them)
            match &logos_token {
                LogosToken::Whitespace | LogosToken::LineComment | LogosToken::BlockComment => {
                    println!("Skipping token: {:?}", logos_token);
                    continue;
                }

                LogosToken::Error => {
                    println!("Error token found: '{}'", lexeme);
                    // Try to treat it as an identifier if it starts with a letter or underscore
                    if lexeme
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_alphabetic() || c == '_' || !c.is_ascii())
                    {
                        println!("Treating as identifier: '{}'", lexeme);
                        let token = Token {
                            token_type: TokenType::Identifier(lexeme.to_string()),
                            lexeme: lexeme.to_string(),
                            location: Location {
                                line: self.line as usize,
                                column: self.column as usize,
                                offset: self.offset,
                            },
                        };
                        return Some(token);
                    } else {
                        println!("Invalid token: '{}'", lexeme);
                        return Some(Token {
                            token_type: TokenType::Error(format!("Invalid token: {}", lexeme)),
                            lexeme: lexeme.to_string(),
                            location: Location {
                                line: self.line as usize,
                                column: self.column as usize,
                                offset: self.offset,
                            },
                        });
                    }
                }

                _ => {
                    // For all other tokens, convert them using the convert_token method
                    let token = self.convert_token(logos_token, lexeme, &span);
                    println!("Generated token: {:?}", token);
                    return Some(token);
                }
            }
        }
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
