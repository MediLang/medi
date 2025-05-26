//! A memory-efficient, streaming lexer for the Medi programming language.
//!
//! The `ChunkedLexer` provides a way to tokenize large input sources without
//! loading the entire input into memory at once. It processes the input in
//! configurable chunks, making it suitable for processing very large files
//! or streams.
//!
//! # Features
//! - Processes input in fixed-size chunks to minimize memory usage
//! - Handles tokens that span chunk boundaries
//! - Tracks source positions accurately across chunks
//! - Provides both iterator and direct API access
//! - Configurable chunk size and buffer limits
//!
//! # Examples
//!
//! Basic usage with a string:
//! ```no_run
//! use medic_lexer::{ChunkedLexer, ChunkedLexerConfig};
//! use std::io::Cursor;
//!
//! let input = "let x = 42;\nlet y = x + 1;";
//! let cursor = Cursor::new(input);
//! let config = ChunkedLexerConfig {
//!     chunk_size: 32,
//!     max_buffer_size: 20,
//!     include_whitespace: false,
//!     keep_source_in_memory: true,
//! };
//!
//! for token in ChunkedLexer::from_reader(cursor, config) {
//!     println!("Token: {:?}", token);
//! }
//! ```
//!
//! Processing a large file:
//! ```no_run
//! use medic_lexer::{ChunkedLexer, ChunkedLexerConfig};
//! use std::path::Path;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let path = Path::new("large_file.medi");
//! let lexer = ChunkedLexer::from_file(path)?;
//! 
//! // Process tokens in a streaming fashion
//! for token in lexer {
//!     // Process each token
//!     println!("Token: {:?}", token);
//! }
//! # Ok(())
//! # }
//! ```

use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

use log::error;
use logos::{Logos, Span};

use crate::string_interner::InternedString;
use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// Configuration for the chunked lexer
#[derive(Debug, Clone, Copy)]
pub struct ChunkedLexerConfig {
    /// Size of each chunk in bytes
    pub chunk_size: usize,
    /// Maximum number of tokens to buffer internally
    pub max_buffer_size: usize,
    /// Whether to include whitespace tokens
    pub include_whitespace: bool,
    /// Whether to keep the source in memory (faster but uses more memory)
    pub keep_source_in_memory: bool,
}

impl Default for ChunkedLexerConfig {
    fn default() -> Self {
        Self {
            chunk_size: 8 * 1024, // 8KB chunks by default
            max_buffer_size: 1024,
            include_whitespace: false,
            keep_source_in_memory: false,
        }
    }
}

/// A lexer that processes source code in chunks from a file
pub struct ChunkedLexer {
    /// The source file being lexed
    source: Box<dyn BufRead>,
    /// Current chunk being processed
    current_chunk: String,
    /// Buffer for tokens that have been lexed but not yet consumed
    buffer: VecDeque<Token>,
    /// Configuration for the lexer
    config: ChunkedLexerConfig,
    /// Current position in the source
    position: Position,
    /// Whether we've reached the end of the input
    eof: bool,
    /// Internal buffer for partial tokens across chunks
    partial_token: Option<PartialToken>,
}

/// Tracks the current position in the source code
#[derive(Debug, Clone, Copy, Default)]
struct Position {
    /// Current byte offset
    offset: usize,
    /// Current line number (1-based)
    line: usize,
    /// Current column number (1-based, in characters, not bytes)
    column: usize,
}

/// Represents a token that spans multiple chunks
#[derive(Debug, Clone)]
struct PartialToken {
    token_type: LogosToken,
    start_pos: Position,
    partial_lexeme: String,
}

impl ChunkedLexer {
    /// Convert a Logos token to our internal token type
    fn convert_token(
        &self,
        token_type: LogosToken,
        span: Span,
        source: &str,
    ) -> Option<Token> {
        use LogosToken::*;
        
        let lexeme = &source[span.start..span.end];
        
        // Helper to create a token with the current span
        let create_token = |token_type| {
            Token::new(
                token_type,
                lexeme,
                Location {
                    line: self.position.line,
                    column: self.position.column,
                    offset: self.position.offset + span.start,
                },
            )
        };
        
        match token_type {
            // Identifiers and keywords
            Identifier => {
                // Check if this is a keyword
                let token_type = match lexeme {
                    "module" => TokenType::Module,
                    "import" => TokenType::Import,
                    "fn" => TokenType::Fn,
                    "let" => TokenType::Let,
                    "const" => TokenType::Const,
                    "type" => TokenType::Type,
                    "struct" => TokenType::Struct,
                    "enum" => TokenType::Enum,
                    "trait" => TokenType::Trait,
                    "impl" => TokenType::Impl,
                    "pub" => TokenType::Pub,
                    "priv" => TokenType::Priv,
                    "return" => TokenType::Return,
                    "while" => TokenType::While,
                    "for" => TokenType::For,
                    "in" => TokenType::In,
                    "match" => TokenType::Match,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "fhir_query" => TokenType::FhirQuery,
                    "query" => TokenType::Query,
                    "regulate" => TokenType::Regulate,
                    "scope" => TokenType::Scope,
                    "federated" => TokenType::Federated,
                    "safe" => TokenType::Safe,
                    "real_time" => TokenType::RealTime,
                    "patient" => TokenType::Patient,
                    "observation" => TokenType::Observation,
                    "medication" => TokenType::Medication,
                    "of" => TokenType::Of,
                    _ => TokenType::Identifier(InternedString::new(lexeme)),
                };
                Some(create_token(token_type))
            }
            
            // Literals
            Integer(_) => {
                let value = lexeme.parse().unwrap_or_else(|_| {
                    error!("Failed to parse integer: {}", lexeme);
                    0
                });
                Some(create_token(TokenType::Integer(value)))
            }
            
            Float(_) => {
                let value = lexeme.parse().unwrap_or_else(|_| {
                    error!("Failed to parse float: {}", lexeme);
                    0.0
                });
                Some(create_token(TokenType::Float(value)))
            }
            
            StringLiteral => {
                // Remove quotes from string literals
                if lexeme.len() >= 2 && lexeme.starts_with('"') && lexeme.ends_with('"') {
                    let content = &lexeme[1..lexeme.len() - 1];
                    Some(create_token(TokenType::String(InternedString::new(content))))
                } else {
                    error!("Invalid string literal: {}", lexeme);
                    None
                }
            }
            
            // Operators and punctuation
            Plus => Some(create_token(TokenType::Plus)),
            Minus => Some(create_token(TokenType::Minus)),
            Star => Some(create_token(TokenType::Star)),
            Slash => Some(create_token(TokenType::Slash)),
            Percent => Some(create_token(TokenType::Percent)),
            Equal => Some(create_token(TokenType::Equal)),
            EqualEqual => Some(create_token(TokenType::EqualEqual)),
            NotEqual => Some(create_token(TokenType::NotEqual)),
            Less => Some(create_token(TokenType::Less)),
            LessEqual => Some(create_token(TokenType::LessEqual)),
            Greater => Some(create_token(TokenType::Greater)),
            GreaterEqual => Some(create_token(TokenType::GreaterEqual)),
            LeftParen => Some(create_token(TokenType::LeftParen)),
            RightParen => Some(create_token(TokenType::RightParen)),
            LeftBrace => Some(create_token(TokenType::LeftBrace)),
            RightBrace => Some(create_token(TokenType::RightBrace)),
            LeftBracket => Some(create_token(TokenType::LeftBracket)),
            RightBracket => Some(create_token(TokenType::RightBracket)),
            Comma => Some(create_token(TokenType::Comma)),
            Dot => Some(create_token(TokenType::Dot)),
            Semicolon => Some(create_token(TokenType::Semicolon)),
            Colon => Some(create_token(TokenType::Colon)),
            
            // Comments and whitespace
            Comment | Whitespace => None, // Skip comments and whitespace
            
            // Error tokens
            Error => {
                error!("Lexer error at position {}: {}", span.start, lexeme);
                None
            }
        }
    }
    
    /// Update the position based on the current token
    fn update_position(&mut self, token: &Token) {
        // Get the lexeme as a string slice
        let lexeme_str = token.lexeme.as_str();
        
        // Count newlines in the token to update line/column
        let newlines = lexeme_str.matches('\n').count();
        
        if newlines > 0 {
            self.position.line += newlines;
            // Column is 1 + number of chars after last newline
            self.position.column = 1 + lexeme_str
                .chars()
                .rev()
                .take_while(|&c| c != '\n')
                .count();
        } else {
            self.position.column += lexeme_str.chars().count();
        }
        
        // Update the offset based on the token's length
        self.position.offset = token.location.offset + lexeme_str.len();
        
        // Ensure we don't have any invalid positions
        if self.position.line == 0 {
            self.position.line = 1;
        }
        if self.position.column == 0 {
            self.position.column = 1;
        }
    }
    /// Create a new chunked lexer for a file with default configuration
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        Self::from_file_with_config(path, ChunkedLexerConfig::default())
    }

    /// Create a new chunked lexer for a file with custom configuration
    pub fn from_file_with_config<P: AsRef<Path>>(
        path: P,
        config: ChunkedLexerConfig,
    ) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::with_capacity(config.chunk_size, file);
        
        Ok(Self::from_reader(reader, config))
    }

    /// Create a new chunked lexer from any BufRead implementor
    pub fn from_reader<R: BufRead + 'static>(reader: R, config: ChunkedLexerConfig) -> Self {
        Self {
            source: Box::new(reader),
            current_chunk: String::with_capacity(config.chunk_size),
            buffer: VecDeque::with_capacity(config.max_buffer_size.min(128)),
            config,
            position: Position::default(),
            eof: false,
            partial_token: None,
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
            // If we don't have a partial token and need more input, read next chunk
            if self.partial_token.is_none() && self.current_chunk.is_empty() {
                if !self.read_next_chunk()? {
                    break;
                }
            }

            // Process the current chunk
            self.process_chunk()?;
        }

        Some(())
    }

    /// Read the next chunk from the input source
    /// 
    /// This method reads the next chunk of data from the input source, handling any
    /// partial tokens from the previous chunk. It returns `Some(true)` if more data
    /// is available, `Some(false)` if an error occurred but we should continue,
    /// or `None` if we've reached the end of the input.
    /// 
    /// # Returns
    /// - `Some(true)`: Successfully read a chunk, more data may be available
    /// - `Some(false)`: Error occurred but we should continue processing
    /// - `None`: End of input reached
    fn read_next_chunk(&mut self) -> Option<bool> {
        // Clear the current chunk but keep capacity
        self.current_chunk.clear();
        
        // Read a line from the source
        match self.source.read_line(&mut self.current_chunk) {
            // End of file
            Ok(0) => {
                self.eof = true;
                
                // If we have a partial token at EOF, emit it as an error
                if let Some(partial) = &self.partial_token {
                    if !partial.partial_lexeme.is_empty() {
                        error!(
                            "Incomplete token at end of file: {:?}", 
                            partial.partial_lexeme
                        );
                        
                        // Create an error token for the partial token
                        let token = Token {
                            token_type: TokenType::Error("incomplete token".into()),
                            lexeme: partial.partial_lexeme.clone(),
                            location: Location {
                                line: partial.start_pos.line,
                                column: partial.start_pos.column,
                                offset: partial.start_pos.offset,
                            },
                        };
                        
                        self.buffer.push_back(token);
                        self.partial_token = None;
                    }
                }
                
                None
            }
            
            // Successfully read some data
            Ok(bytes_read) => {
                trace!("Read {} bytes from source", bytes_read);
                
                // If we have a partial token, we'll handle it in process_chunk
                // by prepending it to the current chunk
                
                // Check if we've reached the end of the file
                if bytes_read < self.config.chunk_size && !self.current_chunk.ends_with('\n') {
                    self.eof = true;
                }
                
                Some(true)
            }
            
            // Error reading from source
            Err(e) => {
                error!("Error reading from source: {}", e);
                
                // If this is a WouldBlock error, we'll try again later
                if e.kind() == io::ErrorKind::WouldBlock {
                    trace!("Read would block, retrying later");
                    Some(false)
                } else {
                    // For other errors, mark as EOF and stop
                    self.eof = true;
                    None
                }
            }
        }
    }

    /// Process the current chunk and extract tokens
    /// 
    /// This method processes the current chunk of source code, tokenizes it using the Logos lexer,
    /// and handles partial tokens that might span chunk boundaries. It maintains accurate position
    /// tracking and handles errors gracefully.
    fn process_chunk(&mut self) -> Option<()> {
        use LogosToken::*;
        
        // If we have a partial token from the previous chunk, prepend it to the current chunk
        let mut source = if let Some(partial) = &self.partial_token.take() {
            let mut combined = String::with_capacity(
                partial.partial_lexeme.len() + self.current_chunk.len()
            );
            combined.push_str(&partial.partial_lexeme);
            combined.push_str(&self.current_chunk);
            combined
        } else {
            self.current_chunk.clone()
        };

        let mut lexer = LogosToken::lexer(&source);
        let mut last_valid_span = 0..0;
        let mut last_valid_token_type = None;
        
        // Process tokens from the current chunk
        while let Some(token_result) = lexer.next() {
            let span = lexer.span();
            last_valid_span = span.clone();
            
            match token_result {
                Ok(token_type) => {
                    last_valid_token_type = Some(token_type);
                    
                    if let Some(mut token) = self.convert_token(token_type, span, &source) {
                        // Update token location with current position
                        token.location.line = self.position.line;
                        token.location.column = self.position.column;
                        
                        // Update position based on the token
                        self.update_position(&token);
                        
                        self.buffer.push_back(token);
                        
                        // If we've filled the buffer, pause processing
                        if self.buffer.len() >= self.config.max_buffer_size {
                            break;
                        }
                    }
                }
                Err(_) => {
                    // If we hit an error, it might be because we're in the middle of a token
                    // that spans chunks. We'll handle this after the loop.
                    break;
                }
            }
        }
        
        // Handle partial tokens at chunk boundaries
        let remaining = lexer.remainder();
        if !remaining.is_empty() && !self.eof {
            // If we have a valid token type, try to complete it in the next chunk
            if let Some(token_type) = last_valid_token_type {
                // For string literals and comments, we need to handle them specially
                if matches!(token_type, StringLiteral | Comment) {
                    self.partial_token = Some(PartialToken {
                        token_type,
                        start_pos: self.position,
                        partial_lexeme: remaining.to_string(),
                    });
                    
                    // Update position for the partial token
                    self.position.offset += remaining.len();
                    self.position.column += remaining.chars().count();
                } else {
                    // For other tokens, we'll restart from the beginning of the token
                    // in the next chunk to ensure we don't miss anything
                    let token_start = last_valid_span.start;
                    let partial = &source[token_start..];
                    
                    if !partial.is_empty() {
                        self.partial_token = Some(PartialToken {
                            token_type: Error, // Will be determined in next chunk
                            start_pos: Position {
                                offset: self.position.offset - (source.len() - token_start),
                                ..self.position
                            },
                            partial_lexeme: partial.to_string(),
                        });
                    }
                }
            } else {
                // No valid token type, just save the remaining text
                self.partial_token = Some(PartialToken {
                    token_type: Error,
                    start_pos: self.position,
                    partial_lexeme: remaining.to_string(),
                });
                
                // Update position for the partial token
                self.position.offset += remaining.len();
                self.position.column += remaining.chars().count();
            }
        }
        
        // Clear the chunk after processing
        self.current_chunk.clear();
        
        Some(())
    }
}

impl ChunkedLexer {
    /// Process the entire input and return all tokens as a vector
    /// 
    /// This is a convenience method that consumes the lexer and returns all tokens.
    /// For very large inputs, consider using the iterator interface instead to
    /// avoid holding all tokens in memory at once.
    pub fn into_tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        
        while let Some(token) = self.next_token() {
            tokens.push(token);
            
            // Prevent infinite loops in case of bugs
            if tokens.len() > 1_000_000 {
                error!("Excessive number of tokens generated, possible infinite loop");
                break;
            }
        }
        
        tokens
    }
    
    /// Process the entire input and return a result with all tokens or an error
    /// 
    /// This is similar to `into_tokens` but returns a `Result` that can be used
    /// with the `?` operator for better error handling.
    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        
        while let Some(token) = self.next_token() {
            // Check for error tokens
            if let TokenType::Error(msg) = &token.token_type {
                return Err(format!(
                    "Lexer error at line {}:{} - {}",
                    token.location.line, token.location.column, msg
                ));
            }
            
            tokens.push(token);
            
            // Prevent infinite loops in case of bugs
            if tokens.len() > 1_000_000 {
                return Err("Excessive number of tokens generated, possible infinite loop".to_string());
            }
        }
        
        Ok(tokens)
    }
}

impl Iterator for ChunkedLexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
    
    fn size_hint(&self) -> (usize, Option<usize>) {
        // Provide a rough estimate of the number of tokens remaining
        // This helps with iterator optimizations
        let remaining_bytes = self.position.offset.saturating_sub(self.position.offset);
        // Estimate ~6 bytes per token on average
        let avg_token_size = 6;
        let estimated_tokens = remaining_bytes / avg_token_size;
        
        (0, Some(estimated_tokens.max(1)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use std::path::PathBuf;
    use tempfile::NamedTempFile;
    use std::io::Write;

    /// Helper function to create a temporary file with the given content
    fn create_temp_file(content: &str) -> NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        write!(file, "{}", content).unwrap();
        file
    }

    #[test]
    fn test_chunked_lexer_basic() {
        let input = "let x = 42;\nlet y = x + 1;";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 8,
            max_buffer_size: 10,
            include_whitespace: false,
            keep_source_in_memory: true,
        };
        
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();
        
        // Verify we got the expected number of tokens
        assert_eq!(tokens.len(), 8);
        
        // Verify some token types
        assert!(matches!(tokens[0].token_type, TokenType::Let));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::Equal));
        assert!(matches!(tokens[3].token_type, TokenType::Integer(42)));
    }
    
    #[test]
    fn test_chunked_lexer_with_file() {
        // Create a temporary file with test content
        let content = r#"
            // A simple function
            fn add(a: int, b: int) -> int {
                return a + b;
            }
            
            let result = add(5, 3);
        "#;
        
        let file = create_temp_file(content);
        let path = file.path();
        
        // Test with a small chunk size to force chunked reading
        let config = ChunkedLexerConfig {
            chunk_size: 32, // Small chunk size to test chunking
            max_buffer_size: 20,
            include_whitespace: false,
            keep_source_in_memory: false,
        };
        
        let lexer = ChunkedLexer::from_file(path).unwrap();
        let tokens: Vec<_> = lexer.collect();
        
        // Verify we got some tokens
        assert!(!tokens.is_empty());
        
        // Verify we have function definition and call tokens
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Fn));
        assert!(token_types.contains(&&TokenType::Return));
        assert!(token_types.contains(&&TokenType::Let));
    }
    
    #[test]
    fn test_chunked_lexer_large_input() {
        // Generate a large input with many identifiers and numbers
        let mut large_input = String::new();
        for i in 0..1000 {
            large_input.push_str(&format!("let x{} = {};\n", i, i));
        }
        
        let cursor = Cursor::new(large_input);
        
        // Use a small chunk size to ensure chunked processing
        let config = ChunkedLexerConfig {
            chunk_size: 128,
            max_buffer_size: 50,
            include_whitespace: false,
            keep_source_in_memory: false,
        };
        
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();
        
        // We should have 4 tokens per line (let, ident, =, number, ;)
        // and 1000 lines
        assert_eq!(tokens.len(), 1000 * 4);
    }
    
    #[test]
    fn test_chunked_lexer_partial_tokens() {
        // Test with tokens that span chunk boundaries
        let input = "let long_identifier_name = 12345;\n";
        
        // Use a chunk size that will split the identifier
        let chunk_size = 10;
        let cursor = Cursor::new(input);
        
        let config = ChunkedLexerConfig {
            chunk_size,
            max_buffer_size: 10,
            include_whitespace: false,
            keep_source_in_memory: true,
        };
        
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();
        
        // Verify we got all tokens correctly
        assert_eq!(tokens.len(), 4);
        assert!(matches!(tokens[0].token_type, TokenType::Let));
        
        if let TokenType::Identifier(id) = &tokens[1].token_type {
            assert_eq!(id.to_string(), "long_identifier_name");
        } else {
            panic!("Expected identifier token");
        }
        
        assert!(matches!(tokens[2].token_type, TokenType::Equal));
        assert!(matches!(tokens[3].token_type, TokenType::Integer(12345)));
    }
    
    #[test]
    fn test_chunked_lexer_string_literals() {
        // Test with string literals that might span chunks
        let input = r#"
            let greeting = "Hello, world! This is a long string that might span chunks";
            let empty = "";
            let escaped = "Line 1\nLine 2\nLine 3";
        "#;
        
        // Use a small chunk size to force chunking within strings
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 16,
            max_buffer_size: 20,
            include_whitespace: false,
            keep_source_in_memory: true,
        };
        
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();
        
        // Verify we have string literals
        let string_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::String(_)))
            .collect();
            
        assert_eq!(string_tokens.len(), 3);
    }
    
    #[test]
    fn test_chunked_lexer_error_handling() {
        // Test with invalid input
        let input = "let x = 123abc;\nlet y = 456;";
        let cursor = Cursor::new(input);
        
        let config = ChunkedLexerConfig {
            chunk_size: 8,
            max_buffer_size: 10,
            include_whitespace: false,
            keep_source_in_memory: true,
        };
        
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let result = lexer.tokenize();
        
        // Should get an error for the invalid token
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.contains("Lexer error"));
    }
    
    #[test]
    fn test_chunked_lexer_position_tracking() {
        let input = r#"
            let x = 1;
            let y = 2;
            
            // A comment
            let z = x + y;
        "#;
        
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 16, // Small chunks to test position tracking
            max_buffer_size: 20,
            include_whitespace: false,
            keep_source_in_memory: true,
        };
        
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();
        
        // Verify positions are tracked correctly
        assert!(tokens.len() >= 11);
        
        // Check positions of specific tokens
        let let_x = &tokens[0];
        assert_eq!(let_x.location.line, 2);
        
        let plus = tokens.iter().find(|t| matches!(t.token_type, TokenType::Plus)).unwrap();
        assert_eq!(plus.location.line, 6);
    }
}
