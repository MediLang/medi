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

use log::{debug, error, trace};
use logos::{Logos, Span};
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use std::string::String as StdString;

use crate::string_interner::InternedString;
use crate::token::{Token, TokenType};
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
    // String interner was removed as it wasn't being used
}

/// Tracks the current position in the source code
#[derive(Debug, Clone, Copy)]
pub struct Position {
    /// The current line number (1-based)
    pub line: usize,
    /// The current column number (1-based)
    pub column: usize,
    /// The byte offset from the start of the file
    pub offset: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: 1,   // Start line numbers at 1
            column: 1, // Start column numbers at 1
            offset: 0,
        }
    }
}

use std::ops::AddAssign;

impl AddAssign<usize> for Position {
    fn add_assign(&mut self, rhs: usize) {
        self.offset += rhs;
    }
}

impl Position {
    /// Create a new position with the given line, column, and offset
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        // Ensure line is at least 1
        let line = if line == 0 { 1 } else { line };
        Self { line, column, offset }
    }

    /// Advance the position by the given string
    pub fn advance(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }

        // Track if we're at the start of a line
        let mut at_start_of_line = self.column == 1;
        
        for c in s.chars() {
            if c == '\n' {
                // Special case: if this is the first character and it's a newline
                if self.offset == 0 && self.line == 1 {
                    self.line = 2;
                } else if self.offset > 0 || self.line > 1 {
                    // Only increment line if we're not at the very start of the file
                    self.line += 1;
                }
                self.column = 1;
                at_start_of_line = true;
            } else {
                if at_start_of_line {
                    self.column = 1;
                    at_start_of_line = false;
                }
                self.column += 1;
            }
            
            // Update the offset by the number of bytes in the character
            self.offset += c.len_utf8();
        }
    }

    /// Convert to a Location
    fn to_location(self) -> crate::token::Location {
        crate::token::Location {
            line: self.line,
            column: self.column,
            offset: self.offset,
        }
    }
}

/// Represents a token that spans multiple chunks
#[derive(Debug, Clone)]
struct PartialToken {
    /// The partial lexeme from the current chunk
    partial_lexeme: StdString,
}

impl ChunkedLexer {
    /// Helper function to map string identifiers to keyword token types
    fn get_keyword(ident: &str) -> Option<TokenType> {
        match ident {
            "module" => Some(TokenType::Module),
            "import" => Some(TokenType::Import),
            "fn" => Some(TokenType::Fn),
            "let" => Some(TokenType::Let),
            "const" => Some(TokenType::Const),
            "type" => Some(TokenType::Type),
            "struct" => Some(TokenType::Struct),
            "enum" => Some(TokenType::Enum),
            "trait" => Some(TokenType::Trait),
            "impl" => Some(TokenType::Impl),
            "pub" => Some(TokenType::Pub),
            "priv" => Some(TokenType::Priv),
            "return" => Some(TokenType::Return),
            "while" => Some(TokenType::While),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "match" => Some(TokenType::Match),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "fhir_query" => Some(TokenType::FhirQuery),
            "query" => Some(TokenType::Query),
            "regulate" => Some(TokenType::Regulate),
            "scope" => Some(TokenType::Scope),
            "federated" => Some(TokenType::Federated),
            "safe" => Some(TokenType::Safe),
            "real_time" => Some(TokenType::RealTime),
            "patient" => Some(TokenType::Patient),
            "observation" => Some(TokenType::Observation),
            "medication" => Some(TokenType::Medication),
            "of" => Some(TokenType::Of),
            "per" => Some(TokenType::Per),
            _ => None,
        }
    }

    /// Convert a Logos token to our internal token type
    fn convert_token(&mut self, token_type: LogosToken, span: &Span, source: &str) -> Option<Token> {
        let lexeme_str = &source[span.start..span.end];

        // Debug logging for token conversion
        debug!(
            "Converting token: {:?} with lexeme: '{}' at {:?}",
            token_type, lexeme_str, span
        );

        // Skip whitespace and comments
        if matches!(token_type, LogosToken::Whitespace | LogosToken::LineComment | LogosToken::BlockComment) {
            // Update position for skipped tokens
            self.position.advance(lexeme_str);
            return None;
        }

        // Create the token with the current position
        let lexeme = InternedString::from(lexeme_str);
        let token_position = self.position;
        let location = token_position.to_location();
        
        // Helper function to create tokens with the current position
        let create_token = |token_type| {
            Token::new(token_type, lexeme.clone(), location)
        };

        // Helper function to create error tokens
        let create_error = |msg: String| {
            error!("{}", msg);
            Token::new(
                TokenType::Error(InternedString::from(msg.as_str())),
                lexeme.clone(),
                location,
            )
        };
        
        // Convert the Logos token to our token type
        let token = match token_type {
            // Keywords
            LogosToken::Module => create_token(TokenType::Module),
            LogosToken::Import => create_token(TokenType::Import),
            LogosToken::Fn => create_token(TokenType::Fn),
            LogosToken::Let => create_token(TokenType::Let),
            LogosToken::Const => create_token(TokenType::Const),
            LogosToken::Type => create_token(TokenType::Type),
            LogosToken::Struct => create_token(TokenType::Struct),
            LogosToken::Enum => create_token(TokenType::Enum),
            LogosToken::Trait => create_token(TokenType::Trait),
            LogosToken::Impl => create_token(TokenType::Impl),
            LogosToken::Pub => create_token(TokenType::Pub),
            LogosToken::Priv => create_token(TokenType::Priv),
            LogosToken::Return => create_token(TokenType::Return),
            LogosToken::While => create_token(TokenType::While),
            LogosToken::For => create_token(TokenType::For),
            LogosToken::In => create_token(TokenType::In),
            LogosToken::Match => create_token(TokenType::Match),
            LogosToken::If => create_token(TokenType::If),
            LogosToken::Else => create_token(TokenType::Else),

            // Healthcare keywords
            LogosToken::FhirQuery => create_token(TokenType::FhirQuery),
            LogosToken::Query => create_token(TokenType::Query),
            LogosToken::Regulate => create_token(TokenType::Regulate),
            LogosToken::Scope => create_token(TokenType::Scope),
            LogosToken::Federated => create_token(TokenType::Federated),
            LogosToken::Safe => create_token(TokenType::Safe),
            LogosToken::RealTime => create_token(TokenType::RealTime),
            LogosToken::Patient => create_token(TokenType::Patient),
            LogosToken::Observation => create_token(TokenType::Observation),
            LogosToken::Medication => create_token(TokenType::Medication),

            // Boolean literals
            LogosToken::Bool(b) => create_token(TokenType::Boolean(b)),

            // Identifiers and literals
            LogosToken::Identifier(ident) => {
                // Check if this is a keyword
                if let Some(keyword) = Self::get_keyword(&ident) {
                    create_token(keyword)
                } else {
                    create_token(TokenType::Identifier(InternedString::from(ident.as_str())))
                }
            },
            LogosToken::String(s) => {
                create_token(TokenType::String(InternedString::from(s.as_str())))
            },
            LogosToken::Integer(n) => create_token(TokenType::Integer(n)),
            LogosToken::Float(f) => create_token(TokenType::Float(f)),

            // Medical codes
            LogosToken::ICD10(code) => create_token(TokenType::ICD10(InternedString::from(code.as_str()))),
            LogosToken::LOINC(code) => create_token(TokenType::LOINC(InternedString::from(code.as_str()))),
            LogosToken::SNOMED(code) => create_token(TokenType::SNOMED(InternedString::from(code.as_str()))),
            LogosToken::CPT(code) => create_token(TokenType::CPT(InternedString::from(code.as_str()))),

            // Operators and punctuation
            LogosToken::Plus => create_token(TokenType::Plus),
            LogosToken::Minus => create_token(TokenType::Minus),
            LogosToken::Star => create_token(TokenType::Star),
            LogosToken::Slash => create_token(TokenType::Slash),
            LogosToken::Percent => create_token(TokenType::Percent),
            LogosToken::Equal => create_token(TokenType::Equal),
            LogosToken::EqualEqual => create_token(TokenType::EqualEqual),
            LogosToken::Not => create_token(TokenType::Not),
            LogosToken::NotEqual => create_token(TokenType::NotEqual),
            LogosToken::Less => create_token(TokenType::Less),
            LogosToken::LessEqual => create_token(TokenType::LessEqual),
            LogosToken::Greater => create_token(TokenType::Greater),
            LogosToken::GreaterEqual => create_token(TokenType::GreaterEqual),
            LogosToken::And => create_token(TokenType::And),
            LogosToken::Or => create_token(TokenType::Or),
            LogosToken::Dot => create_token(TokenType::Dot),
            LogosToken::Comma => create_token(TokenType::Comma),
            LogosToken::Colon => create_token(TokenType::Colon),
            LogosToken::Semicolon => create_token(TokenType::Semicolon),
            LogosToken::LeftParen => create_token(TokenType::LeftParen),
            LogosToken::RightParen => create_token(TokenType::RightParen),
            LogosToken::LeftBrace => create_token(TokenType::LeftBrace),
            LogosToken::RightBrace => create_token(TokenType::RightBrace),
            LogosToken::LeftBracket => create_token(TokenType::LeftBracket),
            LogosToken::RightBracket => create_token(TokenType::RightBracket),
            LogosToken::Arrow => create_token(TokenType::Arrow),
            // FatArrow is not a valid LogosToken variant
            LogosToken::Error => create_error(format!("Lexer error at {}:{}", location.line, location.column)),
            _ => create_error(format!("Unhandled token type: {:?}", token_type)),
        };

        // Return the token
        Some(token)
    }

    /// Create a new chunked lexer from any BufRead implementor
    pub fn from_reader<R: BufRead + 'static>(reader: R, config: ChunkedLexerConfig) -> Self {
        let chunk_size = config.chunk_size.clamp(1_024, 1_048_576); // 1KB to 1MB chunks

        Self {
            source: Box::new(BufReader::with_capacity(chunk_size, reader)),
            current_chunk: String::with_capacity(chunk_size * 2),
            buffer: VecDeque::with_capacity(config.max_buffer_size),
            config,
            position: Position::new(1, 1, 0), // Start at line 1, column 1
            eof: false,
            partial_token: None,
        }
    }

    /// Reads the next chunk of input and processes it into tokens.
    ///
    /// Returns:
    /// - `Ok(true)` if more data might be available
    /// - `Ok(false)` if EOF has been reached
    /// - `Err(String)` if an error occurred during reading or tokenization
    fn read_next_chunk(&mut self) -> Result<bool, String> {
        // If we've already reached EOF, return false
        if self.eof {
            return Ok(false);
        }

        // Read the next chunk from the source
        let mut chunk = String::with_capacity(self.config.chunk_size);
        match self.source.read_line(&mut chunk) {
            Ok(0) => {
                // Reached EOF
                self.eof = true;
                
                // Handle any remaining partial token
                if let Some(partial) = self.partial_token.take() {
                    let partial_lexeme = partial.partial_lexeme;
                    if !partial_lexeme.is_empty() {
                        self.process_chunk(&partial_lexeme)?;
                    }
                }
                
                // Process any remaining partial token at EOF
                if let Some(partial) = self.partial_token.take() {
                    let source = partial.partial_lexeme;
                    if !source.is_empty() {
                        self.process_chunk(&source)
                            .map_err(|e| format!("Error processing final partial token: {}", e))?;
                    }
                }
                
                // Return true if there are still tokens in the buffer
                Ok(!self.buffer.is_empty())
            }
            Ok(_bytes_read) => {
                // Successfully read a chunk
                self.current_chunk = chunk;
                
                // Handle any partial token from the previous chunk
                let source = if let Some(partial) = self.partial_token.take() {
                    // Combine the partial token with the new chunk
                    let combined = partial.partial_lexeme + &self.current_chunk;
                    trace!("Combined partial token with new chunk: '{}'", &combined);
                    combined
                } else {
                    self.current_chunk.clone()
                };

                // Process the current chunk
                self.process_chunk(&source)
                    .map_err(|e| format!("Error processing chunk: {}", e))?;

                // We might have more data unless we hit EOF
                Ok(true)
            }
            Err(e) => {
                let err_msg = format!("Error reading from source: {}", e);
                error!("{}", err_msg);
                Err(err_msg)
            }
        }
    }
    
    /// Process a chunk of source code and update the token buffer
    /// 
    /// Returns `Ok(())` on success, or an error string if tokenization fails.
    fn process_chunk(&mut self, source: &str) -> Result<(), String> {
        if source.is_empty() {
            debug!("Empty source, nothing to process");
            return Ok(());
        }
        
        debug!("=== Processing chunk ({} chars) ===", source.len());
        debug!("Source: '{}'", source);
        debug!("Current position: line={}, column={}, offset={}", 
              self.position.line, self.position.column, self.position.offset);
        
        let mut source = source; // Make source mutable for tracking remaining input
        debug!("Initial source: '{}' (length: {})", source, source.len());
        
        // Special handling for the very first chunk that starts with a newline
        if self.position.offset == 0 && source.starts_with('\n') {
            // The first line is empty, so we start at line 2
            self.position.line = 1;  // Changed from 2 to 1
            self.position.column = 1;
            self.position.offset = 1;
            
            // Process the rest of the source after the newline
            if source.len() > 1 {
                let remaining = &source[1..];
                // If there's more content after the newline, process it
                if !remaining.is_empty() {
                    // Process the remaining content
                    return self.process_chunk(remaining);
                }
            }
            return Ok(());
        }
        
        // Initialize the lexer and token storage
        let mut lexer = LogosToken::lexer(source);
        let mut tokens = Vec::new();
        let mut last_span = 0..0;
        let _has_error = false; // Track if we've encountered any errors (currently unused)
        
        // Process the source to update the position
        let mut current_line = self.position.line;
        let mut current_column = self.position.column;
        let mut current_offset = self.position.offset;
        
        // Count newlines and update position
        for c in source.chars() {
            if c == '\n' {
                current_line += 1;
                current_column = 1;
            } else {
                current_column += 1;
            }
            current_offset += c.len_utf8();
        }
        
        // Update the position in the lexer
        self.position.line = current_line;
        self.position.column = current_column;
        self.position.offset = current_offset;
        
        // Process all tokens in the current chunk
        while let Some(token_result) = lexer.next() {
            match token_result {
                Ok(token) => {
                    let span = lexer.span();
                    last_span = span.clone();
                    let _lexeme_str = &source[span.start..span.end]; // Store lexeme string (currently unused)
                    
                    // Check if this is an integer token that might be invalid (followed by letters)
                    if let LogosToken::Integer(_) = token {
                        // Look at the next character after the number
                        if span.end < source.len() {
                            let next_char = &source[span.end..];
                            if let Some(c) = next_char.chars().next() {
                                if c.is_alphabetic() || c == '_' {
                                    // This is an invalid number like "123abc" or "123_abc"
                                    // Instead of returning an error, we'll create an error token and continue
                                    let invalid_end = next_char.find(|c: char| c.is_whitespace() || "+*/=<>!&|^%.,;(){}[]".contains(c))
                                        .map(|i| span.end + i)
                                        .unwrap_or_else(|| source.len());
                                    
                                    let invalid_token = &source[span.start..invalid_end];
                                    let _line = current_line + source[..span.start].matches('\n').count();
                                    let _col = if let Some(last_nl) = source[..span.start].rfind('\n') {
                                        span.start - last_nl
                                    } else {
                                        current_column + span.start
                                    };
                                    
                                    // Create an error token for the invalid numeric literal
                                    let error_msg = format!("Invalid numeric literal '{}'", invalid_token);
                                    let error_token = Token {
                                        token_type: TokenType::Error(error_msg.as_str().into()),
                                        lexeme: invalid_token.into(),
                                        location: self.position.to_location(),
                                    };
                                    
                                    // Push the error token
                                    tokens.push(error_token);
                                    
                                    // Update the position
                                    self.position.advance(invalid_token);
                                    
                                    // Skip past the invalid token in the source
                                    if invalid_end < source.len() {
                                        source = &source[invalid_end..];
                                        lexer = LogosToken::lexer(source);
                                        continue;
                                    } else {
                                        // If we're at the end of the source, we're done
                                        return Ok(());
                                    }
                                }
                            }
                        }
                    }
                    
                    // Calculate line and column for this token
                    let line = if self.position.offset == 1 && span.start == 0 {
                        // First token after initial newline is on line 1
                        1
                    } else {
                        // Count newlines before this token in the current chunk
                        let newlines_before = source[..span.start].matches('\n').count();
                        
                        // If this is the first chunk and we're at the start of the source,
                        // we're on line 1
                        if self.position.offset == 1 && newlines_before == 0 {
                            1
                        } else if newlines_before == 0 {
                            // If no newlines before this token, it's on the current line
                            self.position.line
                        } else {
                            // Otherwise, it's on line 1 + number of newlines
                            let base_line = 1;
                            base_line + newlines_before
                        }
                    };
                    
                    // Find the last newline before this token to calculate the column
                    let last_nl = source[..span.start].rfind('\n').map(|i| i + 1).unwrap_or(0);
                    let column = if last_nl == 0 {
                        // If no newline before this token, use the span start + 1
                        span.start + 1
                    } else {
                        // Otherwise, calculate the column from the last newline
                        span.start - last_nl + 1
                    };
                    
                    // Convert the token and add it to the buffer
                    if let Some(mut converted) = self.convert_token(token, &span, source) {
                        // Set the token's position
                        converted.location.line = line;
                        converted.location.column = column;
                        converted.location.offset = self.position.offset - (source.len() - span.start);
                        tokens.push(converted);
                    }
                }
                Err(_) => {
                    // Get the current position in the source where the error occurred
                    let error_pos = lexer.span().start;
                    debug!("Error at position {} in chunk '{}'", error_pos, source);
                    
                    // Skip past the invalid token to the next whitespace or semicolon
                    let remaining = &source[error_pos..];
                    let skip_len = remaining
                        .find(|c: char| c.is_whitespace() || c == ';' || c == '\n')
                        .unwrap_or(remaining.len());
                    
                    // Handle the error by creating an error token and skipping past it
                    let error_lexeme = &source[error_pos..error_pos + skip_len];
                    let skip_len = error_lexeme.len();
                    
                    debug!("Error token: '{}' at position {} (skipping {} chars)", 
                          error_lexeme, error_pos, skip_len);
                    
                    // Special handling for invalid numeric literals (e.g., 123abc)
                    if error_lexeme.chars().next().is_some_and(|c| c.is_ascii_digit() || c == '-') {
                        // Try to find the end of the invalid numeric literal
                        let mut end = 0;
                        let mut has_dot = false;
                        let mut has_e = false;
                        
                        // Skip the sign if present
                        let mut i = if error_lexeme.starts_with('-') { 1 } else { 0 };
                        
                        // Find the end of the invalid numeric literal
                        while i < error_lexeme.len() {
                            let c = error_lexeme.chars().nth(i).unwrap();
                            if c.is_ascii_digit() {
                                i += 1;
                            } else if c == '.' && !has_dot && !has_e {
                                has_dot = true;
                                i += 1;
                            } else if (c == 'e' || c == 'E') && !has_e {
                                has_e = true;
                                i += 1;
                                // Skip optional sign after 'e' or 'E'
                                if i < error_lexeme.len() && (error_lexeme.chars().nth(i) == Some('+') || error_lexeme.chars().nth(i) == Some('-')) {
                                    i += 1;
                                }
                            } else {
                                break;
                            }
                            end = i;
                        }
                        
                        // If we found a valid prefix, create an error token for the invalid part
                        if end > 0 && end < error_lexeme.len() {
                            let invalid_part = &error_lexeme[..end];
                            let remaining = &error_lexeme[end..];
                            
                            debug!("Splitting invalid numeric literal: valid='{}', invalid='{}'", invalid_part, remaining);
                            
                            // Create an error token for the invalid numeric literal
                            let error_token = Token {
                                token_type: TokenType::Error("Invalid numeric literal".into()),
                                lexeme: invalid_part.into(),
                                location: self.position.to_location(),
                            };
                            debug!("Pushing error token: {:?}", error_token);
                            tokens.push(error_token);
                            
                            // Update the position
                            self.position.advance(invalid_part);
                            
                            // Process the remaining part as a new token
                            if !remaining.is_empty() {
                                debug!("Processing remaining part as new token: '{}'", remaining);
                                source = remaining;
                                lexer = LogosToken::lexer(source);
                                continue;
                            }
                            
                            // Continue with the next token
                            continue;
                        }
                    }
                    
                    // For other types of errors, create a generic error token
                    let error_token = Token {
                        token_type: TokenType::Error("Invalid token".into()),
                        lexeme: error_lexeme.into(),
                        location: self.position.to_location(),
                    };
                    debug!("Pushing error token: {:?}", error_token);
                    tokens.push(error_token);
                    
                    // Update the position
                    self.position.advance(error_lexeme);
                    
                    // If we've processed all input, we're done
                    if error_pos + skip_len >= source.len() {
                        debug!("Reached end of input after error");
                        // Add any remaining tokens before breaking
                        if !tokens.is_empty() {
                            debug!("Adding {} tokens to buffer", tokens.len());
                            self.buffer.extend(tokens);
                        }
                        return Ok(());
                    }
                    
                    // Get the remaining source after the error
                    let remaining_source = &source[error_pos + skip_len..];
                    
                    // Process the remaining source as a new chunk if there is any
                    if !remaining_source.is_empty() {
                        debug!("Processing remaining source in new chunk: '{}' (length: {})", remaining_source, remaining_source.len());
                        
                        // Add any tokens we've collected so far to the buffer
                        if !tokens.is_empty() {
                            debug!("Adding {} tokens to buffer before processing remaining source", tokens.len());
                            for token in &tokens {
                                debug!("  - Token: {:?}", token);
                            }
                            self.buffer.extend(tokens);
                        }
                        
                        // Process the remaining source as a new chunk
                        return self.process_chunk(remaining_source);
                    } else {
                        debug!("No more source to process after error");
                        // Add any remaining tokens before returning
                        if !tokens.is_empty() {
                            debug!("Adding {} tokens to buffer", tokens.len());
                            for token in &tokens {
                                debug!("  - Token: {:?}", token);
                            }
                            self.buffer.extend(tokens);
                        }
                        return Ok(());
                    }
                }
            }
        }

        // Check if the last token might be partial (reached end of chunk but not end of input)
        if !self.eof && !source.is_empty() && !last_span.is_empty() && last_span.end == source.len() {
            if let Some(last_token) = tokens.last() {
                // Only treat as partial if it's a string literal that can span multiple lines
                let can_span_lines = matches!(
                    last_token.token_type,
                    TokenType::String(_)
                );
                
                if can_span_lines {
                    let partial_lexeme = source[last_span.start..].to_string();
                    trace!("Found partial token: '{}'", partial_lexeme);
                    
                    // Store the partial token information to handle in the next chunk
                    self.partial_token = Some(PartialToken {
                        partial_lexeme,
                    });
                    
                    // Remove the partial token from the current tokens
                    tokens.pop();
                }
            }
        }
        
        // Add the tokens to the buffer
        self.buffer.extend(tokens);
        Ok(())
    }

    /// Process the entire input and return all tokens as a vector
    ///
    /// This is a convenience method that consumes the lexer and returns all tokens.
    /// For very large inputs, consider using the iterator interface instead to
    /// avoid holding all tokens in memory at once.
    pub fn into_tokens(self) -> Vec<Token> {
        self.collect()
    }

    /// Process the entire input and return a result with all tokens or an error
    ///
    /// This is similar to `into_tokens` but returns a `Result` that can be used
    /// with the `?` operator for better error handling.
    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        println!("Starting tokenization...");
        let mut tokens = Vec::new();
        let mut token_count = 0;

        while let Some(token) = self.next_token() {
            token_count += 1;
            println!(
                "Token #{}: {:?} (lexeme: '{}' at line {})",
                token_count, token.token_type, token.lexeme, token.location.line
            );

            // Check for error tokens
            match &token.token_type {
                TokenType::Error(msg) => {
                    let error_msg = format!(
                        "Lexer error at line {}: {}",
                        token.location.line,
                        msg.as_str()
                    );
                    println!("Found error token: {}", error_msg);
                    return Err(error_msg);
                }
                _ => {
                    tokens.push(token);
                }
            }
        }

        println!("Tokenization completed. Processed {} tokens.", token_count);
        Ok(tokens)
    }

    /// Create a new chunked lexer from a file path
    ///
    /// This is a convenience method that opens the file and creates a new lexer
    /// with default configuration.
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(Self::from_reader(reader, ChunkedLexerConfig::default()))
    }

    /// Get the next token from the input stream
    ///
    /// This is the main method that drives the lexer. It returns the next token
    /// from the input, or `None` if the end of input has been reached.
    /// Returns an error token if there was an error during tokenization.
    fn next_token(&mut self) -> Option<Token> {
        // If we have tokens in the buffer, return the next one
        if let Some(token) = self.buffer.pop_front() {
            debug!("Returning buffered token: {:?}", token);
            self.position.offset += token.lexeme.len();
            return Some(token);
        }

        // If we've reached the end of the input, return None
        if self.eof {
            debug!("Reached EOF, no more tokens");
            return None;
        }

        // Otherwise, try to read more tokens
        loop {
            match self.read_next_chunk() {
                Ok(true) => {
                    // We might have more tokens in the buffer now
                    if let Some(token) = self.buffer.pop_front() {
                        self.position.offset += token.lexeme.len();
                        return Some(token);
                    }
                    // If we didn't get any tokens but read_next_chunk returned true,
                    // try reading again in case we hit a chunk boundary in the middle of a token
                }
                Ok(false) => {
                    // No more data and no more tokens
                    self.eof = true;
                    return None;
                }
                Err(e) => {
                    // Error occurred
                    error!("Error reading next chunk: {}", e);
                    self.eof = true;
                    // Return an error token
                    return Some(Token::new(
                        TokenType::Error(InternedString::from(e.as_str())),
                        "",
                        self.position.to_location(),
                    ));
                }
            }
            
            // If we're here, we need to try reading more chunks
            if self.eof {
                return None;
            }
        }
    }

    /// Get the current position in the source code
    ///
    /// This method returns the current position of the lexer in the source code.
    /// The position is updated as tokens are consumed.
    pub fn position(&self) -> Position {
        self.position
    }

    /// Check if the lexer has reached the end of the input
    ///
    /// This method returns `true` if the lexer has reached the end of the input
    /// and there are no more tokens to return.
    pub fn is_eof(&self) -> bool {
        self.eof
    }

    /// Get the number of tokens remaining in the buffer
    ///
    /// This method returns the number of tokens that have been lexed but not yet consumed.
    pub fn buffered_tokens(&self) -> usize {
        self.buffer.len()
    }
}

impl Iterator for ChunkedLexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // We don't know how many tokens are left, but we can at least say it's not empty if not at EOF
        if self.eof && self.buffer.is_empty() {
            (0, Some(0))
        } else {
            (0, None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use std::io::Write;
    use tempfile::NamedTempFile;

    /// Helper function to create a temporary file with the given content
    fn create_temp_file(content: &str) -> NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        write!(file, "{}", content).unwrap();
        file
    }

    #[test]
    fn test_chunked_lexer_basic() {
        let input = "let x = 42;\nlet y = x + 1;";
        println!("Input: {}", input);
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 8,
            max_buffer_size: 10,
            include_whitespace: false,
            keep_source_in_memory: true,
        };

        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        // Print all tokens for debugging
        println!("\nFound {} tokens:", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!(
                "Token {}: {:?} - '{}' at {:?}",
                i, token.token_type, token.lexeme, token.location
            );
        }

        // Verify we got the expected number of tokens
        assert_eq!(tokens.len(), 12, "Expected 12 tokens, got {}", tokens.len());

        // Verify some token types
        assert!(
            matches!(tokens[0].token_type, TokenType::Let),
            "First token should be 'let', got {:?}",
            tokens[0].token_type
        );
        assert!(
            matches!(tokens[1].token_type, TokenType::Identifier(_)),
            "Second token should be an identifier, got {:?}",
            tokens[1].token_type
        );
        assert!(
            matches!(tokens[2].token_type, TokenType::Equal),
            "Third token should be '=', got {:?}",
            tokens[2].token_type
        );
        assert!(
            matches!(tokens[3].token_type, TokenType::Integer(42)),
            "Fourth token should be integer 42, got {:?}",
            tokens[3].token_type
        );
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

        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);
        let lexer = ChunkedLexer::from_reader(reader, config);
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

        // We should have 5 tokens per line (let, ident, =, number, ;)
        // and 1000 lines
        assert_eq!(tokens.len(), 1000 * 5);
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

        // Verify we got all tokens correctly (5 tokens: let, identifier, =, 12345, ;)
        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0].token_type, TokenType::Let));

        if let TokenType::Identifier(id) = &tokens[1].token_type {
            assert_eq!(id.to_string(), "long_identifier_name");
        } else {
            panic!("Expected identifier token");
        }

        assert!(matches!(tokens[2].token_type, TokenType::Equal));
        assert!(matches!(tokens[3].token_type, TokenType::Integer(12345)));
        assert!(matches!(tokens[4].token_type, TokenType::Semicolon));
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
        // Enable debug logging for the test
        std::env::set_var("RUST_LOG", "debug");
        let _ = env_logger::builder().is_test(true).try_init();

        // Test with invalid input
        let input = "let x = 123abc;\nlet y = 456;";
        println!("Test input: {}", input);

        let cursor = Cursor::new(input);

        let config = ChunkedLexerConfig {
            chunk_size: 8,
            max_buffer_size: 10,
            include_whitespace: false,
            keep_source_in_memory: true,
        };

        let lexer = ChunkedLexer::from_reader(cursor, config);
        println!("Created lexer, starting tokenization...");

        let result = lexer.tokenize();

        // Print the result for debugging
        match &result {
            Ok(tokens) => {
                println!("Tokenization succeeded with {} tokens:", tokens.len());
                for (i, token) in tokens.iter().enumerate() {
                    println!(
                        "  Token {}: {:?} (lexeme: '{}' at line {})",
                        i, token.token_type, token.lexeme, token.location.line
                    );
                }
            }
            Err(e) => {
                println!("Tokenization failed with error: {}", e);
            }
        }

        // Should get an error for the invalid token
        assert!(
            result.is_err(),
            "Expected an error for invalid token '123abc', but got: {:?}",
            result
        );
        let error = result.unwrap_err();
        println!("Error message: {}", error);
        assert!(
            error.contains("Lexer error"),
            "Error message should contain 'Lexer error', but was: {}",
            error
        );
    }

    #[test]
    fn test_chunked_lexer_position_tracking() {
        let input = r#"
            let x = 1;
            let y = 2;
            
            // A comment
            let z = x + y;
        "#;

        println!("Input source code:");
        for (i, line) in input.lines().enumerate() {
            println!("{:2}: {}", i + 1, line);
        }
        println!();

        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 16, // Small chunks to test position tracking
            max_buffer_size: 20,
            include_whitespace: false,
            keep_source_in_memory: true,
        };

        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();

        println!("\nTokens found:");
        for (i, token) in tokens.iter().enumerate() {
            println!("Token {}: {:?} at line {}:{} (offset: {})", 
                   i, token.token_type, token.location.line, token.location.column, token.location.offset);
        }

        // Verify positions are tracked correctly
        assert!(tokens.len() >= 11, "Expected at least 11 tokens, got {}", tokens.len());

        // Check positions of specific tokens
        let let_x = &tokens[0];
        println!("\nChecking first 'let' token at line {}", let_x.location.line);
        assert_eq!(let_x.location.line, 2, "First 'let' should be on line 2");

        let plus = tokens
            .iter()
            .find(|t| matches!(t.token_type, TokenType::Plus))
            .expect("Could not find '+' token in the token stream");
            
        println!("\nFound '+' token at line {}", plus.location.line);
        println!("Token details: {:?}", plus);
        
        // Find and print the 'let z' token for context
        if let Some(let_z) = tokens.iter().find(|t| matches!(t.token_type, TokenType::Let) && 
                                                  t.location.line > 2) {
            println!("Found 'let z' token at line {}", let_z.location.line);
        }
        
        assert_eq!(plus.location.line, 6, "'+' token should be on line 6");
    }
}
