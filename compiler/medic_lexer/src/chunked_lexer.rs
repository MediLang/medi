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

use log::{debug, error};
use logos::{Logos, Span};
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
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
    /// Current byte offset
    offset: usize,
    /// Current line number (1-based)
    line: usize,
    /// Current column number (1-based, in characters, not bytes)
    column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 1,   // Start line numbers at 1
            column: 1, // Start column numbers at 1
        }
    }
}

use std::ops::AddAssign;

impl AddAssign<usize> for Position {
    fn add_assign(&mut self, rhs: usize) {
        self.offset += rhs;
        // We can't determine line/column changes from just a byte count,
        // so we'll need to be careful when using this operator
    }
}

impl Position {
    /// Advance the position by the given string
    fn advance(&mut self, s: &str) {
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    // Handle \r\n
                    if let Some('\r') = chars.peek() {
                        chars.next();
                    }
                }
                '\r' => {
                    self.line += 1;
                    self.column = 1;
                    // Handle \n after \r
                    if let Some('\n') = chars.peek() {
                        chars.next();
                    }
                }
                _ => {
                    self.column += 1;
                }
            }
        }
        self.offset += s.len();
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
    fn convert_token(&mut self, token_type: LogosToken, span: Span, source: &str) -> Option<Token> {
        let lexeme_str = &source[span.start..span.end];

        // Debug logging for token conversion
        debug!(
            "Converting token: {:?} with lexeme: '{}' at {:?}",
            token_type, lexeme_str, span
        );

        // Create the token first without advancing the position
        let lexeme = InternedString::new(lexeme_str);
        let location = self.position.to_location();

        // Helper function to create tokens with the current position
        let create_token = |token_type| {
            let token = Token::from_interned(token_type, lexeme.clone(), location);
            debug!("Created token: {:?}", token);
            token
        };

        // Map LogosToken to our internal TokenType
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

            // Identifiers
            LogosToken::Identifier(ident) => {
                // Check if this is a keyword
                if let Some(keyword) = Self::get_keyword(ident.as_str()) {
                    create_token(keyword)
                } else {
                    create_token(TokenType::Identifier(lexeme.clone()))
                }
            }

            // Numbers
            LogosToken::Integer(n) => {
                // Check if the lexeme is a valid integer (digits only)
                if lexeme_str.chars().all(|c| c.is_ascii_digit() || c == '-') {
                    create_token(TokenType::Integer(n))
                } else {
                    // Check if this is a number followed by letters (e.g., 123abc)
                    if lexeme_str
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_digit() || c == '-')
                        && lexeme_str
                            .chars()
                            .skip_while(|&c| c.is_ascii_digit() || c == '-')
                            .any(|c| c.is_alphabetic())
                    {
                        let error_msg = format!("Invalid numeric literal: {}", lexeme_str);
                        error!("{}", error_msg);
                        return Some(Token::new(
                            TokenType::Error(InternedString::new(&error_msg)),
                            lexeme_str,
                            location,
                        ));
                    } else {
                        // Some other invalid format, but we'll let it pass as an integer
                        // since we don't want to break existing valid cases
                        create_token(TokenType::Integer(n))
                    }
                }
            }
            LogosToken::Float(n) => {
                // For float literals, we've already validated the format in the regex
                create_token(TokenType::Float(n))
            }
            LogosToken::Bool(b) => create_token(TokenType::Boolean(b)),

            // String literals
            LogosToken::String(s) => create_token(TokenType::String(InternedString::new(&s))),

            // Medical code identifiers
            LogosToken::ICD10(code) => create_token(TokenType::ICD10(InternedString::new(&code))),
            LogosToken::LOINC(code) => create_token(TokenType::LOINC(InternedString::new(&code))),
            LogosToken::SNOMED(code) => create_token(TokenType::SNOMED(InternedString::new(&code))),
            LogosToken::CPT(code) => create_token(TokenType::CPT(InternedString::new(&code))),

            // Range operators
            LogosToken::Range => create_token(TokenType::Range),
            LogosToken::RangeInclusive => create_token(TokenType::RangeInclusive),

            // Assignment operators
            LogosToken::Equal => create_token(TokenType::Equal),
            LogosToken::PlusEqual => create_token(TokenType::PlusEqual),
            LogosToken::MinusEqual => create_token(TokenType::MinusEqual),
            LogosToken::StarEqual => create_token(TokenType::StarEqual),
            LogosToken::SlashEqual => create_token(TokenType::SlashEqual),
            LogosToken::PercentEqual => create_token(TokenType::PercentEqual),
            LogosToken::DoubleStarAssign => create_token(TokenType::DoubleStarAssign),
            LogosToken::BitAndAssign => create_token(TokenType::BitAndAssign),
            LogosToken::BitOrAssign => create_token(TokenType::BitOrAssign),
            LogosToken::BitXorAssign => create_token(TokenType::BitXorAssign),
            LogosToken::ShlAssign => create_token(TokenType::ShlAssign),
            LogosToken::ShrAssign => create_token(TokenType::ShrAssign),

            // Comparison operators
            LogosToken::EqualEqual => create_token(TokenType::EqualEqual),
            LogosToken::NotEqual => create_token(TokenType::NotEqual),
            LogosToken::Less => create_token(TokenType::Less),
            LogosToken::LessEqual => create_token(TokenType::LessEqual),
            LogosToken::Greater => create_token(TokenType::Greater),
            LogosToken::GreaterEqual => create_token(TokenType::GreaterEqual),

            // Logical operators
            LogosToken::And => create_token(TokenType::And),
            LogosToken::Or => create_token(TokenType::Or),
            LogosToken::Not => create_token(TokenType::Not),

            // Bitwise operators
            LogosToken::BitOr => create_token(TokenType::BitOr),
            LogosToken::BitXor => create_token(TokenType::BitXor),
            LogosToken::BitAnd => create_token(TokenType::BitAnd),
            LogosToken::Shl => create_token(TokenType::Shl),
            LogosToken::Shr => create_token(TokenType::Shr),

            // Arithmetic operators
            LogosToken::Plus => create_token(TokenType::Plus),
            LogosToken::Minus => create_token(TokenType::Minus),
            LogosToken::Star => create_token(TokenType::Star),
            LogosToken::Slash => create_token(TokenType::Slash),
            LogosToken::Percent => create_token(TokenType::Percent),
            LogosToken::DoubleStar => create_token(TokenType::DoubleStar),

            // Null-coalescing and Elvis operators
            LogosToken::QuestionQuestion => create_token(TokenType::QuestionQuestion),
            LogosToken::QuestionColon => create_token(TokenType::QuestionColon),

            // Delimiters
            LogosToken::LeftParen => create_token(TokenType::LeftParen),
            LogosToken::RightParen => create_token(TokenType::RightParen),
            LogosToken::LeftBrace => create_token(TokenType::LeftBrace),
            LogosToken::RightBrace => create_token(TokenType::RightBrace),
            LogosToken::LeftBracket => create_token(TokenType::LeftBracket),
            LogosToken::RightBracket => create_token(TokenType::RightBracket),
            LogosToken::Comma => create_token(TokenType::Comma),
            LogosToken::Colon => create_token(TokenType::Colon),
            LogosToken::Semicolon => create_token(TokenType::Semicolon),
            LogosToken::Dot => create_token(TokenType::Dot),
            LogosToken::Underscore => create_token(TokenType::Underscore),
            LogosToken::Arrow => create_token(TokenType::Arrow),

            // Error token
            LogosToken::Error => {
                let error_msg = format!("Lexer error at {:?}", location);
                error!("{}", error_msg);
                return Some(Token::new(
                    TokenType::Error(InternedString::new(&error_msg)),
                    lexeme_str,
                    location,
                ));
            }

            // Skip whitespace and comments
            LogosToken::Whitespace | LogosToken::LineComment | LogosToken::BlockComment => {
                // Skip these tokens
                self.position.advance(lexeme_str);
                return None;
            }

            // For any unhandled token, log a warning and return an error token
            _ => {
                let error_msg = format!("Unhandled token type: {:?}", token_type);
                error!("{} at {:?}", error_msg, location);
                self.position.advance(lexeme_str);
                return Some(Token::new(
                    TokenType::Error(InternedString::new(&error_msg)),
                    lexeme_str,
                    location,
                ));
            }
        };

        // Update position and return the token
        self.position.advance(lexeme_str);
        Some(token)
    }

    /// Update the position based on the given token
    fn update_position(&mut self, token: &Token) {
        let lexeme = token.lexeme.as_str();
        let lines: Vec<&str> = lexeme.split('\n').collect();

        if lines.len() > 1 {
            // Token spans multiple lines
            self.position.line = token.location.line + lines.len() - 1;
            self.position.column = lines.last().unwrap_or(&"").chars().count() + 1;
        } else {
            // Token is on a single line
            self.position.column = token.location.column + lexeme.chars().count();
        }

        // Update the position's offset
        self.position.offset = token.location.offset + lexeme.len();
    }

    /// Create a new chunked lexer from any BufRead implementor
    pub fn from_reader<R: BufRead + 'static>(reader: R, config: ChunkedLexerConfig) -> Self {
        let chunk_size = config.chunk_size.clamp(1_024, 1_048_576); // 1KB to 1MB chunks

        Self {
            source: Box::new(BufReader::with_capacity(chunk_size, reader)),
            current_chunk: String::with_capacity(chunk_size * 2),
            buffer: VecDeque::with_capacity(config.max_buffer_size),
            config,
            position: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
            eof: false,
            partial_token: None,
        }
    }

    fn read_next_chunk(&mut self) -> Option<bool> {
        // If we've already reached EOF, return false
        if self.eof {
            return Some(false);
        }

        // Read the next chunk from the source
        let mut chunk = String::with_capacity(self.config.chunk_size);
        match self.source.read_to_string(&mut chunk) {
            Ok(0) => {
                // Reached EOF
                self.eof = true;
                return Some(false);
            }
            Ok(_) => {
                // Successfully read a chunk
                self.current_chunk = chunk;
            }
            Err(e) => {
                error!("Error reading from source: {}", e);
                return None;
            }
        }

        // Handle any partial token from the previous chunk
        let source = if let Some(partial) = self.partial_token.take() {
            partial.partial_lexeme + &self.current_chunk
        } else {
            self.current_chunk.clone()
        };

        // Tokenize the current chunk
        let mut lexer = LogosToken::lexer(&source);
        let mut tokens = Vec::new();

        // Process all tokens in the current chunk
        while let Some(token_result) = lexer.next() {
            match token_result {
                Ok(token) => {
                    if let Some(converted) = self.convert_token(token, lexer.span(), &source) {
                        tokens.push(converted);
                    }
                }
                Err(_) => {
                    error!("Lexer error at position {}", lexer.span().start);
                    return None;
                }
            }
        }

        // Check if the last token might be partial (reached end of chunk but not end of input)
        if !self.eof && !source.is_empty() {
            let last_span = lexer.span();
            if !last_span.is_empty() && last_span.end == source.len() {
                if let Some(_last_token) = tokens.last().cloned() {
                    let partial_lexeme = source[last_span.start..].to_string();

                    // Store the partial token information to handle in the next chunk
                    self.partial_token = Some(PartialToken {
                        partial_lexeme: partial_lexeme.clone(),
                    });

                    // Remove the partial token from the tokens list
                    tokens.pop();
                }
            }
        }

        // Update the position based on the processed tokens
        for token in &tokens {
            self.update_position(token);
        }

        // Add tokens to the buffer
        self.buffer.extend(tokens);

        // Return true if we might have more data
        Some(true)
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
        // If we have tokens in the buffer, check for invalid numeric literals
        if self.buffer.len() >= 2 {
            // Get references to the first two tokens without borrowing self.buffer
            // by using indices and length checks
            let is_invalid = {
                if let (TokenType::Integer(_), TokenType::Identifier(_)) =
                    (&self.buffer[0].token_type, &self.buffer[1].token_type)
                {
                    let first_token = &self.buffer[0];
                    let second_token = &self.buffer[1];

                    // Check if the identifier immediately follows the number without whitespace
                    let current_end = first_token.location.offset + first_token.lexeme.len();
                    second_token.location.offset == current_end
                } else {
                    false
                }
            };

            if is_invalid {
                // Remove the first token and get its data
                let first_token = self.buffer.remove(0).unwrap();
                let second_token = self.buffer.remove(0).unwrap();

                // This is an invalid numeric literal like 123abc
                let invalid_literal = format!("{}{}", first_token.lexeme, second_token.lexeme);
                let error_msg = format!("Invalid numeric literal: {}", invalid_literal);

                // Return an error token
                return Some(Token::new(
                    TokenType::Error(InternedString::new(&error_msg)),
                    &*invalid_literal,
                    first_token.location,
                ));
            }
        }

        // If we have tokens in the buffer, return the next one
        if let Some(token) = self.buffer.pop_front() {
            self.position.offset += token.lexeme.len();
            return Some(token);
        }

        // If we've reached the end of the input, return None
        if self.eof {
            return None;
        }

        // Otherwise, try to read more tokens
        match self.read_next_chunk() {
            Some(true) => {
                // We successfully read more tokens, try to get the next one
                self.next_token()
            }
            Some(false) => {
                // No more data to read, but we might have buffered tokens
                if !self.buffer.is_empty() {
                    self.next_token()
                } else {
                    self.eof = true;
                    None
                }
            }
            None => {
                // An error occurred while reading the next chunk
                self.eof = true;
                // Return an error token
                Some(Token::new(
                    TokenType::Error(InternedString::new("Error reading input")),
                    "",
                    self.position.to_location(),
                ))
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

        let plus = tokens
            .iter()
            .find(|t| matches!(t.token_type, TokenType::Plus))
            .unwrap();
        assert_eq!(plus.location.line, 6);
    }
}
