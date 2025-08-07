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
//! let config = ChunkedLexerConfig::default();
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
//! use std::fs::File;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let path = Path::new("large_file.medi");
//! let file = File::open(path)?;
//! let lexer = ChunkedLexer::from_reader(file, Default::default());
//!
//! // Process tokens in a streaming fashion
//! for token in lexer {
//!     // Process each token
//!     println!("Token: {:?}", token);
//! }
//! # Ok(())
//! # }
//! ```

use log::warn;
use logos::Logos;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, Read, Seek};
use std::ops::AddAssign;
use std::path::Path;

use crate::string_interner::InternedString;
use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// Configuration for the chunked lexer
#[derive(Debug, Clone, Copy)]
pub struct ChunkedLexerConfig {
    /// Size of each chunk in bytes
    pub chunk_size: usize,
}

impl Default for ChunkedLexerConfig {
    fn default() -> Self {
        Self {
            chunk_size: 8 * 1024, // 8KB chunks by default
        }
    }
}

/// A lexer that processes source code in chunks from a file
pub struct ChunkedLexer {
    /// The source file being lexed
    source: Box<dyn Read>,
    /// Buffer for tokens that have been lexed but not yet consumed
    buffer: VecDeque<Token>,
    /// Configuration for the lexer
    config: ChunkedLexerConfig,
    /// Current position in the source
    pub position: Position,
    /// Whether we've reached the end of the input
    eof: bool,
    /// Internal buffer for partial tokens across chunks
    partial_token: Option<PartialToken>,
}

/// Tracks the current position in the source code
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Position {
    /// The current line number (1-based)
    pub line: usize,
    /// The current column number (1-based)
    pub column: usize,
    /// The byte offset from the start of the file
    pub offset: usize,
}

impl From<Position> for Location {
    fn from(pos: Position) -> Self {
        Self {
            line: pos.line,
            column: pos.column,
            offset: pos.offset,
        }
    }
}

impl AddAssign<usize> for Position {
    fn add_assign(&mut self, rhs: usize) {
        self.offset += rhs;
    }
}

impl Position {
    /// Create a new position with the given line, column, and offset
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        let line = if line == 0 { 1 } else { line };
        let column = if column == 0 { 1 } else { column };
        Self {
            line,
            column,
            offset,
        }
    }

    /// Update the position based on the processed text
    pub fn advance(&mut self, text: &str) {
        for c in text.chars() {
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

/// Represents a token that spans multiple chunks
#[derive(Debug, Clone)]
struct PartialToken {
    /// The partial lexeme from the current chunk
    partial_lexeme_bytes: Vec<u8>,
    /// The starting position of the partial token in the source
    start: Position,
}

impl ChunkedLexer {
    /// Create a new chunked lexer from a file path
    pub fn from_file(path: &Path) -> io::Result<Self> {
        let file = File::open(path)?;
        Ok(Self::from_reader(file, Default::default()))
    }

    /// Create a new chunked lexer from a reader
    pub fn from_reader(reader: impl Read + 'static, config: ChunkedLexerConfig) -> Self {
        Self {
            source: Box::new(reader),
            buffer: VecDeque::new(),
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

    /// Tokenizes a chunk of source code into tokens
    fn tokenize_chunk(
        &self,
        chunk: &str,
        start_pos: Position,
    ) -> (Vec<Token>, Position, Option<String>) {
        let mut tokens = Vec::new();
        let mut lexer = LogosToken::lexer(chunk);
        lexer.extras = start_pos;

        let mut last_span_end = 0;

        while let Some(token_result) = lexer.next() {
            let span = lexer.span();
            let slice = lexer.slice();
            last_span_end = span.end;

            let mut token_start_pos = start_pos;
            token_start_pos.advance(&chunk[..span.start]);

            let token_type = match token_result {
                Ok(t) => t,
                Err(_) => {
                    tokens.push(Token::new(
                        TokenType::Error(InternedString::from("Invalid token")),
                        slice,
                        token_start_pos.into(),
                    ));
                    continue;
                }
            };

            if !slice.is_empty() {
                if let Some(token) =
                    self.convert_token(token_type, slice, token_start_pos.into())
                {
                    tokens.push(token);
                }
            }
        }

        let mut end_pos = start_pos;
        end_pos.advance(&chunk[..last_span_end]);

        let partial_string = if last_span_end < chunk.len() {
            Some(chunk[last_span_end..].to_string())
        } else {
            None
        };

        (tokens, end_pos, partial_string)
    }

    /// Convert a Logos token to our internal token type
    fn convert_token(
        &self,
        token_type: LogosToken,
        lexeme: &str,
        location: Location,
    ) -> Option<Token> {
        let token_type = match token_type {
            LogosToken::Identifier(ident) => {
                if let Some(keyword) = Self::get_keyword(&ident) {
                    keyword
                } else {
                    TokenType::Identifier(InternedString::from(ident.as_str()))
                }
            }
            LogosToken::String(s) => TokenType::String(InternedString::from(s.as_str())),
            LogosToken::NegativeInteger(n) => TokenType::NegativeInteger(n),
            LogosToken::Integer(n) => TokenType::Integer(n),
            LogosToken::Float(n) => TokenType::Float(n),
            LogosToken::Bool(b) => TokenType::Boolean(b),
            LogosToken::Plus => TokenType::Plus,
            LogosToken::Minus => TokenType::Minus,
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
            LogosToken::Of => TokenType::Of,
            LogosToken::Per => TokenType::Per,
            LogosToken::Underscore => TokenType::Underscore,
            LogosToken::ICD10(code) => TokenType::ICD10(InternedString::from(code.as_str())),
            LogosToken::LOINC(code) => TokenType::LOINC(InternedString::from(code.as_str())),
            LogosToken::SNOMED(code) => TokenType::SNOMED(InternedString::from(code.as_str())),
            LogosToken::CPT(code) => TokenType::CPT(InternedString::from(code.as_str())),
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
            LogosToken::Error => TokenType::Error(InternedString::from("Invalid token")),
            _ => {
                warn!("Unhandled token type: {:?}", token_type);
                return None;
            }
        };

        Some(Token::new(token_type, lexeme, location))
    }

    /// Reads the next chunk of source code and tokenize it
    fn read_next_chunk(&mut self) -> Option<Vec<Token>> {
        if self.eof && self.partial_token.is_none() {
            return None;
        }

        let (mut chunk_bytes, start_pos) =
            self.partial_token.take().map_or((Vec::new(), self.position), |p| {
                (p.partial_lexeme_bytes, p.start)
            });

        if !self.eof {
            let mut buffer = vec![0; self.config.chunk_size];
            if let Ok(bytes_read) = self.source.read(&mut buffer) {
                if bytes_read == 0 {
                    self.eof = true;
                } else {
                    chunk_bytes.extend_from_slice(&buffer[..bytes_read]);
                }
            } else {
                self.eof = true;
            }
        }

        if chunk_bytes.is_empty() {
            return None;
        }

        let (chunk_str, remainder_bytes) = match std::str::from_utf8(&chunk_bytes) {
            Ok(s) => (s, &[][..]),
            Err(e) => {
                let valid_len = e.valid_up_to();
                (
                    std::str::from_utf8(&chunk_bytes[..valid_len]).unwrap(),
                    &chunk_bytes[valid_len..],
                )
            }
        };

        if chunk_str.is_empty() && !remainder_bytes.is_empty() {
            self.partial_token = Some(PartialToken {
                partial_lexeme_bytes: remainder_bytes.to_vec(),
                start: start_pos,
            });
            return Some(vec![]);
        }

        let (tokens, new_pos, partial_str) = self.tokenize_chunk(chunk_str, start_pos);

        let mut next_partial_bytes = partial_str.map_or(Vec::new(), |s| s.into_bytes());
        next_partial_bytes.extend_from_slice(remainder_bytes);

        if !next_partial_bytes.is_empty() {
            self.partial_token = Some(PartialToken {
                partial_lexeme_bytes: next_partial_bytes,
                start: new_pos,
            });
        } else {
            self.partial_token = None;
        }
        self.position = new_pos;

        Some(tokens)
    }

    /// Process the entire input and return all tokens as a vector
    pub fn into_tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next() {
            tokens.push(token);
        }
        tokens
    }

    /// Process the entire input and return a result with all tokens or an error
    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next() {
            if let TokenType::Error(msg) = &token.token_type {
                let error_msg = format!(
                    "Lexer error at line {}: {}",
                    token.location.line,
                    msg.as_str()
                );
                return Err(error_msg);
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    /// Get the next token from the source
    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.buffer.pop_front() {
            return Some(token);
        }

        if self.eof {
            return None;
        }

        if let Some(tokens) = self.read_next_chunk() {
            self.buffer.extend(tokens);
        }

        self.buffer.pop_front()
    }
}

impl Iterator for ChunkedLexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
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
        file.seek(std::io::SeekFrom::Start(0)).unwrap();
        file
    }

    #[test]
    fn test_chunked_lexer_basic() {
        let input = "let x = 42;\nlet y = x + 1;";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 8,
        };

        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();
        assert_eq!(tokens.len(), 12);
        assert!(matches!(tokens[0].token_type, TokenType::Let));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::Equal));
        assert!(matches!(tokens[3].token_type, TokenType::Integer(42)));
    }

    #[test]
    fn test_chunked_lexer_with_file() {
        let content = r#"
            // A simple function
            fn add(a: int, b: int) -> int {
                return a + b;
            }
            
            let result = add(5, 3);
        "#;
        let file = create_temp_file(content);
        let lexer = ChunkedLexer::from_file(file.path()).unwrap();
        let tokens: Vec<_> = lexer.collect();

        assert!(!tokens.is_empty());
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Fn));
        assert!(token_types.contains(&&TokenType::Return));
        assert!(token_types.contains(&&TokenType::Let));
    }

    #[test]
    fn test_chunked_lexer_large_input() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut large_input = String::new();
        for i in 0..1000 {
            large_input.push_str(&format!("let x{} = {};\n", i, i));
        }

        let cursor = Cursor::new(large_input);
        let config = ChunkedLexerConfig {
            chunk_size: 128,
        };
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();

        assert_eq!(
            tokens.len(),
            5000,
            "Expected 5000 tokens for 1000 lines of code"
        );
    }

    #[test]
    fn test_chunked_lexer_partial_tokens() {
        let input = "let long_identifier_name = 12345;\n";
        let config = ChunkedLexerConfig {
            chunk_size: 10,
        };
        let cursor = Cursor::new(input);
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();

        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0].token_type, TokenType::Let));
        if let TokenType::Identifier(id) = &tokens[1].token_type {
            assert_eq!(id.as_str(), "long_identifier_name");
        } else {
            panic!("Expected identifier token");
        }
        assert!(matches!(tokens[2].token_type, TokenType::Equal));
        assert!(matches!(tokens[3].token_type, TokenType::Integer(12345)));
    }

    #[test]
    fn test_chunked_lexer_string_literals() {
        let input = r#"
            let greeting = "Hello, world! This is a long string that might span chunks";
            let empty = "";
            let escaped = "Line 1\nLine 2\nLine 3";
        "#;
        let config = ChunkedLexerConfig {
            chunk_size: 16,
        };
        let cursor = Cursor::new(input);
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens: Vec<_> = lexer.collect();
        let string_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::String(_)))
            .collect();
        assert_eq!(string_tokens.len(), 3);
    }

    #[test]
    fn test_chunked_lexer_error_handling() {
        let _ = env_logger::builder().is_test(true).try_init();
        let input = "let x = @;\nlet y = 456;";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 8,
        };
        let lexer = ChunkedLexer::from_reader(cursor, config);
        let result = lexer.tokenize();
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.contains("Lexer error"));
    }

    #[test]
    fn test_chunked_lexer_position_tracking() {
        let input = "let x = 1;\nlet y = 2;\n// A comment\nlet z = x + y;";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig {
            chunk_size: 16,
        };
        let mut lexer = ChunkedLexer::from_reader(cursor, config);

        let token = lexer.next().unwrap(); // let
        assert_eq!(
            token.location,
            Location {
                line: 1,
                column: 1,
                offset: 0
            }
        );

        let token = lexer.next().unwrap(); // x
        assert_eq!(
            token.location,
            Location {
                line: 1,
                column: 5,
                offset: 4
            }
        );
    }
}
