//! Medi lexical analyzer module
//!
//! This module provides lexical analysis for the Medi programming language,
//! converting source code into a stream of tokens for the parser.
//!
//! # Features
//! - Tokenization of Medi source code
//! - Support for Unicode identifiers
//! - Medical code recognition (ICD-10, LOINC, SNOMED, CPT)
//! - Efficient string interning
//! - Streaming and chunked lexing for large files
//! - Comprehensive error reporting with source locations

#![warn(missing_docs)]
#![warn(rustdoc::missing_crate_level_docs)]

/// Chunked lexer implementation for processing large files in fixed-size chunks.
pub mod chunked_lexer;

/// Main lexer implementation that converts source code into tokens.
pub mod lexer;

/// Token definitions and patterns for the Logos lexer.
pub mod logos_token;

/// Streaming lexer implementation for efficient token streaming.
pub mod streaming_lexer;

/// String interning for efficient string storage and comparison.
pub mod string_interner;

/// Token types and related data structures.
pub mod token;

// Re-export the main types for convenience
pub use chunked_lexer::{ChunkedLexer, ChunkedLexerConfig};
pub use lexer::Lexer;
pub use logos_token::LogosToken;
pub use streaming_lexer::{LexerConfig, StreamingLexer};
pub use string_interner::InternedString;
pub use token::{Location, Token, TokenType};
