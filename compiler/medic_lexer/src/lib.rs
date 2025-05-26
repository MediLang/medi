//! Medi lexical analyzer module
//!
//! This module provides lexical analysis for the Medi programming language,
//! converting source code into a stream of tokens for the parser.

#![warn(missing_docs)]
#![warn(rustdoc::missing_crate_level_docs)]

pub mod chunked_lexer;
pub mod lexer;
pub mod logos_token;
pub mod streaming_lexer;
pub mod string_interner;
pub mod token;

// Re-export the main types for convenience
pub use chunked_lexer::{ChunkedLexer, ChunkedLexerConfig};
pub use lexer::Lexer;
pub use logos_token::LogosToken;
pub use streaming_lexer::{LexerConfig, StreamingLexer};
pub use string_interner::InternedString;
pub use token::{Location, Token, TokenType};
