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

use logos::Logos;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, Read};
use std::ops::AddAssign;
use std::path::Path;

use crate::convert::{convert_logos_to_token, ConversionConfig};
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
            } else if c == '\r' {
                // Normalize CRLF/CR: do not advance column on '\r'.
                // Still advance byte offset below.
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

    /// Tokenizes a chunk of source code into tokens
    ///
    /// If this chunk is not the final chunk, and the last token ends exactly at the
    /// end of the chunk and is an extendable token (identifier or number), we treat
    /// it as a partial token and defer emitting it until the next chunk. This
    /// prevents splitting tokens across chunk boundaries.
    fn tokenize_chunk(
        &self,
        chunk: &str,
        start_pos: Position,
        is_final_chunk: bool,
    ) -> (Vec<Token>, Position, Option<String>) {
        #[cfg(test)]
        {
            eprintln!(
                "[tokenize_chunk] is_final_chunk={is_final_chunk}, start_pos={start_pos:?}, chunk=\n{chunk:?}"
            );
        }
        // Collect tokens along with their span start positions so we can safely
        // drop any tokens that fall after an unmatched string quote if needed.
        let mut tokens_with_spans: Vec<(Token, usize)> = Vec::new();
        let mut lexer = LogosToken::lexer(chunk);
        lexer.extras = start_pos;

        let mut last_span_end = 0;
        let mut last_span_start: Option<usize> = None;
        let mut last_token_can_extend = false;
        let mut last_token_pushed = false;
        // Track the end position of the most recent integer literal to assist with
        // range operator parsing across chunk boundaries (e.g., handling a single
        // '.' at the end of a chunk that should become '..' with the next chunk).
        let mut last_integer_end: Option<usize> = None;

        let convert_cfg = ConversionConfig::default();
        while let Some(token_result) = lexer.next() {
            let span = lexer.span();
            let slice = lexer.slice();
            last_span_end = span.end;

            let mut token_start_pos = start_pos;
            token_start_pos.advance(&chunk[..span.start]);

            let token_type = match &token_result {
                Ok(t) => t.clone(),
                Err(_) => {
                    // Expand malformed numeric error lexemes to include the whole offending attempt.
                    // Examples: "123abc" => "123abc", "1.2.3" => "1.2.", "1_000" => "1_000",
                    //           "123e" => "123e", "1.2e+" => "1.2e+", "0x1.2p3" => "0x1".
                    let err_start = span.start;
                    let mut err_end = span.end;
                    let bytes = chunk.as_bytes();
                    let n = bytes.len();

                    // Try to detect a numeric-looking prefix starting at err_start
                    let mut i = err_start;
                    let mut _consumed_any = false;

                    // Hex prefix 0x/0X handling: consume 0x[hex]+ as one invalid token
                    if i + 1 < n
                        && bytes[i] == b'0'
                        && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X')
                    {
                        i += 2;
                        _consumed_any = true;
                        while i < n && (bytes[i].is_ascii_hexdigit()) {
                            i += 1;
                        }
                        err_end = i; // stop before '.' or any non-hex continuation
                    } else {
                        // General numeric attempt state machine
                        // States: 0 start, 1 int run, 2 seen '.', 3 frac run, 4 seen 'e/E', 5 seen exp sign
                        let mut state = 0u8;
                        // Optional leading minus
                        if i < n && bytes[i] == b'-' {
                            i += 1;
                        }
                        // Integer part
                        let int_start = i;
                        while i < n && bytes[i].is_ascii_digit() {
                            i += 1;
                        }
                        if i > int_start {
                            state = 1; // have integer digits
                            _consumed_any = true;
                        }
                        // Fractional part
                        if i < n && bytes[i] == b'.' {
                            // If we already had a '.', then this dot makes it invalid; include it and stop
                            if state >= 2 {
                                i += 1; // include the second dot and stop
                                err_end = i;
                            } else {
                                i += 1; // consume dot
                                state = 2;
                                let frac_start = i;
                                while i < n && bytes[i].is_ascii_digit() {
                                    i += 1;
                                }
                                if i > frac_start {
                                    state = 3;
                                }
                            }
                        }
                        // Allow underscores only when followed by at least one digit (e.g., 1_000).
                        // Do NOT consume a trailing underscore with no following digit (e.g., 0_)
                        // to match non-chunked behavior (Error("0"), then Underscore token).
                        while i + 1 < n && bytes[i] == b'_' && bytes[i + 1].is_ascii_digit() {
                            _consumed_any = true;
                            // consume '_' and subsequent digit run
                            i += 1;
                            let after_us = i;
                            while i < n && bytes[i].is_ascii_digit() {
                                i += 1;
                            }
                            if i == after_us {
                                break;
                            }
                        }
                        // Exponent: only include an 'e'/'E' if it's actually part of an exponent prefix.
                        // Do NOT consume 'e' when it's the start of an identifier (e.g., "0else").
                        if i < n && (bytes[i] == b'e' || bytes[i] == b'E') {
                            let mut include_e = false;
                            if i + 1 < n {
                                let next = bytes[i + 1];
                                // Only include 'e' when it's clearly part of an exponent prefix
                                if next.is_ascii_digit() || next == b'+' || next == b'-' {
                                    include_e = true;
                                }
                            } else if is_final_chunk {
                                // At end of input, include the trailing 'e' to match non-chunked behavior
                                include_e = true;
                            }
                            if include_e {
                                i += 1; // include 'e'
                                state = 4;
                                if i < n && (bytes[i] == b'+' || bytes[i] == b'-') {
                                    i += 1; // include sign
                                    state = 5;
                                }
                                // If there are exponent digits, this would have been a valid float; since we're here
                                // due to Err, we include up to this point (no digits yet) and stop.
                            }
                        }

                        // mark that `state` was used for flow tracking even if final value isn't read
                        let _ = state;

                        // Do NOT consume trailing alphabetic identifier characters; preserve parity
                        // with the non-chunked lexer which emits Error("0") followed by Identifier.

                        // If we advanced, use that as error end; otherwise, keep original slice
                        if i > err_start {
                            err_end = i;
                        }
                    }

                    // Finalize the error lexeme using the expanded [err_start, err_end)
                    let expanded_slice = &chunk[err_start..err_end.min(n)];
                    let err_tok = convert_logos_to_token(
                        LogosToken::Error,
                        expanded_slice,
                        token_start_pos.into(),
                        convert_cfg,
                    );
                    #[cfg(feature = "logging")]
                    {
                        log::debug!(
                            "ChunkedLexer: error token at {}:{} (offset {}), lexeme={:?}, final_chunk={}",
                            token_start_pos.line,
                            token_start_pos.column,
                            token_start_pos.offset,
                            expanded_slice,
                            is_final_chunk
                        );
                    }
                    tokens_with_spans.push((err_tok, span.start));
                    last_token_pushed = true;
                    last_span_start = Some(span.start);
                    last_token_can_extend = false;
                    continue;
                }
            };

            // Track whether this token could legally extend in the next chunk.
            // In addition to identifiers and numbers, treat keyword-like tokens
            // that are purely alphabetic/underscore as extendable so that
            // cases like "observation" + "2" across a boundary will be
            // re-lexed as a single identifier ("observation2"), matching the
            // non-chunked lexer.
            last_span_start = Some(span.start);
            let is_identifierish = {
                let mut chars = slice.chars();
                let first_ok = chars
                    .next()
                    .map(|c| c.is_ascii_alphabetic() || c == '_')
                    .unwrap_or(false);
                first_ok && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            };
            last_token_can_extend = matches!(
                token_type,
                LogosToken::Identifier(_)
                    | LogosToken::Integer(_)
                    | LogosToken::NegativeInteger(_)
                    | LogosToken::Float(_)
            ) || is_identifierish;

            if !slice.is_empty() {
                // Check if this token is an integer BEFORE conversion (conversion consumes the value)
                let is_integer = matches!(token_type, LogosToken::Integer(_));

                let token =
                    convert_logos_to_token(token_type, slice, token_start_pos.into(), convert_cfg);
                tokens_with_spans.push((token, span.start));
                last_token_pushed = true;

                // Mirror non-chunked lexer behavior: if an integer literal is
                // immediately followed by a range operator (".." or "..="),
                // emit a Range/RangeInclusive token and advance the inner lexer
                // to consume the operator so that subsequent tokens are parsed
                // correctly. This avoids the two dots being tokenized as two
                // separate Dot tokens due to priority rules.
                if is_integer {
                    last_integer_end = Some(span.end);
                    let remaining = &chunk[span.end..];
                    if remaining.starts_with("..") {
                        // Determine inclusive vs exclusive range
                        let (range_len, range_tt) = if remaining.starts_with("..=") {
                            (3, TokenType::RangeInclusive)
                        } else {
                            (2, TokenType::Range)
                        };

                        // Build range token location at span.end
                        let mut range_pos = start_pos;
                        range_pos.advance(&chunk[..span.end]);
                        let range_lexeme = &chunk[span.end..span.end + range_len];
                        let range_tok = Token::new(range_tt, range_lexeme, range_pos.into());
                        tokens_with_spans.push((range_tok, span.end));

                        // Update boundary trackers to reflect consumption of the range op
                        last_span_start = Some(span.end);
                        last_span_end = span.end + range_len;
                        last_token_can_extend = false; // Range operator cannot extend
                        last_token_pushed = true;

                        // Consume the range operator in the logos lexer so we don't
                        // get stray Dot/Equal tokens for it.
                        lexer.bump(range_len);
                    }
                }
            } else {
                last_token_pushed = false;
            }
        }

        // Post-process to merge a leading '-' with a following float that begins with '.'
        // into a single negative float token (e.g., "-.5" => Float(-0.5)), matching the
        // non-chunked lexer behavior. Only merge when the tokens are directly adjacent
        // (no whitespace or other chars between) and the float lexeme starts with '.'.
        if !tokens_with_spans.is_empty() {
            let mut merged: Vec<(Token, usize)> = Vec::with_capacity(tokens_with_spans.len());
            let mut idx = 0;
            while idx < tokens_with_spans.len() {
                if idx + 1 < tokens_with_spans.len() {
                    let (ref t1, s1) = tokens_with_spans[idx];
                    let (ref t2, s2) = tokens_with_spans[idx + 1];
                    let t1_is_minus = matches!(t1.token_type, TokenType::Minus);
                    let t2_is_float = matches!(t2.token_type, TokenType::Float(_));
                    let adj = s1 + t1.lexeme.as_str().len() == s2;
                    if t1_is_minus && t2_is_float && adj && t2.lexeme.as_str().starts_with('.') {
                        // Build a single Float token with negated value and combined lexeme
                        let fval = match t2.token_type {
                            TokenType::Float(v) => v,
                            _ => unreachable!(),
                        };
                        let combined_lexeme = {
                            let mut s = String::with_capacity(1 + t2.lexeme.as_str().len());
                            s.push('-');
                            s.push_str(t2.lexeme.as_str());
                            s
                        };
                        let new_tok = Token::new(
                            TokenType::Float(-fval),
                            combined_lexeme.as_str(),
                            t1.location,
                        );
                        merged.push((new_tok, s1));
                        idx += 2;
                        continue;
                    }
                }
                merged.push(tokens_with_spans[idx].clone());
                idx += 1;
            }
            // Recalculate last_span_end based on merged tokens
            if let Some((last_tok, last_start)) = merged.last() {
                last_span_end = last_start + last_tok.lexeme.as_str().len();
            } else {
                last_span_end = 0;
            }
            tokens_with_spans = merged;
        }

        // Note: do not alter Float + adjacent .Float sequences here to preserve parity with
        // the non-chunked lexer (e.g., medical codes like 'B99.13.14'). Invalid cases like
        // '1.2.3' are handled in ChunkedLexer::tokenize() without changing the token stream.

        // Determine if there is an unmatched (unclosed) string starting in this chunk.
        // We scan for unescaped double quotes and toggle in_string. If we end the chunk
        // while still in_string, we will defer from the opening quote onward.
        let mut in_string = false;
        let mut last_open_quote_idx: Option<usize> = None;
        let bytes = chunk.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'"' {
                // Count preceding backslashes to determine if this quote is escaped
                let mut bs_count = 0;
                let mut j = i;
                while j > 0 && bytes[j - 1] == b'\\' {
                    bs_count += 1;
                    j -= 1;
                }
                let escaped = bs_count % 2 == 1;
                if !escaped {
                    if in_string {
                        // Closing quote
                        in_string = false;
                        last_open_quote_idx = None;
                    } else {
                        // Opening quote
                        in_string = true;
                        last_open_quote_idx = Some(i);
                    }
                }
            }
            i += 1;
        }
        #[cfg(test)]
        {
            let chunk_len = bytes.len();
            eprintln!(
                "[string scan] in_string={in_string}, last_open_quote_idx={last_open_quote_idx:?}, chunk_len={chunk_len}"
            );
        }

        // If not the final chunk and we are still inside a string, treat content
        // from the opening quote onward as a partial token and do not emit it yet.
        let mut used_end = last_span_end;
        let mut partial_string: Option<String> = None;
        if !is_final_chunk {
            if let (true, Some(cut_idx)) = (in_string, last_open_quote_idx) {
                #[cfg(test)]
                {
                    let chunk_len = chunk.len();
                    eprintln!(
                        "[string deferral] cut_idx={cut_idx}, in_string={in_string}, chunk_len={chunk_len}"
                    );
                }
                // Drop any tokens that begin at or after the opening quote
                while let Some((_, span_start)) = tokens_with_spans.last() {
                    if *span_start >= cut_idx {
                        tokens_with_spans.pop();
                    } else {
                        break;
                    }
                }
                used_end = cut_idx.min(used_end);
                partial_string = Some(chunk[cut_idx..].to_string());
            }
        }

        // Detect comments that span across the chunk boundary and defer them.
        // We scan for an unmatched block comment opener ("/*" without a closing "*/")
        // and for a line comment start ("//") that has no terminating newline within
        // this chunk. If found and not the final chunk, we drop any tokens starting at
        // or after the comment start and carry the remainder forward.
        // IMPORTANT: If a comment opener occurs earlier than a string opener we already
        // deferred on, prefer the EARLIEST cut so chunked behavior matches non-chunked.
        if !is_final_chunk {
            let mut in_block_comment = false;
            let mut last_open_block_idx: Option<usize> = None;
            let mut in_line_comment = false;
            let mut last_open_line_idx: Option<usize> = None;
            let mut k = 0;
            while k < bytes.len() {
                if !in_block_comment
                    && !in_line_comment
                    && k + 1 < bytes.len()
                    && bytes[k] == b'/'
                    && bytes[k + 1] == b'*'
                {
                    in_block_comment = true;
                    last_open_block_idx = Some(k);
                    k += 2;
                    continue;
                }
                if in_block_comment
                    && k + 1 < bytes.len()
                    && bytes[k] == b'*'
                    && bytes[k + 1] == b'/'
                {
                    in_block_comment = false;
                    last_open_block_idx = None;
                    k += 2;
                    continue;
                }
                if !in_block_comment
                    && !in_line_comment
                    && k + 1 < bytes.len()
                    && bytes[k] == b'/'
                    && bytes[k + 1] == b'/'
                {
                    in_line_comment = true;
                    last_open_line_idx = Some(k);
                    k += 2;
                    continue;
                }
                if in_line_comment && bytes[k] == b'\n' {
                    in_line_comment = false;
                    last_open_line_idx = None;
                }
                k += 1;
            }

            // Prefer deferring from the earliest unmatched construct if present
            if let Some(cut_idx) = last_open_block_idx.or(last_open_line_idx) {
                // Determine any existing deferral cut (from string handling above)
                let existing_cut = partial_string.as_ref().map(|s| chunk.len() - s.len());
                let should_apply = match existing_cut {
                    None => true,
                    Some(prev_cut) => cut_idx < prev_cut,
                };
                if should_apply {
                    while let Some((_, span_start)) = tokens_with_spans.last() {
                        if *span_start >= cut_idx {
                            tokens_with_spans.pop();
                        } else {
                            break;
                        }
                    }
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // If not the final chunk, add special handling for numeric literals that may be
        // followed by an exponent split across the chunk boundary. If the tail
        // of this chunk looks like: (Integer | NegativeInteger | Float-without-exponent)
        // + 'e'/'E' [+|-]? (with no digits yet), defer from the start of the base number
        // so the next chunk can form the full scientific-notation float (e.g., "1e3", ".5e-2").
        if !is_final_chunk && partial_string.is_none() {
            // Helper: can this token serve as the base of an exponent form that still needs digits?
            // True for Integer, NegativeInteger, and Float that does not yet include an exponent marker.
            let base_can_take_exp_at = |tok: &Token| -> bool {
                match tok.token_type {
                    TokenType::Integer(_) | TokenType::NegativeInteger(_) => true,
                    TokenType::Float(_) => {
                        let s = tok.lexeme.as_str();
                        !(s.contains('e') || s.contains('E'))
                    }
                    _ => false,
                }
            };

            let n = tokens_with_spans.len();
            if n >= 2 {
                // Pattern: [.., Float(no exp), Identifier("e"|"E")] at end of chunk
                let (last_tok, _last_start) = &tokens_with_spans[n - 1];
                let (prev_tok, prev_start) = &tokens_with_spans[n - 2];
                let last_is_e_ident = matches!(
                    last_tok.token_type,
                    TokenType::Identifier(ref is) if {
                        let s = is.as_str(); s == "e" || s == "E"
                    }
                );
                if last_is_e_ident && base_can_take_exp_at(prev_tok) {
                    // Defer from the float start
                    let cut_idx = *prev_start;
                    // remove the trailing 'e' token
                    tokens_with_spans.pop();
                    // remove the float token
                    tokens_with_spans.pop();
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                } else if n >= 3 {
                    // Pattern: [.., Float(no exp), Identifier('e'|'E'), (+|-) ] at end of chunk
                    let (third_tok, third_start) = &tokens_with_spans[n - 3];
                    let (second_tok, _second_start) = &tokens_with_spans[n - 2];
                    let (last_tok2, _last_start2) = &tokens_with_spans[n - 1];
                    let second_is_e_ident = matches!(
                        second_tok.token_type,
                        TokenType::Identifier(ref is) if { let s = is.as_str(); s == "e" || s == "E" }
                    );
                    let last_is_sign =
                        matches!(last_tok2.token_type, TokenType::Plus | TokenType::Minus);
                    if second_is_e_ident && last_is_sign && base_can_take_exp_at(third_tok) {
                        let cut_idx = *third_start;
                        // remove sign, 'e', and float
                        tokens_with_spans.pop();
                        tokens_with_spans.pop();
                        tokens_with_spans.pop();
                        used_end = cut_idx.min(used_end);
                        partial_string = Some(chunk[cut_idx..].to_string());
                    }
                }
            }
        }

        // Fallback: raw tail scan for exponent prefix that Logos may not have tokenized
        // cleanly (e.g., produced a LexerError("1") when seeing "1e" at chunk end).
        // Pattern: [numeric base] ('e' | 'E') ([+]|[-])? at chunk end, with no exponent digits yet.
        if !is_final_chunk && partial_string.is_none() && !chunk.is_empty() {
            let bytes = chunk.as_bytes();
            let n = bytes.len();
            // If the last token is an Error and the tail looks like an exponent prefix,
            // compute cut index first to avoid borrowing issues, then defer.
            let mut error_tail_cut: Option<usize> = None;
            if let Some((tok, start)) = tokens_with_spans.last() {
                if matches!(tok.token_type, TokenType::Error(_)) {
                    let cut_idx = *start;
                    let tail = &chunk[cut_idx..];
                    if tail.ends_with('e')
                        || tail.ends_with('E')
                        || tail.ends_with("e+")
                        || tail.ends_with("e-")
                        || tail.ends_with("E+")
                        || tail.ends_with("E-")
                    {
                        error_tail_cut = Some(cut_idx);
                    }
                }
            }
            if let Some(cut_idx) = error_tail_cut {
                // Drop any tokens that overlap the cut point
                while let Some((tok, start)) = tokens_with_spans.last() {
                    let end = *start + tok.lexeme.as_str().len();
                    if end > cut_idx {
                        tokens_with_spans.pop();
                    } else {
                        break;
                    }
                }
                used_end = cut_idx.min(used_end);
                partial_string = Some(chunk[cut_idx..].to_string());
            }

            if partial_string.is_none() {
                // Special-case: Logos sometimes yields Error('1') then Identifier('e') at tail for "1e".
                // If so, and we're not at the final chunk, defer from the Error's start to preserve exponent formation.
                if tokens_with_spans.len() >= 2 {
                    // Capture needed details without holding immutable borrow during mutation
                    let (should_defer_pair, cut_idx_val) = {
                        let (last_tok, last_start) =
                            &tokens_with_spans[tokens_with_spans.len() - 1];
                        let last_end = *last_start + last_tok.lexeme.as_str().len();
                        let is_tail_e_ident = matches!(
                            last_tok.token_type,
                            TokenType::Identifier(ref is) if {
                                let s = is.as_str();
                                (s == "e" || s == "E") && last_end == n
                            }
                        );
                        if is_tail_e_ident {
                            let (prev_tok, prev_start) =
                                &tokens_with_spans[tokens_with_spans.len() - 2];
                            if matches!(prev_tok.token_type, TokenType::Error(_)) {
                                (true, *prev_start)
                            } else {
                                (false, 0)
                            }
                        } else {
                            (false, 0)
                        }
                    };
                    if should_defer_pair {
                        // Remove 'e' Identifier and preceding Error, then defer from cut_idx
                        tokens_with_spans.pop(); // Identifier('e'|'E')
                        tokens_with_spans.pop(); // Error('1' or similar)
                        used_end = cut_idx_val.min(used_end);
                        partial_string = Some(chunk[cut_idx_val..].to_string());
                    }
                }

                if partial_string.is_none() {
                    // Helper to check if the sequence just before idx forms a numeric tail
                    // that can be followed by exponent marker: digit(s) [ '.' digit* ] .
                    let mut consider_cut: Option<usize> = None;
                    if n >= 2 {
                        let last = bytes[n - 1];
                        // Case A: ... [0-9 or .] 'e'|'E' at end
                        if last == b'e' || last == b'E' {
                            // Ensure char before 'e' is digit or a '.' that has a digit before it
                            if bytes[n - 2].is_ascii_digit()
                                || (bytes[n - 2] == b'.' && n >= 3 && bytes[n - 3].is_ascii_digit())
                            {
                                // Walk backwards to find the start of the numeric base
                                let mut k = n - 2; // position before 'e'
                                while k > 0 && bytes[k].is_ascii_digit() {
                                    k -= 1;
                                }
                                // Determine the tentative base start (first digit of the run)
                                let mut base_start =
                                    if bytes[k].is_ascii_digit() { k } else { k + 1 };
                                // Consider a preceding '.' that has at least one digit before it
                                if base_start > 0 && bytes[base_start - 1] == b'.' {
                                    let mut j = base_start - 1;
                                    if j > 0 && bytes[j - 1].is_ascii_digit() {
                                        j -= 1;
                                        while j > 0 && bytes[j].is_ascii_digit() {
                                            j -= 1;
                                        }
                                        if !bytes[j].is_ascii_digit() {
                                            j += 1;
                                        }
                                        base_start = j;
                                    }
                                }
                                // Optional leading '-'
                                if base_start > 0
                                    && bytes[base_start - 1] == b'-'
                                    && (base_start < 2 || !bytes[base_start - 2].is_ascii_digit())
                                {
                                    base_start -= 1;
                                }
                                // Clamp so we don't cut before the end of the last successful token
                                let last_ok_end = tokens_with_spans
                                    .iter()
                                    .rfind(|(t, _)| !matches!(t.token_type, TokenType::Error(_)))
                                    .map(|(t, s)| *s + t.lexeme.as_str().len())
                                    .unwrap_or(0);
                                if base_start < last_ok_end {
                                    base_start = last_ok_end;
                                }
                                consider_cut = Some(base_start);
                            }
                        }
                        // Case B: ... 'e'|'E' ('+'|'-') at end
                        else if (last == b'+' || last == b'-')
                            && n >= 3
                            && (bytes[n - 2] == b'e' || bytes[n - 2] == b'E')
                            && (bytes[n - 3].is_ascii_digit()
                                || (bytes[n - 3] == b'.'
                                    && n >= 4
                                    && bytes[n - 4].is_ascii_digit()))
                        {
                            // Walk back starting before the 'e'
                            let mut k = n - 3; // position before 'e'
                            while k > 0 && bytes[k].is_ascii_digit() {
                                k -= 1;
                            }
                            // Tentative base start
                            let mut base_start = if bytes[k].is_ascii_digit() { k } else { k + 1 };
                            // Consider preceding '.' with digits before it
                            if base_start > 0 && bytes[base_start - 1] == b'.' {
                                let mut j = base_start - 1;
                                if j > 0 && bytes[j - 1].is_ascii_digit() {
                                    j -= 1;
                                    while j > 0 && bytes[j].is_ascii_digit() {
                                        j -= 1;
                                    }
                                    if !bytes[j].is_ascii_digit() {
                                        j += 1;
                                    }
                                    base_start = j;
                                }
                            }
                            // Optional leading '-'
                            if base_start > 0
                                && bytes[base_start - 1] == b'-'
                                && (base_start < 2 || !bytes[base_start - 2].is_ascii_digit())
                            {
                                base_start -= 1;
                            }
                            // Clamp to end of last successful token
                            let last_ok_end = tokens_with_spans
                                .iter()
                                .rfind(|(t, _)| !matches!(t.token_type, TokenType::Error(_)))
                                .map(|(t, s)| *s + t.lexeme.as_str().len())
                                .unwrap_or(0);
                            if base_start < last_ok_end {
                                base_start = last_ok_end;
                            }
                            consider_cut = Some(base_start);
                        }
                    }
                    if let Some(cut_idx) = consider_cut {
                        if cut_idx < n {
                            // Drop any tokens that overlap the cut (end > cut_idx)
                            while let Some((tok, start)) = tokens_with_spans.last() {
                                let end = *start + tok.lexeme.as_str().len();
                                if end > cut_idx {
                                    tokens_with_spans.pop();
                                } else {
                                    break;
                                }
                            }
                            used_end = cut_idx.min(used_end);
                            partial_string = Some(chunk[cut_idx..].to_string());
                        }
                    }
                }
            }
        }

        // Special-case: CPT followed by a trailing '-' at the very end of a non-final chunk.
        // Logos will tokenize this as [CPT("CPT:12345A"), Minus], but the streaming lexer
        // would match a single CPT token once the suffix arrives (e.g., "-110"). Defer from
        // the CPT start so the next chunk can form the complete CPT token.
        if !is_final_chunk
            && partial_string.is_none()
            && last_span_end == chunk.len()
            && tokens_with_spans.len() >= 2
        {
            let n = tokens_with_spans.len();
            let (last_tok, _last_start) = &tokens_with_spans[n - 1];
            let (prev_tok, prev_start) = &tokens_with_spans[n - 2];
            let last_is_minus = matches!(last_tok.token_type, TokenType::Minus);
            // Consider CPT-like if explicitly a CPT token OR an identifier lexeme starting with "CPT:"
            let prev_is_cpt_like = match &prev_tok.token_type {
                TokenType::CPT(_) => true,
                TokenType::Identifier(ref is) => is.as_str().starts_with("CPT:"),
                _ => false,
            };
            if last_is_minus && prev_is_cpt_like {
                let cut_idx = *prev_start;
                // remove '-' and the CPT-like token
                tokens_with_spans.pop();
                tokens_with_spans.pop();
                used_end = cut_idx.min(used_end);
                partial_string = Some(chunk[cut_idx..].to_string());
            }
        }

        // Special-case: LOINC followed by a trailing '-' at the very end of a non-final chunk.
        // Logos will tokenize this as [LOINC("LOINC:12345"), Minus], but the streaming lexer
        // would match a single LOINC token once the digit arrives (e.g., "-6"). Defer from
        // the LOINC start so the next chunk can form the complete LOINC token.
        if !is_final_chunk
            && partial_string.is_none()
            && last_span_end == chunk.len()
            && tokens_with_spans.len() >= 2
        {
            let n = tokens_with_spans.len();
            let (last_tok, _last_start) = &tokens_with_spans[n - 1];
            let (prev_tok, prev_start) = &tokens_with_spans[n - 2];
            let last_is_minus = matches!(last_tok.token_type, TokenType::Minus);
            // Consider LOINC-like if explicitly a LOINC token OR an identifier lexeme starting with "LOINC:"
            let prev_is_loinc_like = match &prev_tok.token_type {
                TokenType::LOINC(_) => true,
                TokenType::Identifier(ref is) => is.as_str().starts_with("LOINC:"),
                _ => false,
            };
            if last_is_minus && prev_is_loinc_like {
                let cut_idx = *prev_start;
                // remove '-' and the LOINC-like token
                tokens_with_spans.pop();
                tokens_with_spans.pop();
                used_end = cut_idx.min(used_end);
                partial_string = Some(chunk[cut_idx..].to_string());
            }
        }

        // Special-case: ICD10 followed by trailing '.' at the very end of a non-final chunk.
        // Logos will tokenize this as [ICD10("ICD10:B99"), Dot], but the streaming lexer
        // would match a single ICD10 token once the digit(s) arrive (e.g., ".1"). Defer from
        // the ICD10 start so the next chunk can form the complete ICD10 token.
        if !is_final_chunk
            && partial_string.is_none()
            && last_span_end == chunk.len()
            && tokens_with_spans.len() >= 2
        {
            let n = tokens_with_spans.len();
            let (last_tok, _last_start) = &tokens_with_spans[n - 1];
            let (prev_tok, prev_start) = &tokens_with_spans[n - 2];
            let last_is_dot = matches!(last_tok.token_type, TokenType::Dot);
            // Consider ICD10-like if explicitly an ICD10 token OR an identifier lexeme starting with "ICD10:"
            let prev_is_icd10_like = match &prev_tok.token_type {
                TokenType::ICD10(_) => true,
                TokenType::Identifier(ref is) => is.as_str().starts_with("ICD10:"),
                _ => false,
            };
            if last_is_dot && prev_is_icd10_like {
                let cut_idx = *prev_start;
                // remove '.' and the ICD10-like token
                tokens_with_spans.pop();
                tokens_with_spans.pop();
                used_end = cut_idx.min(used_end);
                // Carry from the ICD10 start INCLUDING the trailing '.' so that
                // the next chunk's leading digits merge into a full ICD10 with decimal.
                partial_string = Some(chunk[cut_idx..].to_string());
            }
        }

        // General medical code boundary handling:
        // If we see a scheme identifier followed by ':' anywhere in this chunk and it's
        // not the final chunk, defer from the scheme start so the full code token can be
        // recognized across chunks (e.g., "ICD10:B99.12"). This is robust to mid-code
        // boundaries and avoids prematurely emitting partial pieces like Identifier/Colon/Float.
        if !is_final_chunk && partial_string.is_none() {
            // Find the last occurrence of (Identifier one of schemes) followed by Colon
            let schemes = ["ICD10", "LOINC", "SNOMED", "CPT"];
            let mut cut: Option<usize> = None;
            for i in (0..tokens_with_spans.len().saturating_sub(1)).rev() {
                let (tok_i, start_i) = &tokens_with_spans[i];
                let (tok_j, _start_j) = &tokens_with_spans[i + 1];
                let is_scheme = match &tok_i.token_type {
                    TokenType::Identifier(is) => {
                        let s = is.as_str();
                        schemes.contains(&s)
                    }
                    _ => false,
                };
                if is_scheme && matches!(tok_j.token_type, TokenType::Colon) {
                    cut = Some(*start_i);
                    break;
                }
            }
            if let Some(cut_idx) = cut {
                // Remove all tokens that start at or after the cut point
                while let Some((_, span_start)) = tokens_with_spans.last() {
                    if *span_start >= cut_idx {
                        tokens_with_spans.pop();
                    } else {
                        break;
                    }
                }
                used_end = cut_idx.min(used_end);
                partial_string = Some(chunk[cut_idx..].to_string());
            }
        }

        // Defensive: if the last token in a non-final chunk is a String that ends
        // exactly at the chunk end, defer it so that any escape sequences or the
        // closing quote that may start the next chunk are handled correctly.
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, start_idx)) = tokens_with_spans.last() {
                if matches!(last_tok.token_type, TokenType::String(_)) {
                    let cut_idx = *start_idx;
                    tokens_with_spans.pop();
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // Refined medical code handling at chunk end:
        // Only defer when the specific code lexeme can still be legally extended
        // per its regex, to avoid merging adjacent codes (e.g., ICD10 followed by CPT).
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, start_idx)) = tokens_with_spans.last() {
                let should_defer = match &last_tok.token_type {
                    // ICD10: "ICD10:[A-Z]\d{2}(?:\.\d{1,2})?"
                    // Defer only if we have the base without decimal, or exactly one decimal digit.
                    TokenType::ICD10(ref s) => {
                        let s = s.as_str();
                        // Defer if no decimal yet (can extend with .d or .dd),
                        // or if exactly one decimal digit is present (can extend to two digits).
                        (!s.contains('.'))
                            || s.rsplit_once('.')
                                .map(|(_, tail)| tail.len() == 1)
                                .unwrap_or(false)
                    }
                    // LOINC: "LOINC:[0-9]+(?:-[0-9]+)?"
                    // Always safe to defer to allow more digits or an optional hyphen+digits
                    TokenType::LOINC(_) => true,
                    // SNOMED: "SNOMED:[0-9]+" always extendable with more digits
                    TokenType::SNOMED(_) => true,
                    // CPT: "CPT:[0-9]{4,5}(?:[A-Z])?(?:-[0-9A-Z]+)?"
                    // Always defer at chunk end to allow optional letter or hyphen-suffix to extend.
                    // This ensures cases like "CPT:12345A-1" + "10" merge to "CPT:12345A-110".
                    TokenType::CPT(_) => true,
                    // Also treat Identifier lexemes that start with known medical schemes as extendable.
                    // This handles cases where partial codes are lexed as Identifier tokens.
                    TokenType::Identifier(ref is) if is.as_str().starts_with("ICD10:") => true,
                    TokenType::Identifier(ref is) if is.as_str().starts_with("CPT:") => true,
                    _ => false,
                };
                if should_defer {
                    let cut_idx = *start_idx;
                    tokens_with_spans.pop();
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // If not the final chunk and the last token ended exactly at the end of the chunk
        // and that token could extend, treat it as a partial token and do not emit it yet.
        if !is_final_chunk
            && partial_string.is_none()
            && last_span_end == chunk.len()
            && last_token_can_extend
            && last_token_pushed
        {
            if let Some(start) = last_span_start {
                // Remove the last emitted token and carry it over
                if let Some((_, span_start)) = tokens_with_spans.last() {
                    if *span_start == start {
                        tokens_with_spans.pop();
                    }
                }
                used_end = start;
                partial_string = Some(chunk[start..].to_string());
            }
        }

        // Additional boundary handling for trailing '.':
        // If this chunk ends with a single '.' and it's not the final chunk, defer from
        // the '.' onward. Prefer pulling a preceding number (Integer or NegativeInteger)
        // so that floats across chunks (e.g., "-0." + "5") merge into a single Float.
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, span_start)) = tokens_with_spans.last() {
                if matches!(last_tok.token_type, TokenType::Dot) {
                    let dot_start = *span_start;

                    // Check if the token immediately before '.' is an Integer or NegativeInteger and is adjacent.
                    if tokens_with_spans.len() >= 2 {
                        let prev_idx = tokens_with_spans.len() - 2;
                        // Capture needed data without holding an immutable borrow during mutation
                        let (prev_start_val, prev_is_adjacent, prev_is_number, prev_is_minus) = {
                            let (prev_tok, prev_start_ref) = &tokens_with_spans[prev_idx];
                            let prev_len = prev_tok.lexeme.as_str().len();
                            let prev_end = *prev_start_ref + prev_len;
                            let adjacent = prev_end == dot_start;
                            let is_number = matches!(
                                prev_tok.token_type,
                                TokenType::Integer(_) | TokenType::NegativeInteger(_)
                            );
                            let is_minus = matches!(prev_tok.token_type, TokenType::Minus);
                            (*prev_start_ref, adjacent, is_number, is_minus)
                        };

                        if prev_is_adjacent && (prev_is_number || prev_is_minus) {
                            // Pop '.' and the preceding number; carry both into partial
                            tokens_with_spans.pop(); // '.'
                            tokens_with_spans.pop(); // number
                            used_end = prev_start_val.min(used_end);
                            partial_string = Some(chunk[prev_start_val..].to_string());
                        } else {
                            // Only defer the '.' itself
                            tokens_with_spans.pop();
                            used_end = dot_start.min(used_end);
                            partial_string = Some(chunk[dot_start..].to_string());
                        }
                    } else {
                        // No previous token, just defer '.'
                        tokens_with_spans.pop();
                        used_end = dot_start.min(used_end);
                        partial_string = Some(chunk[dot_start..].to_string());
                    }
                } else if !last_token_pushed {
                    if let (Some(dot_start), Some(int_end)) = (last_span_start, last_integer_end) {
                        // Fallback: no token pushed, but detect the integer + '.' boundary
                        if dot_start == int_end && chunk[dot_start..].starts_with('.') {
                            used_end = dot_start.min(used_end);
                            partial_string = Some(chunk[dot_start..].to_string());
                        }
                    }
                }
            }
        }

        // If a non-final chunk ends exactly at a NegativeInteger, defer it so it can
        // merge with a following fractional/exponent part in the next chunk to form a Float.
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, span_start)) = tokens_with_spans.last() {
                if matches!(last_tok.token_type, TokenType::NegativeInteger(_)) {
                    let neg_start = *span_start;
                    tokens_with_spans.pop();
                    used_end = neg_start.min(used_end);
                    partial_string = Some(chunk[neg_start..].to_string());
                }
            }
        }

        // Boundary handling for trailing '..' (Range) at chunk end:
        // If a non-final chunk ends exactly with a Range token, defer it so the next
        // chunk can upgrade it to RangeInclusive ("..=") if an '=' follows. This avoids
        // prematurely emitting a Range that would differ from the streaming lexer.
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, start_idx)) = tokens_with_spans.last() {
                if matches!(last_tok.token_type, TokenType::Range) {
                    let cut_idx = *start_idx;
                    tokens_with_spans.pop();
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // Boundary handling for trailing '?' to support '??' and '?:' across chunks:
        // If a non-final chunk ends with a lone '?' character, defer it so the next
        // chunk can combine it with a following '?' or ':' to form the proper
        // operator token (QuestionQuestion or QuestionColon) instead of emitting
        // a LexerError for '?'.
        if !is_final_chunk && partial_string.is_none() {
            let bytes = chunk.as_bytes();
            if !bytes.is_empty() && bytes[bytes.len() - 1] == b'?' {
                // Only defer if this is a single trailing '?', not already a complete '??'
                let ends_with_double_q = bytes.len() >= 2 && bytes[bytes.len() - 2] == b'?';
                if !ends_with_double_q {
                    let cut_idx = bytes.len() - 1;
                    // Drop any tokens that overlap the cut (end > cut_idx)
                    while let Some((tok, start)) = tokens_with_spans.last() {
                        let end = *start + tok.lexeme.as_str().len();
                        if end > cut_idx {
                            tokens_with_spans.pop();
                        } else {
                            break;
                        }
                    }
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // Boundary handling for trailing '&' and '|' to support '&&' and '||' across chunks.
        if !is_final_chunk && partial_string.is_none() {
            let bytes = chunk.as_bytes();
            if !bytes.is_empty() {
                let last = bytes[bytes.len() - 1];
                let prev_same = bytes.len() >= 2 && bytes[bytes.len() - 2] == last;
                if (last == b'&' || last == b'|') && !prev_same {
                    let cut_idx = bytes.len() - 1;
                    // Drop any tokens that overlap the cut (end > cut_idx)
                    while let Some((tok, start)) = tokens_with_spans.last() {
                        let end = *start + tok.lexeme.as_str().len();
                        if end > cut_idx {
                            tokens_with_spans.pop();
                        } else {
                            break;
                        }
                    }
                    used_end = cut_idx.min(used_end);
                    partial_string = Some(chunk[cut_idx..].to_string());
                }
            }
        }

        // Additional boundary handling for trailing operator prefixes:
        // If a non-final chunk ends with an operator that could form a multi-character
        // operator with the next chunk, defer it. Examples: '==', '!=', '<=', '>=', '=>',
        // '->', '**', '**=', '<<', '<<=', '>>', '>>=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^='
        if !is_final_chunk && partial_string.is_none() && last_span_end == chunk.len() {
            if let Some((last_tok, span_start)) = tokens_with_spans.last() {
                if matches!(
                    last_tok.token_type,
                    TokenType::Equal
                        | TokenType::Not
                        | TokenType::Less
                        | TokenType::Greater
                        | TokenType::Plus
                        | TokenType::Minus
                        | TokenType::Star
                        | TokenType::Slash
                        | TokenType::Percent
                        | TokenType::BitAnd
                        | TokenType::BitOr
                        | TokenType::BitXor
                        | TokenType::Shl
                        | TokenType::Shr
                        | TokenType::DoubleStar
                ) {
                    let mut op_start = *span_start;

                    // If the trailing token is '=' and it can merge with a preceding operator,
                    // pull the previous operator into the deferral (e.g., '!=', '<=', '>=', '+=', etc.).
                    if matches!(last_tok.token_type, TokenType::Equal)
                        && tokens_with_spans.len() >= 2
                    {
                        let prev_idx = tokens_with_spans.len() - 2;
                        // Capture needed data without holding an immutable borrow during mutation
                        let (prev_start_val, prev_end_equals_span, prev_is_merge_op) = {
                            let (prev_tok, prev_start_ref) = &tokens_with_spans[prev_idx];
                            let prev_len = prev_tok.lexeme.as_str().len();
                            let prev_end = *prev_start_ref + prev_len;
                            let can_merge = matches!(
                                prev_tok.token_type,
                                TokenType::Not
                                    | TokenType::Less
                                    | TokenType::Greater
                                    | TokenType::Plus
                                    | TokenType::Minus
                                    | TokenType::Star
                                    | TokenType::Slash
                                    | TokenType::Percent
                                    | TokenType::BitAnd
                                    | TokenType::BitOr
                                    | TokenType::BitXor
                                    | TokenType::Shl
                                    | TokenType::Shr
                                    | TokenType::DoubleStar
                            );
                            (*prev_start_ref, prev_end == *span_start, can_merge)
                        };

                        if prev_end_equals_span && prev_is_merge_op {
                            // Remove both the '=' and the preceding operator, then defer
                            tokens_with_spans.pop(); // '='
                            tokens_with_spans.pop(); // preceding operator
                            op_start = prev_start_val;
                            used_end = op_start.min(used_end);
                            partial_string = Some(chunk[op_start..].to_string());
                        } else {
                            // Can't merge '=', just defer '='
                            tokens_with_spans.pop();
                            used_end = op_start.min(used_end);
                            partial_string = Some(chunk[op_start..].to_string());
                        }
                    } else {
                        // Default: just defer the trailing operator token itself
                        tokens_with_spans.pop();
                        used_end = op_start.min(used_end);
                        partial_string = Some(chunk[op_start..].to_string());
                    }
                }
            }
        }

        // If we didn't already set a partial string from boundary logic, use any remainder
        if partial_string.is_none() && last_span_end < chunk.len() {
            partial_string = Some(chunk[last_span_end..].to_string());
        }

        let mut end_pos = start_pos;
        end_pos.advance(&chunk[..used_end]);
        let tokens = tokens_with_spans.into_iter().map(|(t, _)| t).collect();
        (tokens, end_pos, partial_string)
    }

    /// Reads the next chunk of source code and tokenize it
    fn read_next_chunk(&mut self) -> Option<Vec<Token>> {
        if self.eof && self.partial_token.is_none() {
            return None;
        }

        let (mut chunk_bytes, start_pos) = self
            .partial_token
            .take()
            .map_or((Vec::new(), self.position), |p| {
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
            // If we're at EOF and still cannot decode any UTF-8 bytes, drop the remainder to avoid infinite loops.
            if self.eof {
                self.partial_token = None;
                // No decodable data remains; signal no more tokens
                return None;
            } else {
                // Not EOF yet; carry the undecodable bytes forward and try again next read
                self.partial_token = Some(PartialToken {
                    partial_lexeme_bytes: remainder_bytes.to_vec(),
                    start: start_pos,
                });
                return Some(vec![]);
            }
        }

        // Determine if this is the final chunk (no more bytes to read and no UTF-8 remainders)
        let is_final_chunk = self.eof && remainder_bytes.is_empty();

        let (tokens, new_pos, partial_str) =
            self.tokenize_chunk(chunk_str, start_pos, is_final_chunk);

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
    pub fn into_tokens(self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for token in self {
            tokens.push(token);
        }
        tokens
    }

    /// Process the entire input and return a result with all tokens or an error
    pub fn tokenize(self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut iter = self.into_iter();
        let mut prev: Option<Token> = None;
        while let Some(token) = iter.next() {
            // Detect invalid numeric pattern: Float immediately followed by adjacent Float
            // that begins with '.' (e.g., "1.2.3" -> error "1.2."). Do this here to avoid
            // altering the iterator stream and breaking parity tests.
            if let Some(ref p) = prev {
                if matches!(p.token_type, TokenType::Float(_))
                    && matches!(token.token_type, TokenType::Float(_))
                    && token.lexeme.as_str().starts_with('.')
                    && p.location.offset + p.lexeme.as_str().len() == token.location.offset
                    && !p.lexeme.as_str().starts_with('.')
                {
                    let offending = format!("{}.", p.lexeme.as_str());
                    let error_msg = format!(
                        "Lexer error at line {}: Invalid token '{}'",
                        p.location.line, offending
                    );
                    return Err(error_msg);
                }
                // Detect Float followed by adjacent Identifier("e"/"E") and validate exponent tail.
                // Valid forms: Float + 'e'/'E' + [ '+' | '-' ]? + Integer (all adjacent).
                // Invalid examples (emit error): "1.2e", "1.2e+", "1.2e-".
                if matches!(p.token_type, TokenType::Float(_))
                    && matches!(token.token_type, TokenType::Identifier(_))
                    && (token.lexeme.as_str() == "e" || token.lexeme.as_str() == "E")
                    && p.location.offset + p.lexeme.as_str().len() == token.location.offset
                {
                    // Try to look ahead to see if we have + / - and digits
                    // We'll only consume lookahead tokens if we are NOT going to error.
                    let mut offending = String::with_capacity(p.lexeme.len() + 2);
                    offending.push_str(p.lexeme.as_str());
                    offending.push_str(token.lexeme.as_str());
                    // Peek next
                    if let Some(next1) = iter.next() {
                        let adj1 =
                            token.location.offset + token.lexeme.len() == next1.location.offset;
                        if adj1 {
                            match &next1.token_type {
                                TokenType::Plus | TokenType::Minus => {
                                    // Optional sign present; require adjacent Integer next
                                    offending.push_str(next1.lexeme.as_str());
                                    if let Some(next2) = iter.next() {
                                        let adj2 = next1.location.offset + next1.lexeme.len()
                                            == next2.location.offset;
                                        if adj2 {
                                            match &next2.token_type {
                                                TokenType::Integer(_) => {
                                                    // Valid exponent; push tokens and continue
                                                    tokens.push(token.clone());
                                                    tokens.push(next1.clone());
                                                    tokens.push(next2.clone());
                                                    prev = Some(next2);
                                                    continue;
                                                }
                                                _ => {
                                                    // No digits after sign
                                                    let error_msg = format!(
                                                        "Lexer error at line {}: Invalid token '{}'",
                                                        p.location.line, offending
                                                    );
                                                    return Err(error_msg);
                                                }
                                            }
                                        } else {
                                            // Non-adjacent after sign -> invalid
                                            let error_msg = format!(
                                                "Lexer error at line {}: Invalid token '{}'",
                                                p.location.line, offending
                                            );
                                            return Err(error_msg);
                                        }
                                    } else {
                                        // EOF after sign
                                        let error_msg = format!(
                                            "Lexer error at line {}: Invalid token '{}'",
                                            p.location.line, offending
                                        );
                                        return Err(error_msg);
                                    }
                                }
                                TokenType::Integer(_) => {
                                    // Valid exponent without sign; push and continue
                                    tokens.push(token.clone());
                                    tokens.push(next1.clone());
                                    prev = Some(next1);
                                    continue;
                                }
                                _ => {
                                    // Adjacent but not sign or digits -> invalid (e.g., eX)
                                    let error_msg = format!(
                                        "Lexer error at line {}: Invalid token '{}'",
                                        p.location.line, offending
                                    );
                                    return Err(error_msg);
                                }
                            }
                        } else {
                            // Non-adjacent 'e' -> invalid continuation
                            let error_msg = format!(
                                "Lexer error at line {}: Invalid token '{}'",
                                p.location.line, offending
                            );
                            return Err(error_msg);
                        }
                    } else {
                        // EOF right after 'e'
                        let error_msg = format!(
                            "Lexer error at line {}: Invalid token '{}'",
                            p.location.line, offending
                        );
                        return Err(error_msg);
                    }
                }
            }
            match &token.token_type {
                TokenType::Error(_msg) => {
                    // Try to expand message when an invalid numeric is immediately followed by
                    // a contiguous identifier (e.g., "123" + "abc" => "123abc").
                    let mut offending = token.lexeme.as_str().to_string();
                    // Only attempt to lookahead once; we won't emit further tokens since we return Err.
                    if let Some(next_tok) = iter.next() {
                        // Ensure contiguity by checking offsets
                        let expected_next_offset = token.location.offset + token.lexeme.len();
                        let is_contiguous = next_tok.location.offset == expected_next_offset;
                        if is_contiguous {
                            if let TokenType::Identifier(id) = &next_tok.token_type {
                                offending.push_str(id.as_str());
                            }
                        }
                    }
                    let error_msg = format!(
                        "Lexer error at line {}: Invalid token '{}'",
                        token.location.line, offending
                    );
                    return Err(error_msg);
                }
                TokenType::LexerError => {
                    let error_msg = format!("Lexer error at line {}", token.location.line);
                    return Err(error_msg);
                }
                _ => {}
            }
            prev = Some(token.clone());
            tokens.push(token);
        }
        Ok(tokens)
    }

    /// Get the next token from the source
    pub fn next_token(&mut self) -> Option<Token> {
        // If there's a buffered token, return it immediately
        if let Some(token) = self.buffer.pop_front() {
            return Some(token);
        }

        // Keep reading with a progress guard to avoid infinite loops while still
        // allowing multi-chunk tokens (e.g., long strings) that may yield many
        // empty batches until completion.
        let mut last_state = (
            self.eof,
            self.position,
            self.partial_token
                .as_ref()
                .map(|p| p.partial_lexeme_bytes.len())
                .unwrap_or(0),
        );
        loop {
            if self.eof && self.partial_token.is_none() {
                return None;
            }

            if let Some(mut tokens) = self.read_next_chunk() {
                if !tokens.is_empty() {
                    // Cross-chunk merge: '..' (Range) + '=' -> '..=' (RangeInclusive)
                    if let (Some(prev), Some(first)) = (self.buffer.back(), tokens.first()) {
                        let prev_is_range = matches!(prev.token_type, TokenType::Range)
                            && prev.lexeme.as_str() == "..";
                        let first_is_equal = matches!(first.token_type, TokenType::Equal)
                            && first.lexeme.as_str() == "=";
                        if prev_is_range && first_is_equal {
                            let prev_end_off = prev.location.offset + prev.lexeme.as_str().len();
                            let first_start_off = first.location.offset;
                            if prev_end_off == first_start_off {
                                // Merge into RangeInclusive located at prev
                                let merged =
                                    Token::new(TokenType::RangeInclusive, "..=", prev.location);
                                self.buffer.pop_back();
                                // Drop the leading '=' from the new tokens
                                tokens.remove(0);
                                // Push merged first, then the rest
                                self.buffer.push_back(merged);
                            }
                        }
                    }
                    self.buffer.extend(tokens);
                    return self.buffer.pop_front();
                }
                // No tokens this round; only continue if progress is being made
                let current_state = (
                    self.eof,
                    self.position,
                    self.partial_token
                        .as_ref()
                        .map(|p| p.partial_lexeme_bytes.len())
                        .unwrap_or(0),
                );
                if current_state == last_state {
                    return None;
                }
                last_state = current_state;
                continue;
            } else {
                return None;
            }
        }
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
    use crate::lexer::Lexer;
    use std::io::Cursor;

    fn normalize(tokens: &[Token]) -> Vec<(String, String)> {
        tokens
            .iter()
            .map(|t| (format!("{:?}", t.token_type), t.lexeme.as_str().to_string()))
            .collect()
    }

    #[test]
    fn test_range_operator_same_chunk() {
        // Ensure standard case works: 1..10 and 1..=10 in a single chunk
        let input = "1..10 1..=10";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 64 };
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        // Expect: Integer(1) Range Integer(10) Integer(1) RangeInclusive Integer(10)
        let kinds: Vec<&TokenType> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(matches!(kinds[0], TokenType::Integer(1)));
        assert!(matches!(kinds[1], TokenType::Range));
        assert!(matches!(kinds[2], TokenType::Integer(10)));
        assert!(matches!(kinds[3], TokenType::Integer(1)));
        assert!(matches!(kinds[4], TokenType::RangeInclusive));
        assert!(matches!(kinds[5], TokenType::Integer(10)));
    }

    #[test]
    fn test_float_then_range_not_confused() {
        // Ensure we don't mis-tokenize 1.5..10 as Integer/Dot/Float, etc.
        let input = "1.5..10";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 64 };
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        let kinds: Vec<&TokenType> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(matches!(kinds[0], TokenType::Float(f) if (*f - 1.5).abs() < 1e-12));
        assert!(matches!(kinds[1], TokenType::Range));
        assert!(matches!(kinds[2], TokenType::Integer(10)));
    }

    #[test]
    fn test_identifier_across_chunks() {
        let input = "veryLongIdentifier12345";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 4 };
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token_type, TokenType::Identifier(_)));
        assert_eq!(tokens[0].lexeme.as_str(), input);
    }

    #[test]
    fn test_string_across_chunks_simple() {
        let input = "\"Hello, world\"";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 3 };
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        assert_eq!(tokens.len(), 1);
        match &tokens[0].token_type {
            TokenType::String(s) => {
                assert_eq!(s.as_str(), "Hello, world");
            }
            _ => panic!("expected String token"),
        }
        assert_eq!(tokens[0].lexeme.as_str(), input);
    }

    #[test]
    fn test_string_across_chunks_multiline_and_escape() {
        let input = "\"line1\nline2 says \\\"hi\\\"\""; // contains a newline and escaped quotes
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 5 };
        let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();
        // Debug: print tokens for investigation
        for (i, t) in tokens.iter().enumerate() {
            let kind = &t.token_type;
            let lexeme = &t.lexeme;
            eprintln!("tok[{i}]: kind={kind:?}, lexeme={lexeme:?}");
        }
        // Also compare to non-chunked lexer for reference
        let plain_tokens: Vec<_> = Lexer::new(input).collect();
        for (i, t) in plain_tokens.iter().enumerate() {
            let kind = &t.token_type;
            let lexeme = &t.lexeme;
            eprintln!("plain[{i}]: kind={kind:?}, lexeme={lexeme:?}");
        }

        assert_eq!(tokens.len(), 1);
        match &tokens[0].token_type {
            TokenType::String(s) => {
                // Only escaped quotes are unescaped; the newline is literal
                assert_eq!(s.as_str(), "line1\nline2 says \"hi\"");
            }
            _ => panic!("expected String token"),
        }
    }

    #[test]
    fn test_medical_codes_across_chunks() {
        let input_icd10 = "ICD10:A01.1"; // split inside code
        let input_loinc = "LOINC:12345-6";
        let input_snomed = "SNOMED:1234567";
        let input_cpt = "CPT:12345A-7";

        for (input, expect_variant) in [
            (input_icd10, "ICD10"),
            (input_loinc, "LOINC"),
            (input_snomed, "SNOMED"),
            (input_cpt, "CPT"),
        ] {
            let cursor = Cursor::new(input);
            let config = ChunkedLexerConfig { chunk_size: 4 };
            let tokens: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();
            assert_eq!(tokens.len(), 1, "input: {input}");
            let kind = format!("{:?}", tokens[0].token_type);
            assert!(
                kind.starts_with(expect_variant),
                "got kind {kind} for {input}"
            );
            assert_eq!(tokens[0].lexeme.as_str(), input);
        }
    }

    #[test]
    fn test_line_comment_across_chunks_skipped() {
        let input = "let x = 1; // this is a comment that spans chunk boundary\npatient";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 8 };
        let tokens_chunked: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        // Ensure there is no comment token emitted and the sequence matches non-chunked
        let tokens_plain: Vec<_> = Lexer::new(input).collect();
        assert_eq!(normalize(&tokens_chunked), normalize(&tokens_plain));
    }

    #[test]
    fn test_negative_dot_float_single_token() {
        let input = "-.5";
        // Compare to non-chunked lexer
        let plain = normalize(&Lexer::new(input).collect::<Vec<_>>());
        // Try a few chunk sizes to stress boundary handling
        for &cs in &[1usize, 2, 3, 4, 8] {
            let chunked = normalize(
                &ChunkedLexer::from_reader(
                    Cursor::new(input),
                    ChunkedLexerConfig { chunk_size: cs },
                )
                .collect::<Vec<_>>(),
            );
            assert_eq!(
                chunked, plain,
                "mismatch at chunk_size={} for '{}'",
                cs, input
            );
        }
    }

    #[test]
    fn test_dot_float_chain_two_floats_parity() {
        // Cases where a leading dot float is followed by another dot float should not error
        for input in [".13.14", "B99.13.14"] {
            let plain = normalize(&Lexer::new(input).collect::<Vec<_>>());
            for &cs in &[1usize, 2, 3, 4, 5, 8, 16] {
                let chunked = normalize(
                    &ChunkedLexer::from_reader(
                        Cursor::new(input),
                        ChunkedLexerConfig { chunk_size: cs },
                    )
                    .collect::<Vec<_>>(),
                );
                assert_eq!(
                    chunked, plain,
                    "mismatch at chunk_size={} for '{}'",
                    cs, input
                );
            }
        }
    }

    #[test]
    fn test_invalid_exponent_tails_error_messages() {
        // These should be invalid: missing exponent digits
        for input in ["1.2e", "1.2e+", "1.2e-"] {
            let res =
                ChunkedLexer::from_reader(Cursor::new(input), ChunkedLexerConfig { chunk_size: 2 })
                    .tokenize();
            assert!(res.is_err(), "expected error for '{}'", input);
            let err = res.err().unwrap();
            assert!(
                err.contains(input),
                "error message '{}' does not contain expected lexeme '{}'",
                err,
                input
            );
        }
    }

    #[test]
    fn test_identifier_then_float_boundary_parity() {
        // Specifically stress the scenario: identifier ending at chunk end followed by
        // a float starting with '.' in the next chunk, e.g., "observation" + "2.5e-2".
        let input = "observation2.5e-2";
        let plain = normalize(&Lexer::new(input).collect::<Vec<_>>());

        // Choose a chunk size that ends exactly after "observation"
        let boundary = "observation".len();
        for &cs in &[boundary, boundary + 1, 8usize, 13usize] {
            let chunked = normalize(
                &ChunkedLexer::from_reader(
                    Cursor::new(input),
                    ChunkedLexerConfig { chunk_size: cs },
                )
                .collect::<Vec<_>>(),
            );
            assert_eq!(
                chunked, plain,
                "mismatch at chunk_size={} for '{}'",
                cs, input
            );
        }
    }

    #[test]
    fn test_block_comment_across_chunks_skipped() {
        let input = "/* multi-line comment that is long and crosses boundary */patient";
        let cursor = Cursor::new(input);
        let config = ChunkedLexerConfig { chunk_size: 7 };
        let tokens_chunked: Vec<_> = ChunkedLexer::from_reader(cursor, config).collect();

        // Only the Patient token should remain, same as non-chunked
        let tokens_plain: Vec<_> = Lexer::new(input).collect();
        assert_eq!(normalize(&tokens_chunked), normalize(&tokens_plain));
    }

    // Property: For valid inputs, chunked and non-chunked lexers produce identical
    // token kinds and lexemes across random chunk sizes.
    #[test]
    fn parity_fixed_examples_multiple_chunk_sizes() {
        let inputs = vec![
            "let patient = ICD10:A01.1\nobservation.per\n\"note\"",
            "1..10 patient.of observation\nCPT:12345A-7",
            "SNOMED:123456 || LOINC:12345-6 && safe",
        ];
        let chunk_sizes = [1usize, 2, 3, 4, 5, 8, 13, 64];
        for input in inputs {
            let plain = normalize(&Lexer::new(input).collect::<Vec<_>>());
            for &cs in &chunk_sizes {
                let chunked = normalize(
                    &ChunkedLexer::from_reader(
                        Cursor::new(input),
                        ChunkedLexerConfig { chunk_size: cs },
                    )
                    .collect::<Vec<_>>(),
                );
                assert_eq!(
                    chunked, plain,
                    "mismatch at chunk_size={} for input=\n{}",
                    cs, input
                );
            }
        }
    }

    // proptest-based parity testing
    use proptest::prelude::*;

    fn token_samples() -> Vec<&'static str> {
        vec![
            // identifiers
            "x",
            "foo",
            "bar1",
            "alpha_beta",
            "PatientID",
            "obs",
            "medicationDosage",
            // numbers
            "0",
            "1",
            "10",
            "123456",
            "-42",
            "3.14",
            ".5",
            "1e3",
            "2.5e-2",
            "-0.5",
            // ranges/operators
            "..",
            "..=",
            "+",
            "-",
            "*",
            "/",
            "%",
            "==",
            "!=",
            "<",
            "<=",
            ">",
            ">=",
            "&&",
            "||",
            "??",
            "?:",
            "=",
            "+=",
            "-=",
            "*=",
            "/=",
            "%=",
            "**",
            "**=",
            "of",
            "per",
            // punctuation
            ".",
            ",",
            ":",
            ";",
            "(",
            ")",
            "{",
            "}",
            "[",
            "]",
            "->",
            "=>",
            "_",
            // keywords
            "let",
            "fn",
            "if",
            "else",
            "return",
            "type",
            "struct",
            "patient",
            "observation",
            "medication",
            "fhir_query",
            "query",
            "regulate",
            "scope",
            "federated",
            "safe",
            "real_time",
            // medical codes
            "ICD10:A01",
            "ICD10:B99.1",
            "LOINC:12345-6",
            "SNOMED:123456",
            "CPT:12345",
            "CPT:12345A-1",
            // strings (keep simple and valid)
            "\"hello\"",
            "\"a\\\"b\"",
            "\"line1\nline2\"",
            "\"unicode 😀\"",
            // whitespace and comments (skipped by lexer)
            " ",
            "  ",
            "\n",
            "\t",
            "// comment here\n",
            "/* block comment */",
        ]
    }

    fn sample_token() -> impl Strategy<Value = String> {
        prop::sample::select(token_samples()).prop_map(|s| s.to_string())
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(40))]
        #[test]
        fn parity_chunked_vs_nonchunked_across_random_chunk_sizes(
            parts in prop::collection::vec(sample_token(), 1..30),
            chunk_size in 1usize..32
        ) {
            let input: String = parts.concat();
            let plain = normalize(&Lexer::new(&input).collect::<Vec<_>>());
            // Pass owned data into Cursor so it satisfies the 'static bound
            let chunked = normalize(&ChunkedLexer::from_reader(Cursor::new(input.clone()), ChunkedLexerConfig { chunk_size }).collect::<Vec<_>>());
            prop_assert_eq!(chunked, plain);
        }
    }
}
