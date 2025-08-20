//! # Medi Language Parser
//!
//! This module implements a hand-written recursive descent parser for the Medi language,
//! transforming a sequence of tokens into an abstract syntax tree (AST). The parser uses
//! the `nom` parser combinator library for building composable and maintainable parsers.
//!
//! ## Architecture Overview
//!
//! The parser follows a recursive descent approach with these key components:
//! - **Token Stream**: Wraps the token stream with position tracking and lookahead capabilities
//! - **Expression Parser**: Handles complex expressions with operator precedence and associativity
//! - **Statement Parser**: Processes control flow constructs and declarations
//! - **AST Generation**: Constructs the abstract syntax tree with source location information
//!
//! ## Parser Phases
//!
//! 1. **Lexical Analysis** (handled by the lexer)
//!    - Converts source text into tokens
//!    - Handles whitespace and comments
//!
//! 2. **Syntactic Analysis** (this module)
//!    - Parses tokens into an AST
//!    - Validates syntax according to the language grammar
//!    - Handles operator precedence and associativity
//!
//! 3. **Semantic Analysis** (future)
//!    - Type checking
//!    - Name resolution
//!    - Other semantic validations
//!
//! ## Error Handling
//!
//! The parser provides detailed error messages with source locations and implements
//! error recovery to continue parsing after encountering syntax errors.
//!
//! ## Usage
//!
//! ```rust
//! use medic_parser::parser::{parse_program, TokenSlice};
//! use medic_lexer::token::Token;
//!
//! // In production, obtain tokens from the lexer. For this example, use an empty token list.
//! let tokens: Vec<Token> = vec![];
//! let input = TokenSlice::new(&tokens);
//! let _ast = parse_program(input);
//! ```

#![allow(dead_code)]
#![allow(unused_imports)]

use medic_ast::visit::Span;
use std::ops;

use nom::error::{Error as NomError, ErrorKind, ParseError};
use nom::{
    multi::many0, Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, ParseTo, Slice,
};

use log::debug;

use medic_ast::ast::{
    BinaryExpressionNode, BinaryOperator, BlockNode, CallExpressionNode, ExpressionNode, ForNode,
    IdentifierNode, LiteralNode, MatchArmNode, MatchNode, MemberExpressionNode, PatternNode,
    ProgramNode, StatementNode,
};
use medic_lexer::token::{Token, TokenType};

// Declare submodules
pub mod diagnostics;
pub mod expressions;
pub mod identifiers;
pub mod literals;
pub mod statements;
pub mod test_utils;

// Re-export commonly used functions from submodules
pub use diagnostics::{diagnostic_from_nom_error, Diagnostic, Severity};
pub use expressions::{parse_expression, *};
pub use identifiers::*;
pub use literals::*;
pub use statements::*;

/// A slice of tokens for parsing
#[derive(Clone, Copy, Debug)]
pub struct TokenSlice<'a>(pub &'a [Token]);

impl<'a> TokenSlice<'a> {
    /// Create a new TokenSlice from a slice of tokens
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenSlice(tokens)
    }

    /// Get the first token in the slice
    pub fn first(&self) -> Option<&Token> {
        self.0.first()
    }

    /// Peek at the first token without consuming it
    pub fn peek(&self) -> Option<&'a Token> {
        self.0.first()
    }

    /// Check if the slice is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Slice the token slice from the start to the end index
    pub fn slice(&self, start: usize, end: usize) -> TokenSlice<'a> {
        TokenSlice(&self.0[start..end])
    }

    /// Get the length of the token slice
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Advance the token slice by one token
    /// Returns an empty slice if already at the end
    pub fn advance(&self) -> TokenSlice<'a> {
        if self.0.is_empty() {
            TokenSlice(&[])
        } else {
            TokenSlice(&self.0[1..])
        }
    }

    /// Skip any whitespace tokens at the beginning of the slice
    /// Returns a new TokenSlice with leading whitespace tokens removed
    pub fn skip_whitespace(&self) -> TokenSlice<'a> {
        let mut idx = 0;
        while idx < self.0.len() {
            match self.0[idx].token_type {
                TokenType::Whitespace => idx += 1,
                _ => break,
            }
        }
        TokenSlice(&self.0[idx..])
    }
}

/// Skips a top-level function declaration starting with `fn` and consumes its entire body.
///
/// This is a temporary utility to allow the parser to consume helper function declarations
/// present in integration tests until full function parsing is implemented.
fn skip_function_declaration(mut input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ()> {
    log::debug!(
        "skip_function_declaration: starting with {} tokens",
        input.len()
    );
    let start_tokens = input
        .0
        .iter()
        .take(6)
        .map(|t| &t.token_type)
        .collect::<Vec<_>>();
    println!("skip_function_declaration: start, next tokens: {start_tokens:?}");

    // Expect and consume the 'fn' keyword
    let (after_fn, _) = take_token_if(|t| matches!(t, TokenType::Fn), ErrorKind::Tag)(input)?;
    input = after_fn;

    // Find the start of the function body '{'
    let mut idx = 0usize;
    while idx < input.len() {
        if matches!(input.0[idx].token_type, TokenType::LeftBrace) {
            break;
        }
        idx += 1;
    }

    if idx == input.len() {
        // Be tolerant: Some helper declarations might be incomplete in tests.
        // If we can't find '{', skip ahead to the next reasonable boundary and continue.
        log::error!("skip_function_declaration: no function body '{{' found after 'fn' — performing best-effort skip");
        let preview_tokens = input
            .0
            .iter()
            .take(20)
            .map(|t| &t.token_type)
            .collect::<Vec<_>>();
        println!("skip_function_declaration: failed to find '{{' after tokens: {preview_tokens:?}");

        // Try to skip to the next semicolon or right brace, otherwise to end of input
        let boundary = input
            .0
            .iter()
            .position(|t| matches!(t.token_type, TokenType::Semicolon | TokenType::RightBrace))
            .unwrap_or(input.len());
        let next = TokenSlice(&input.0[boundary.min(input.len())..]);
        let next_tok = next.peek().map(|t| &t.token_type);
        println!("skip_function_declaration: tolerant skip w/o '{{', next token: {next_tok:?}");
        return Ok((next, ())); // Do not error out, let the parser continue
    }

    // Consume from the first '{' until its matching '}' (track nested braces)
    let mut brace_depth = 0i32;
    let mut end = idx;
    while end < input.len() {
        match input.0[end].token_type {
            TokenType::LeftBrace => brace_depth += 1,
            TokenType::RightBrace => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    // Include this closing brace
                    end += 1;
                    break;
                }
            }
            _ => {}
        }
        end += 1;
    }

    if brace_depth != 0 || end > input.len() {
        // Be tolerant: if braces are unmatched, skip to the end of input rather than erroring.
        log::error!(
            "skip_function_declaration: unmatched braces while skipping function declaration — performing best-effort recovery"
        );
        let unmatched_preview = input
            .0
            .iter()
            .skip(idx)
            .take(20)
            .map(|t| &t.token_type)
            .collect::<Vec<_>>();
        println!("skip_function_declaration: unmatched braces, preview: {unmatched_preview:?}");
        let next = TokenSlice(&[]);
        return Ok((next, ())); // Recover by consuming the rest
    }

    log::debug!(
        "skip_function_declaration: consumed {} tokens of function declaration",
        end + 1
    );

    // Advance input past the function declaration
    let next = TokenSlice(&input.0[end..]);
    let remaining_next = next.peek().map(|t| &t.token_type);
    println!("skip_function_declaration: done, remaining next token: {remaining_next:?}");
    Ok((next, ()))
}

impl InputLength for TokenSlice<'_> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl InputTake for TokenSlice<'_> {
    fn take(&self, count: usize) -> Self {
        TokenSlice(&self.0[..count])
    }

    /// Splits the token slice at the given index, returning the suffix and prefix as new `TokenSlice` instances.
    ///
    /// The first element of the tuple is the suffix (tokens after the split point), and the second is the prefix (tokens before the split point).
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::InputTake;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    ///     Token::new(TokenType::Minus, "-", loc.clone()),
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// let (suffix, prefix) = slice.take_split(1);
    /// assert_eq!(prefix.len(), 1);
    /// assert_eq!(suffix.len(), 1);
    /// ```
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenSlice(suffix), TokenSlice(prefix))
    }
}

impl Slice<ops::RangeTo<usize>> for TokenSlice<'_> {
    /// ```
    fn slice(&self, range: ops::RangeTo<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl Slice<ops::RangeFrom<usize>> for TokenSlice<'_> {
    /// Returns a new `TokenSlice` starting from the specified index to the end of the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    ///     Token::new(TokenType::Minus, "-", loc.clone()),
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// let rest = slice.slice(1, 2); // Get tokens from index 1 to 2 (exclusive)
    /// assert_eq!(rest.len(), 1);
    /// ```
    fn slice(&self, range: ops::RangeFrom<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl Slice<ops::Range<usize>> for TokenSlice<'_> {
    fn slice(&self, range: ops::Range<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl<'a> InputIter for TokenSlice<'a> {
    type Item = &'a Token;
    type Iter = std::iter::Enumerate<std::slice::Iter<'a, Token>>;
    type IterElem = std::slice::Iter<'a, Token>;

    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }

    /// Returns the index of the first token that satisfies the given predicate, or `None` if no such token exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::InputIter;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = [
    ///     Token::new(TokenType::Identifier(InternedString::from("a")), "a", loc.clone()),
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    ///     Token::new(TokenType::Identifier(InternedString::from("b")), "b", loc.clone()),
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// let pos = slice.position(|t| t.token_type == TokenType::Plus);
    /// assert_eq!(pos, Some(1));
    /// ```
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    /// Returns the index for slicing if the token slice contains at least `count` tokens, or an error indicating how many more tokens are needed.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::{Needed, InputIter};
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![
    ///     Token::new(TokenType::Plus, "+", loc.clone())
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// assert_eq!(slice.slice_index(0), Ok(0));
    /// assert_eq!(slice.slice_index(1), Ok(1));
    /// assert_eq!(slice.slice_index(2), Err(Needed::new(1)));
    /// ```
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.0.len()))
        }
    }
}

impl Compare<Token> for TokenSlice<'_> {
    /// Compares the first token in the slice to the given token.
    ///
    /// Returns `CompareResult::Ok` if the first token matches, or `CompareResult::Error` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::{Compare, InputIter};
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let token = Token::new(TokenType::Plus, "+", loc.clone());
    /// let tokens = [token.clone()];
    /// let slice = TokenSlice::new(&tokens);
    /// assert_eq!(slice.compare(token), nom::CompareResult::Ok);
    /// ```
    fn compare(&self, t: Token) -> CompareResult {
        if self.0.first() == Some(&t) {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: Token) -> CompareResult {
        self.compare(t)
    }
}

impl ParseTo<Token> for TokenSlice<'_> {
    fn parse_to(&self) -> Option<Token> {
        self.0.first().cloned()
    }
}

impl InputTakeAtPosition for TokenSlice<'_> {
    type Item = Token;

    /// Splits the token slice at the first position where the predicate returns true.
    ///
    /// Returns a tuple containing the remaining tokens after the matched token and a slice containing the matched token itself. If no token satisfies the predicate, returns an error.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::{IResult, InputIter, InputTakeAtPosition};
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![
    ///     Token::new(TokenType::Minus, "-", loc.clone()),
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// type Error<'a> = nom::error::Error<TokenSlice<'a>>;
    /// let result: Result<_, nom::Err<Error>> = slice.split_at_position(|t| t.token_type == TokenType::Minus);
    /// assert!(result.is_ok());
    /// let (rest, matched) = result.unwrap();
    /// assert_eq!(rest.len(), 1);
    /// assert_eq!(matched.len(), 1);
    /// assert!(matches!(matched.first().unwrap().token_type, TokenType::Minus));
    /// assert!(matches!(rest.first().unwrap().token_type, TokenType::Plus));
    ///
    /// // Test case where no token matches the predicate
    /// let tokens = vec![
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// let result: Result<_, nom::Err<Error>> = slice.split_at_position(|t| t.token_type == TokenType::Minus);
    /// assert!(result.is_err()); // Should fail since no token matches the predicate
    /// ```
    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.split_first() {
            Some((token, rest)) => {
                if predicate(token.clone()) {
                    Ok((TokenSlice(rest), TokenSlice(&self.0[..1])))
                } else {
                    Err(Err::Error(E::from_error_kind(*self, ErrorKind::TakeTill1)))
                }
            }
            None => Err(Err::Error(E::from_error_kind(*self, ErrorKind::TakeTill1))),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true, requiring at least one matching token.
    ///
    /// Returns a tuple containing the remaining tokens after the match and the matched token as a slice. Returns an error if the input is empty or the first token does not satisfy the predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::error::ErrorKind;
    /// use nom::InputTakeAtPosition;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = [
    ///     Token::new(TokenType::Identifier(InternedString::from("foo")), "foo", loc.clone()),
    ///     Token::new(TokenType::Identifier(InternedString::from("bar")), "bar", loc.clone())
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// type Error<'a> = nom::error::Error<TokenSlice<'a>>;
    /// let result: Result<_, nom::Err<Error>> = slice.split_at_position1(|t| matches!(&t.token_type, TokenType::Identifier(_)), ErrorKind::Tag);
    /// assert!(result.is_ok());
    /// let (rest, matched) = result.unwrap();
    /// assert_eq!(rest.len(), 1);
    /// assert_eq!(matched.len(), 1);
    /// ```
    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        if self.0.is_empty() {
            return Err(Err::Error(E::from_error_kind(*self, e)));
        }

        match self.0.split_first() {
            Some((token, rest)) => {
                if predicate(token.clone()) {
                    if rest.is_empty() {
                        Ok((TokenSlice(&[]), *self))
                    } else {
                        Ok((TokenSlice(rest), TokenSlice(&self.0[..1])))
                    }
                } else {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                }
            }
            None => Err(Err::Error(E::from_error_kind(*self, e))),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true.
    ///
    /// Returns a tuple containing the remaining tokens after the split and the matched prefix. If no token satisfies the predicate, the entire slice is returned as the prefix and the remainder is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::InputTakeAtPosition;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![Token::new(TokenType::Plus, "+", loc.clone())];
    /// let slice = TokenSlice::new(&tokens);
    /// type Error<'a> = nom::error::Error<TokenSlice<'a>>;
    /// let result: Result<_, nom::Err<Error>> = slice.split_at_position_complete(|t| t.token_type == TokenType::Plus);
    /// assert!(result.is_ok());
    /// let (rest, matched) = result.unwrap();
    /// assert!(rest.is_empty());
    /// assert_eq!(matched.len(), 1);
    /// assert!(matches!(matched.first().unwrap().token_type, TokenType::Plus));
    /// ```
    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.split_first() {
            Some((token, rest)) => {
                if predicate(token.clone()) {
                    Ok((TokenSlice(rest), TokenSlice(&self.0[..1])))
                } else {
                    Ok((TokenSlice(&[]), *self))
                }
            }
            None => Ok((TokenSlice(&[]), *self)),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true, requiring at least one token to match.
    ///
    /// Returns a tuple containing the remaining tokens after the match and the matched prefix. If no token matches or the input is empty, returns an error.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_lexer::token::{Token, TokenType, Location};
    /// use medic_lexer::string_interner::InternedString;
    /// use medic_parser::parser::TokenSlice;
    /// use nom::error::ErrorKind;
    /// use nom::InputTakeAtPosition;
    ///
    /// let loc = Location { line: 1, column: 1, offset: 0 };
    /// let tokens = vec![
    ///     Token::new(TokenType::Plus, "+", loc.clone()),
    ///     Token::new(TokenType::Minus, "-", loc.clone())
    /// ];
    /// let slice = TokenSlice::new(&tokens);
    /// type Error<'a> = nom::error::Error<TokenSlice<'a>>;
    /// let result: Result<_, nom::Err<Error>> = slice.split_at_position1_complete(|t| t.token_type == TokenType::Plus, ErrorKind::Tag);
    /// assert!(result.is_ok());
    /// let (rest, matched) = result.unwrap();
    /// assert_eq!(rest.len(), 1);
    /// assert_eq!(matched.len(), 1);
    /// assert!(matches!(matched.first().unwrap().token_type, TokenType::Plus));
    /// assert_eq!(rest.first().unwrap().token_type, TokenType::Minus);
    /// ```
    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        if self.0.is_empty() {
            return Err(Err::Error(E::from_error_kind(*self, e)));
        }

        match self.0.split_first() {
            Some((token, rest)) => {
                if predicate(token.clone()) {
                    if rest.is_empty() {
                        Ok((TokenSlice(&[]), *self))
                    } else {
                        Ok((TokenSlice(rest), TokenSlice(&self.0[..1])))
                    }
                } else if self.0.len() == 1 {
                    Ok((TokenSlice(&[]), *self))
                } else {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                }
            }
            None => Err(Err::Error(E::from_error_kind(*self, e))),
        }
    }
}

/// Returns a parser that consumes and returns the next token if it satisfies the given predicate.
///
/// The returned parser checks the next token's type using the provided predicate. If the predicate returns `true`, the token is consumed and returned; otherwise, a parsing error with the specified `ErrorKind` is produced.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, take_token_if};
/// use nom::error::ErrorKind;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![Token::new(TokenType::Plus, "+", loc.clone())];
/// let input = TokenSlice::new(&tokens);
/// let parser = take_token_if(|tt| matches!(tt, TokenType::Plus), ErrorKind::Tag);
/// let result = parser(input);
/// assert!(result.is_ok());
/// ```
pub fn take_token_if<F>(
    predicate: F,
    err_kind: ErrorKind,
) -> impl Fn(TokenSlice<'_>) -> IResult<TokenSlice<'_>, Token>
where
    F: Fn(&TokenType) -> bool,
{
    move |input: TokenSlice<'_>| {
        if let Some(token) = input.first() {
            if predicate(&token.token_type) {
                Ok((input.advance(), token.clone()))
            } else {
                Err(Err::Error(NomError::new(input, err_kind)))
            }
        } else {
            Err(Err::Error(NomError::new(input, err_kind)))
        }
    }
}

/// Recover from a statement-level parse error by skipping tokens until a synchronization point.
///
/// Synchronization points are:
/// - Semicolon ';' (consumed)
/// - Right brace '}' (not consumed, so outer block can handle it)
/// - End of input
///
/// Emits a clinician-friendly diagnostic describing the error and the recovery action.
fn recover_to_sync<'a>(
    mut input: TokenSlice<'a>,
    err: &nom::Err<nom::error::Error<TokenSlice<'a>>>,
    diagnostics: &mut Vec<Diagnostic>,
    context: &str,
) -> TokenSlice<'a> {
    // Build base diagnostic from the underlying nom error
    let mut diag = diagnostics::diagnostic_from_nom_error(err);

    // Attach recovery action help text
    let recovery_help = "I skipped ahead to the next ';' or '}' to continue parsing this ";
    let combined_help = match diag.help.take() {
        Some(existing) => format!("{existing} Also: {context}. {recovery_help}{context}."),
        None => format!("{context}. {recovery_help}{context}."),
    };
    diag.help = Some(combined_help);
    diagnostics.push(diag);

    // Skip tokens until a synchronization point
    loop {
        match input.peek().map(|t| &t.token_type) {
            Some(TokenType::Semicolon) => {
                // Consume the semicolon and resume after it
                return input.advance();
            }
            Some(TokenType::RightBrace) => {
                // Do not consume; let the caller handle the block closing
                return input;
            }
            Some(_) => {
                input = input.advance();
            }
            None => {
                // End of input
                return input;
            }
        }
    }
}

/// Parses a block of statements enclosed in braces and returns a `BlockNode`.
///
/// The function expects the input to start with a left brace (`{`), parses zero or more statements until a right brace (`}`) is encountered, and returns the collected statements as a `BlockNode`.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{parse_block, TokenSlice};
/// use medic_ast::ast::BlockNode;
///
/// // Example token sequence: { let x = 1; }
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::LeftBrace, "{", loc.clone()),
///     Token::new(TokenType::Let, "let", loc.clone()),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc.clone()),
///     Token::new(TokenType::Equal, "=", loc.clone()),
///     Token::new(TokenType::Integer(1), "1", loc.clone()),
///     Token::new(TokenType::Semicolon, ";", loc.clone()),
///     Token::new(TokenType::RightBrace, "}", loc.clone()),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_block(input);
/// assert!(result.is_ok());
/// let (_, block) = result.unwrap();
/// assert!(matches!(block, BlockNode { .. }));
/// ```
pub fn parse_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    let (mut input, left_brace) =
        take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;
    let start_span: Span = left_brace.location.into();

    let mut statements = Vec::new();

    // Skip any leading semicolons before the first statement
    while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
        let (new_input, _) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
        input = new_input;
    }

    // Parse statements until we hit a right brace
    while !matches!(
        input.peek().map(|t| &t.token_type),
        Some(TokenType::RightBrace)
    ) {
        // Parse the statement (let each statement handle its own semicolon)
        let (new_input, stmt) = parse_statement(input)?;
        input = new_input;
        statements.push(stmt);

        // Skip any extra semicolons
        while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
            let (new_input, _) =
                take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
            input = new_input;
        }
    }

    // Consume the right brace
    let (input, right_brace) =
        take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;

    let span = Span {
        start: start_span.start,
        end: right_brace.location.offset + right_brace.lexeme.len(),
        line: start_span.line,
        column: start_span.column,
    };

    Ok((input, BlockNode { statements, span }))
}

/// A recovering variant of `parse_block` that records diagnostics and continues after errors.
pub fn parse_block_with_recovery<'a>(
    input: TokenSlice<'a>,
    diagnostics: &mut Vec<Diagnostic>,
) -> IResult<TokenSlice<'a>, BlockNode> {
    let (mut input, left_brace) =
        take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;
    let start_span: Span = left_brace.location.into();

    let mut statements = Vec::new();

    // Skip any leading semicolons before the first statement
    while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
        let (new_input, _) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
        input = new_input;
    }

    // Parse statements until we hit a right brace
    while !matches!(
        input.peek().map(|t| &t.token_type),
        Some(TokenType::RightBrace)
    ) {
        match parse_statement(input) {
            Ok((next, stmt)) => {
                input = next;
                statements.push(stmt);

                // Skip any extra semicolons
                while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
                    let (new_input, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(input)?;
                    input = new_input;
                }
            }
            Err(e) => {
                // Recover to next synchronization point and continue
                let recovered = recover_to_sync(input, &e, diagnostics, "block");
                // If no progress made, decide whether to break (for '}') or abort
                if recovered.len() == input.len() {
                    if matches!(
                        recovered.peek().map(|t| &t.token_type),
                        Some(TokenType::RightBrace)
                    ) {
                        break;
                    } else {
                        return Err(e);
                    }
                }
                input = recovered;
                // If we recovered at a semicolon, also skip any extra semicolons
                while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
                    let (new_input, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(input)?;
                    input = new_input;
                }
            }
        }
    }

    // Consume the right brace
    let (input, right_brace) =
        take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;

    let span = Span {
        start: start_span.start,
        end: right_brace.location.offset + right_brace.lexeme.len(),
        line: start_span.line,
        column: start_span.column,
    };

    Ok((input, BlockNode { statements, span }))
}

/// Get a binary operator from a token type
/// Maps a token type to its corresponding binary operator and associativity.
///
/// Returns a tuple containing the matching `BinaryOperator` and a boolean indicating whether the operator is right-associative (`true`) or left-associative (`false`). Returns `None` if the token type does not represent a binary operator.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::TokenType;
/// use medic_lexer::string_interner::InternedString;
/// use medic_ast::ast::BinaryOperator;
/// use medic_parser::parser::get_binary_operator;
///
/// assert_eq!(get_binary_operator(&TokenType::Plus), Some((BinaryOperator::Add, false)));
/// assert_eq!(get_binary_operator(&TokenType::DoubleStar), Some((BinaryOperator::Pow, true)));
/// assert_eq!(get_binary_operator(&TokenType::Identifier(InternedString::from("foo"))), None);
/// ```
pub fn get_binary_operator(token_type: &TokenType) -> Option<(BinaryOperator, bool)> {
    match token_type {
        // Assignment operator (right-associative, lowest precedence)
        TokenType::Equal => Some((BinaryOperator::Assign, true)),

        // Medical operators
        TokenType::Of => Some((BinaryOperator::Of, false)),
        TokenType::Per => Some((BinaryOperator::Per, false)),
        TokenType::Arrow => Some((BinaryOperator::UnitConversion, true)),

        // Standard operators
        TokenType::Plus => Some((BinaryOperator::Add, false)),
        TokenType::Minus => Some((BinaryOperator::Sub, false)),
        TokenType::Star => Some((BinaryOperator::Mul, false)),
        TokenType::Slash => Some((BinaryOperator::Div, false)),
        TokenType::Percent => Some((BinaryOperator::Mod, false)),
        TokenType::DoubleStar => Some((BinaryOperator::Pow, true)),
        TokenType::BitAnd => Some((BinaryOperator::BitAnd, false)),
        TokenType::BitOr => Some((BinaryOperator::BitOr, false)),
        TokenType::BitXor => Some((BinaryOperator::BitXor, false)), // Bitwise XOR is left-associative
        TokenType::Shl => Some((BinaryOperator::Shl, false)),
        TokenType::Shr => Some((BinaryOperator::Shr, false)),
        TokenType::EqualEqual => Some((BinaryOperator::Eq, false)),
        TokenType::NotEqual => Some((BinaryOperator::Ne, false)),
        TokenType::Less => Some((BinaryOperator::Lt, false)),
        TokenType::LessEqual => Some((BinaryOperator::Le, false)),
        TokenType::Greater => Some((BinaryOperator::Gt, false)),
        TokenType::GreaterEqual => Some((BinaryOperator::Ge, false)),
        TokenType::QuestionQuestion => Some((BinaryOperator::NullCoalesce, true)),
        TokenType::QuestionColon => Some((BinaryOperator::Elvis, true)),
        TokenType::Range => Some((BinaryOperator::Range, false)),
        TokenType::AndAnd => Some((BinaryOperator::And, false)),
        TokenType::OrOr => Some((BinaryOperator::Or, false)),
        _ => None,
    }
}

/// Get the precedence of a binary operator
/// Returns the precedence level of a binary operator.
///
/// Higher numbers indicate higher precedence, determining the order in which operators are evaluated in expressions.
///
/// # Examples
///
/// ```
/// use medic_ast::ast::BinaryOperator;
/// use medic_parser::parser::get_operator_precedence;
///
/// let precedence = get_operator_precedence(&BinaryOperator::Add);
/// assert!(precedence > 0);
/// assert_eq!(get_operator_precedence(&BinaryOperator::Or), 0);
/// ```
pub fn get_operator_precedence(op: &BinaryOperator) -> u8 {
    match op {
        // Assignment has the lowest precedence (right-associative)
        BinaryOperator::Assign => 0,

        // Unit conversion (highest precedence)
        BinaryOperator::UnitConversion => 16,

        // Range operator (has its own special handling in the parser)
        BinaryOperator::Range => 14,

        // Exponentiation
        BinaryOperator::Pow => 13,

        // Multiplication/Division/Modulo
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 12,

        // Addition/Subtraction
        BinaryOperator::Add | BinaryOperator::Sub => 11,

        // Bit shifts
        BinaryOperator::Shl | BinaryOperator::Shr => 9,

        // Bitwise AND
        BinaryOperator::BitAnd => 8,

        // Bitwise XOR
        BinaryOperator::BitXor => 7,

        // Bitwise OR
        BinaryOperator::BitOr => 6,

        // Comparison
        BinaryOperator::Lt | BinaryOperator::Le | BinaryOperator::Gt | BinaryOperator::Ge => 5,

        // Equality
        BinaryOperator::Eq | BinaryOperator::Ne => 4,

        // Elvis operator (?:)
        BinaryOperator::Elvis => 3,

        // Null-coalescing (??)
        BinaryOperator::NullCoalesce => 2,

        // Medical operators
        // 'of' has higher precedence than multiplication (e.g., '2 of 3 doses' -> (2 of (3 * doses)))
        BinaryOperator::Of => 15,
        // 'per' has lower precedence than multiplication (e.g., '5 mg per day' -> ((5 * mg) per day))
        BinaryOperator::Per => 5,

        // Logical AND
        BinaryOperator::And => 1,

        // Logical OR (lowest precedence)
        BinaryOperator::Or => 0,
    }
}

/// Parses a complete program consisting of zero or more statements.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, Location};
/// use medic_parser::parser::{parse_program, TokenSlice};
/// use medic_ast::ast::ProgramNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens: Vec<Token> = vec![];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_program(input);
/// assert!(result.is_ok());
/// ```
pub fn parse_program(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ProgramNode> {
    log::debug!("=== parse_program ===");
    log::debug!("Initial tokens: {}", input.0.len());

    if !input.0.is_empty() {
        log::debug!(
            "First token: {:?} at {}:{}",
            input.0[0].token_type,
            input.0[0].location.line,
            input.0[0].location.column
        );
    }

    // Strict parsing: require all tokens to be consumed by statements.
    // Stop on the first error and surface it (instead of silently returning leftover tokens).
    let mut input = input.skip_whitespace();
    let mut statements = Vec::new();

    while !input.is_empty() {
        // Skip any inter-statement whitespace
        input = input.skip_whitespace();
        if input.is_empty() {
            break;
        }
        let before_len = input.len();
        log::debug!("parse_program: parsing statement with {before_len} tokens remaining");
        // Also print to stdout for integration test diagnostics
        if let Some(tok) = input.peek() {
            println!(
                "parse_program: next token = {:?} at {}:{} (remaining: {})",
                tok.token_type, tok.location.line, tok.location.column, before_len
            );
        }

        match parse_statement(input) {
            Ok((next, stmt)) => {
                let after_len = next.len();
                if after_len == before_len {
                    // Defensive: parser must always consume tokens; otherwise we'd loop forever.
                    log::error!(
                        "Parser made no progress at token: {:?} at {}:{}",
                        input.peek().map(|t| &t.token_type),
                        input.peek().map(|t| t.location.line).unwrap_or(0),
                        input.peek().map(|t| t.location.column).unwrap_or(0)
                    );
                    return Err(nom::Err::Failure(nom::error::Error::new(
                        input,
                        ErrorKind::Many0,
                    )));
                }
                log::debug!(
                    "Statement parsed; consumed {} tokens",
                    before_len - after_len
                );
                statements.push(stmt);
                input = next.skip_whitespace();
            }
            Err(e) => {
                // Surface the error (don’t let many0 hide it by succeeding with leftovers)
                log::error!("Stopping parse due to statement error: {e:?}");
                // Print nearby tokens for better visibility in tests
                let preview: Vec<_> = input.0.iter().take(10).map(|t| &t.token_type).collect();
                println!("parse_program: error near tokens: {preview:?}");
                if let nom::Err::Error(ref er) = e {
                    if let Some(tok) = er.input.peek() {
                        log::error!(
                            "Error at token: {:?} ({}:{})",
                            tok.token_type,
                            tok.location.line,
                            tok.location.column
                        );
                        println!(
                            "parse_program: error at token = {:?} ({}:{})",
                            tok.token_type, tok.location.line, tok.location.column
                        );
                    }
                }
                return Err(e);
            }
        }
    }

    log::debug!(
        "Successfully parsed {} statements; no remaining tokens",
        statements.len()
    );
    Ok((input, ProgramNode { statements }))
}

/// A recovering variant of `parse_program` that continues after errors and collects diagnostics.
pub fn parse_program_recovering<'a>(
    input: TokenSlice<'a>,
    diagnostics: &mut Vec<Diagnostic>,
) -> IResult<TokenSlice<'a>, ProgramNode> {
    log::debug!("=== parse_program_recovering ===");
    let mut input = input.skip_whitespace();
    let mut statements = Vec::new();

    while !input.is_empty() {
        input = input.skip_whitespace();
        if input.is_empty() {
            break;
        }

        match parse_statement(input) {
            Ok((next, stmt)) => {
                statements.push(stmt);
                input = next.skip_whitespace();
            }
            Err(e) => {
                // Attempt to recover and continue parsing subsequent statements
                let mut recovered = recover_to_sync(input, &e, diagnostics, "top-level statement");

                // If we are stuck on an unmatched right brace at the top level, consume it with a warning
                if let Some(TokenType::RightBrace) = recovered.peek().map(|t| &t.token_type) {
                    let tok = recovered.first().cloned();
                    if let Some(tok) = tok {
                        diagnostics.push(Diagnostic {
                            severity: Severity::Warning,
                            message: "Unmatched closing brace '}' at top level".to_string(),
                            span: tok.location.into(),
                            help: Some(
                                "This '}' does not close any open block. It was ignored."
                                    .to_string(),
                            ),
                        });
                    }
                    recovered = recovered.advance();
                }

                // Skip any extra semicolons after recovery
                while let Some(TokenType::Semicolon) = recovered.peek().map(|t| &t.token_type) {
                    let (new_recovered, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(recovered)?;
                    recovered = new_recovered;
                }

                // If no progress could be made, abort to avoid infinite loop
                if recovered.len() == input.len() {
                    return Err(e);
                }
                input = recovered;
            }
        }
    }

    Ok((input, ProgramNode { statements }))
}

/// Wrapper that parses a program and returns a clinician-friendly Diagnostic on error
pub fn parse_program_with_diagnostics(input: TokenSlice<'_>) -> Result<ProgramNode, Diagnostic> {
    match parse_program(input) {
        Ok((_rest, program)) => Ok(program),
        Err(e) => Err(diagnostics::diagnostic_from_nom_error(&e)),
    }
}

/// Parses a pattern for use in match expressions.
///
/// Currently unimplemented; calling this function will result in a runtime error.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, parse_pattern};
/// use medic_ast::ast::PatternNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_pattern(slice);
/// assert!(result.is_ok());
/// ```
///
/// # Panics
///
/// This function will panic if the pattern is not properly formed.
/// Parses a pattern for use in match expressions.
///
/// Supports the following patterns:
/// - Identifier patterns (variable binding): `x`, `value`
/// - Wildcard pattern: `_`
/// - Literal patterns: `42`, `"hello"`, `true`, `false`
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, parse_pattern};
/// use medic_ast::ast::{PatternNode, IdentifierNode, LiteralNode};
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
///
/// // Test identifier pattern
/// let tokens = vec![
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_pattern(slice);
/// assert!(matches!(result, Ok((_, PatternNode::Identifier(_)))));
///
/// // Test wildcard pattern
/// let tokens = vec![
///     Token::new(TokenType::Underscore, "_", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_pattern(slice);
/// assert!(matches!(result, Ok((_, PatternNode::Wildcard))));
///
/// // Test integer literal pattern
/// let tokens = vec![
///     Token::new(TokenType::Integer(42), "42", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_pattern(slice);
/// assert!(matches!(result, Ok((_, PatternNode::Literal(LiteralNode::Int(42))))));
///
/// // Test string literal pattern
/// let tokens = vec![
///     Token::new(TokenType::String(InternedString::from("hello")), "\"hello\"", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_pattern(slice);
/// if let Ok((_, PatternNode::Literal(LiteralNode::String(s)))) = result {
///     assert_eq!(s, "hello");
/// } else {
///     panic!("Expected string literal pattern");
/// }
/// ```
pub fn parse_pattern(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    // Try to parse a parenthesized pattern first
    if let Ok((input, _)) =
        take_token_if(|tt| matches!(tt, TokenType::LeftParen), ErrorKind::Char)(input)
    {
        let (input, pattern) = parse_pattern(input)?;
        let (input, _) =
            take_token_if(|tt| matches!(tt, TokenType::RightParen), ErrorKind::Char)(input)?;
        return Ok((input, pattern));
    }

    // Try to parse a variant pattern like Some(x)
    if let Ok((input, variant)) = take_token_if(
        |tt| matches!(tt, TokenType::Identifier(_)),
        ErrorKind::Alpha,
    )(input)
    {
        if let TokenType::Identifier(variant_name) = &variant.token_type {
            // Check if the next token is an opening parenthesis
            if let Ok((input, _)) =
                take_token_if(|tt| matches!(tt, TokenType::LeftParen), ErrorKind::Char)(input)
            {
                // Parse the inner pattern
                let (input, inner_pattern) = parse_pattern(input)?;
                let (input, _) = take_token_if(
                    |tt| matches!(tt, TokenType::RightParen),
                    ErrorKind::Char,
                )(input)?;

                // Create a variant pattern
                return Ok((
                    input,
                    PatternNode::Variant {
                        name: variant_name.to_string(),
                        inner: Box::new(inner_pattern),
                    },
                ));
            } else {
                // It's just a simple identifier pattern
                return Ok((
                    input,
                    PatternNode::Identifier(IdentifierNode {
                        name: variant_name.to_string(),
                    }),
                ));
            }
        }
    }

    // Try to parse an identifier pattern
    if let Ok((input, ident)) = take_token_if(
        |tt| matches!(tt, TokenType::Identifier(_)),
        ErrorKind::Alpha,
    )(input)
    {
        if let TokenType::Identifier(name) = &ident.token_type {
            return Ok((
                input,
                PatternNode::Identifier(IdentifierNode {
                    name: name.to_string(),
                }),
            ));
        }
    }

    // Try to parse a wildcard pattern
    if let Ok((input, _)) =
        take_token_if(|tt| matches!(tt, TokenType::Underscore), ErrorKind::Char)(input)
    {
        return Ok((input, PatternNode::Wildcard));
    }

    // Try to parse a literal pattern
    if !input.0.is_empty() {
        match &input.0[0].token_type {
            TokenType::Integer(n) => {
                let input = input.advance();
                return Ok((input, PatternNode::Literal(LiteralNode::Int(*n))));
            }
            TokenType::String(s) => {
                let input = input.advance();
                return Ok((
                    input,
                    PatternNode::Literal(LiteralNode::String(s.to_string())),
                ));
            }
            TokenType::Boolean(b) => {
                let input = input.advance();
                return Ok((input, PatternNode::Literal(LiteralNode::Bool(*b))));
            }
            _ => {}
        }
    }

    // If we get here, we couldn't parse a valid pattern
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        ErrorKind::Alt,
    )))
}

/// ```
pub fn parse_for_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    debug!("Starting parse_for_statement with input: {input:?}");

    // Parse 'for'
    let (input, for_tok) = take_token_if(|tt| matches!(tt, TokenType::For), ErrorKind::Tag)(input)?;

    // Helper to skip whitespace between parts
    let input = input.skip_whitespace();

    // Parse loop variable identifier
    let (input_after_ident, ident_tok) = take_token_if(
        |tt| matches!(tt, TokenType::Identifier(_)),
        ErrorKind::Alpha,
    )(input)?;

    let var_name = if let TokenType::Identifier(name) = &ident_tok.token_type {
        name.to_string()
    } else {
        // Safety: guarded by matcher above
        unreachable!()
    };
    let variable = IdentifierNode { name: var_name };

    // Expect 'in'
    let input = input_after_ident.skip_whitespace();
    let (input, _) = take_token_if(|tt| matches!(tt, TokenType::In), ErrorKind::Tag)(input)?;

    // Parse iterable expression
    let input = input.skip_whitespace();
    let (input, iterable) = parse_expression(input)?;

    // Parse body block
    let input = input.skip_whitespace();
    let (input, body) = parse_block(input)?;

    // Build span from 'for' start to end of body
    let span = Span {
        start: for_tok.location.offset,
        end: body.span.end,
        line: for_tok.location.line as u32,
        column: for_tok.location.column as u32,
    };

    let node = ForNode {
        variable,
        iterable,
        body,
        span,
    };

    Ok((input, StatementNode::For(Box::new(node))))
}

/// Parses a match statement from the input token slice.
///
/// Currently unimplemented; calling this function will panic.
///
/// # Panics
///
/// Always panics with a "not implemented" message.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, parse_match_statement};
/// use medic_ast::ast::StatementNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Match, "match", loc.clone()),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc.clone()),
///     Token::new(TokenType::LeftBrace, "{", loc.clone()),
///     Token::new(TokenType::RightBrace, "}", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let result = parse_match_statement(slice);
/// assert!(result.is_ok());
/// ```
///
/// # Panics
///
/// This function will panic if the match statement is not properly formed.
/// Parses a match statement from the input token slice.
///
/// A match statement has the form:
/// ```
/// # use medic_lexer::token::{Token, TokenType, Location};
/// # use medic_lexer::string_interner::InternedString;
/// # use medic_parser::parser::{TokenSlice, parse_match_statement};
/// # let loc = Location { line: 1, column: 1, offset: 0 };
/// # let tokens = vec![
/// #     Token::new(TokenType::Match, "match", loc.clone()),
/// #     Token::new(TokenType::Integer(42), "42", loc.clone()),
/// #     Token::new(TokenType::LeftBrace, "{", loc.clone()),
/// #     Token::new(TokenType::RightBrace, "}", loc.clone()),
/// # ];
/// # let slice = TokenSlice::new(&tokens);
/// # let result = parse_match_statement(slice);
/// # assert!(result.is_ok());
/// ```
pub fn parse_match_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    debug!("Starting parse_match_statement with input: {input:?}");

    // Parse the 'match' keyword
    debug!(
        "Starting to parse match statement. Input length: {}",
        input.0.len()
    );
    let (input, _) = match take_token_if(|tt| matches!(tt, TokenType::Match), ErrorKind::Tag)(input)
    {
        Ok(result) => {
            debug!("Successfully parsed 'match' keyword");
            result
        }
        Err(e) => {
            debug!("Failed to parse 'match' keyword: {e:?}");
            return Err(e);
        }
    };

    debug!(
        "After 'match' keyword, remaining input length: {}",
        input.0.len()
    );

    // Ensure there's at least one token after 'match'
    if input.0.is_empty() {
        debug!("Unexpected end of input after 'match' keyword");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Eof,
        )));
    }

    // Parse the expression to match against using only the tokens before the opening brace
    let lookahead = input.skip_whitespace();
    let expr_end = lookahead
        .0
        .iter()
        .position(|t| t.token_type == TokenType::LeftBrace)
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(lookahead, ErrorKind::Tag)))?;

    if expr_end == 0 {
        debug!("Expected expression before opening brace in match statement");
        return Err(nom::Err::Error(nom::error::Error::new(
            lookahead,
            ErrorKind::Tag,
        )));
    }

    // Parse the expression from the sliced tokens
    let (_, expr) = match parse_expression(TokenSlice(&lookahead.0[..expr_end])) {
        Ok((_, expr)) => {
            debug!("Successfully parsed match expression (sliced): {expr:?}");
            Ok((TokenSlice(&lookahead.0[..expr_end]), expr))
        }
        Err(e) => {
            debug!("Failed to parse sliced expression before '{{': {e:?}");
            Err(e)
        }
    }?;

    // Set input to start at the '{' for arm parsing
    let input = TokenSlice(&lookahead.0[expr_end..]);

    debug!("Successfully parsed match expression: {expr:?}");
    debug!(
        "Remaining input after expression (should start with '{{'): {:?}",
        input.0.iter().map(|t| &t.token_type).collect::<Vec<_>>()
    );

    // Parse the opening brace
    debug!(
        "Looking for opening brace. Next token: {:?}",
        input.0.first().map(|t| &t.token_type)
    );
    let result = take_token_if(|tt| matches!(tt, TokenType::LeftBrace), ErrorKind::Tag)(input);

    let (input, _) = match result {
        Ok((rest, _)) => {
            debug!("Successfully parsed opening brace");
            debug!(
                "Remaining after opening brace: {:?}",
                rest.0.iter().map(|t| &t.token_type).collect::<Vec<_>>()
            );
            (rest, ())
        }
        Err(e) => {
            debug!(
                "Failed to parse opening brace. Expected '{{', found: {:?}",
                input.0.first().map(|t| &t.token_type)
            );
            return Err(e);
        }
    };

    // Check for empty match block first
    debug!(
        "Checking for empty match block. Next token: {:?}",
        input.0.first().map(|t| &t.token_type)
    );
    let (input, arms) =
        match take_token_if(|tt| matches!(tt, TokenType::RightBrace), ErrorKind::Tag)(input) {
            Ok((input, _)) => {
                debug!("Found empty match block");
                debug!(
                    "Remaining after empty match block: {:?}",
                    input.0.iter().map(|t| &t.token_type).collect::<Vec<_>>()
                );
                (input, Vec::new())
            }
            Err(e) => {
                debug!("No empty match block found: {e:?}");
                // Parse match arms
                let mut arms = Vec::new();
                let mut input = input;

                // Parse match arms until we hit a closing brace
                loop {
                    // Try to parse a pattern
                    let (new_input, pattern) = parse_pattern(input)?;
                    input = new_input;

                    // Parse the arrow
                    let (new_input, _) = take_token_if(
                        |tt| matches!(tt, TokenType::FatArrow),
                        ErrorKind::Tag,
                    )(input)?;
                    input = new_input;

                    // Parse the arm body expression up to the next top-level ',' or '}'
                    let look = input.skip_whitespace();
                    debug!(
                        "Arm body lookahead before slice: {:?}",
                        look.0.iter().map(|t| &t.token_type).collect::<Vec<_>>()
                    );
                    let mut idx = 0usize;
                    let mut paren = 0i32;
                    let mut bracket = 0i32;
                    let mut brace = 0i32;
                    let mut boundary: Option<usize> = None;
                    while idx < look.0.len() {
                        match look.0[idx].token_type {
                            TokenType::LeftParen => paren += 1,
                            TokenType::RightParen => paren = (paren - 1).max(0),
                            TokenType::LeftBracket => bracket += 1,
                            TokenType::RightBracket => bracket = (bracket - 1).max(0),
                            TokenType::LeftBrace => brace += 1,
                            TokenType::RightBrace => {
                                if paren == 0 && bracket == 0 && brace == 0 {
                                    boundary = Some(idx);
                                    break;
                                } else {
                                    brace = (brace - 1).max(0);
                                }
                            }
                            TokenType::Comma => {
                                if paren == 0 && bracket == 0 && brace == 0 {
                                    boundary = Some(idx);
                                    break;
                                }
                            }
                            _ => {}
                        }
                        idx += 1;
                    }

                    let end_idx = boundary.ok_or_else(|| {
                        nom::Err::Error(nom::error::Error::new(look, ErrorKind::Tag))
                    })?;

                    debug!(
                        "Arm body boundary at idx {} token {:?}",
                        end_idx, look.0[end_idx].token_type
                    );

                    // Parse body on the sliced range [0..end_idx)
                    let (_, expr) = parse_expression(TokenSlice(&look.0[..end_idx]))?;

                    // Advance input to the boundary token (',' or '}'),
                    // letting the following logic consume it appropriately.
                    input = TokenSlice(&look.0[end_idx..]);
                    debug!(
                        "Input at boundary token: {:?}",
                        input.0.iter().map(|t| &t.token_type).collect::<Vec<_>>()
                    );

                    // Add the arm
                    arms.push(MatchArmNode {
                        pattern,
                        body: Box::new(expr),
                    });

                    // Check for closing brace first
                    if let Ok((new_input, _)) = take_token_if(
                        |tt| matches!(tt, TokenType::RightBrace),
                        ErrorKind::Tag,
                    )(input)
                    {
                        input = new_input;
                        break;
                    }

                    // If no closing brace, expect a comma before the next arm
                    let (new_input, _) =
                        take_token_if(|tt| matches!(tt, TokenType::Comma), ErrorKind::Tag)(input)?;
                    input = new_input;
                }

                // The loop only breaks after consuming the closing brace,
                // so no need to require another '}' here.
                (input, arms)
            }
        };

    // Create a span that covers the entire match expression
    let span = if !arms.is_empty() {
        // If we have arms, the span goes from the start of the expression to the end of the last arm's body
        let _first_arm = arms.first().unwrap();
        let last_arm = arms.last().unwrap();
        let last_arm_span = last_arm.body.span();

        Span {
            start: expr.span().start,
            end: last_arm_span.end,
            line: expr.span().line,
            column: expr.span().column,
        }
    } else {
        // If no arms, just use the expression span
        *expr.span()
    };

    Ok((
        input,
        StatementNode::Match(Box::new(MatchNode {
            expr: Box::new(expr),
            arms,
            span,
        })),
    ))
}

/// Returns `true` if the given operator is a comparison operator (`==`, `!=`, `<`, `<=`, `>`, or `>=`).
pub(crate) fn is_comparison_operator(op: &BinaryOperator) -> bool {
    matches!(
        op,
        BinaryOperator::Eq
            | BinaryOperator::Ne
            | BinaryOperator::Lt
            | BinaryOperator::Le
            | BinaryOperator::Gt
            | BinaryOperator::Ge
    )
}

#[cfg(test)]
mod tests;
