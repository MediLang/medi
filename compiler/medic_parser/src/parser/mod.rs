//! Parser for the Medi language
//!
//! This module implements a recursive descent parser for the Medi language.
//! It transforms a sequence of tokens into an abstract syntax tree (AST).

#![allow(dead_code)]
#![allow(unused_imports)]

use std::ops;

use nom::error::{Error as NomError, ErrorKind, ParseError};
use nom::{
    multi::many0, Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, ParseTo, Slice,
};

use medic_ast::ast::{
    BinaryExpressionNode, BinaryOperator, BlockNode, CallExpressionNode, ExpressionNode,
    IdentifierNode, LiteralNode, MemberExpressionNode, PatternNode, ProgramNode, StatementNode,
};
use medic_lexer::token::{Token, TokenType};

// Declare submodules
mod expressions;
mod identifiers;
mod literals;
mod statements;

// Re-export commonly used functions from submodules
pub use expressions::*;
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
    /// let tokens = vec![Token::new(TokenType::Plus), Token::new(TokenType::Minus)];
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
    /// let tokens = vec![Token::new(TokenType::Plus), Token::new(TokenType::Minus)];
    /// let slice = TokenSlice::new(&tokens);
    /// let rest = slice.slice(1..);
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
    /// let tokens = [Token::Identifier("a".into()), Token::Plus, Token::Identifier("b".into())];
    /// let slice = TokenSlice::new(&tokens);
    /// let pos = slice.position(|t| matches!(t, Token::Plus));
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
    /// use medic_parser::TokenSlice;
    /// use nom::Needed;
    ///
    /// let tokens = vec![];
    /// let slice = TokenSlice::new(&tokens);
    /// assert_eq!(slice.slice_index(0), Ok(0));
    /// assert_eq!(slice.slice_index(1), Err(Needed::new(1)));
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
    /// use medic_lexer::{Token, TokenType};
    /// use medic_parser::TokenSlice;
    /// use nom::CompareResult;
    ///
    /// let tokens = [Token::new(TokenType::Plus, 0..1)];
    /// let slice = TokenSlice::new(&tokens);
    /// assert_eq!(slice.compare(tokens[0]), CompareResult::Ok);
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
    /// use medic_lexer::Token;
    /// use medic_parser::TokenSlice;
    ///
    /// let tokens = vec![Token::from_type(TokenType::Plus), Token::from_type(TokenType::Minus)];
    /// let slice = TokenSlice(&tokens);
    /// let result = slice.split_at_position(|t| t.token_type == TokenType::Minus);
    /// assert!(result.is_ok());
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
    /// use medic_lexer::Token;
    /// use medic_parser::TokenSlice;
    /// use nom::error::ErrorKind;
    ///
    /// let tokens = [Token::new_identifier("foo"), Token::new_identifier("bar")];
    /// let slice = TokenSlice(&tokens);
    /// let result = slice.split_at_position1(|t| t.is_identifier(), ErrorKind::Tag);
    /// assert!(result.is_ok());
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
    /// use medic_lexer::Token;
    /// use medic_parser::TokenSlice;
    ///
    /// let tokens = vec![Token::new_plus(), Token::new_minus()];
    /// let slice = TokenSlice::new(&tokens);
    /// let (rest, prefix) = slice.split_at_position_complete(|t| t.is_minus()).unwrap().0;
    /// assert_eq!(prefix.len(), 1);
    /// assert_eq!(rest.len(), 1);
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
    /// use medic_parser::TokenSlice;
    /// use medic_lexer::{Token, TokenType};
    /// let tokens = vec![Token::new(TokenType::Identifier, 0), Token::new(TokenType::Plus, 1)];
    /// let slice = TokenSlice(&tokens);
    /// let result = slice.split_at_position1_complete(|t| t.token_type == TokenType::Plus, nom::error::ErrorKind::Tag);
    /// assert!(result.is_ok());
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
/// use medic_lexer::{Token, TokenType};
/// use medic_parser::{TokenSlice, take_token_if};
/// use nom::error::ErrorKind;
///
/// let tokens = vec![Token::new(TokenType::Plus, 0..1)];
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

/// Parses a block of statements enclosed in braces and returns a `BlockNode`.
///
/// The function expects the input to start with a left brace (`{`), parses zero or more statements until a right brace (`}`) is encountered, and returns the collected statements as a `BlockNode`.
///
/// # Examples
///
/// ```
/// use medic_lexer::{Token, TokenType};
/// use medic_parser::{parse_block, TokenSlice};
/// use medic_ast::BlockNode;
///
/// // Example token sequence: { let x = 1; }
/// let tokens = vec![
///     Token::new(TokenType::LeftBrace, 0..1),
///     // ... tokens for a statement ...
///     Token::new(TokenType::RightBrace, 10..11),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_block(input);
/// assert!(result.is_ok());
/// let (_, block) = result.unwrap();
/// assert!(matches!(block, BlockNode { .. }));
/// ```
pub fn parse_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    let (mut input, _) =
        take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;

    let mut statements = Vec::new();

    // Parse statements until we hit a right brace
    while !matches!(
        input.peek().map(|t| &t.token_type),
        Some(TokenType::RightBrace)
    ) {
        let (new_input, stmt) = parse_statement(input)?;
        input = new_input;
        statements.push(stmt);
    }

    // Consume the right brace
    let (input, _) = take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;

    Ok((input, BlockNode { statements }))
}

/// Get a binary operator from a token type
/// Maps a token type to its corresponding binary operator and associativity.
///
/// Returns a tuple containing the matching `BinaryOperator` and a boolean indicating whether the operator is right-associative (`true`) or left-associative (`false`). Returns `None` if the token type does not represent a binary operator.
///
/// # Examples
///
/// ```
/// use medic_lexer::TokenType;
/// use medic_ast::BinaryOperator;
/// use medic_parser::get_binary_operator;
///
/// assert_eq!(get_binary_operator(&TokenType::Plus), Some((BinaryOperator::Add, false)));
/// assert_eq!(get_binary_operator(&TokenType::DoubleStar), Some((BinaryOperator::Pow, true)));
/// assert_eq!(get_binary_operator(&TokenType::Identifier), None);
/// ```
pub fn get_binary_operator(token_type: &TokenType) -> Option<(BinaryOperator, bool)> {
    match token_type {
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
        TokenType::And => Some((BinaryOperator::And, false)),
        TokenType::Or => Some((BinaryOperator::Or, false)),
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
/// use medic_ast::BinaryOperator;
/// use parser::get_operator_precedence;
///
/// assert!(get_operator_precedence(&BinaryOperator::Mul) > get_operator_precedence(&BinaryOperator::Add));
/// assert_eq!(get_operator_precedence(&BinaryOperator::Or), 0);
/// ```
pub fn get_operator_precedence(op: &BinaryOperator) -> u8 {
    match op {
        // Unit conversion (highest precedence)
        BinaryOperator::UnitConversion => 16,

        // Medical 'of' operator (e.g., '2 of 3 doses')
        BinaryOperator::Of => 15,

        // Range operator (has its own special handling in the parser)
        BinaryOperator::Range => 14,

        // Exponentiation
        BinaryOperator::Pow => 13,

        // Multiplication/Division/Modulo
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 12,

        // Addition/Subtraction
        BinaryOperator::Add | BinaryOperator::Sub => 11,

        // Medical 'per' operator (e.g., '5 mg per day')
        BinaryOperator::Per => 10,

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
/// use medic_parser::{parse_program, TokenSlice};
/// use medic_ast::ProgramNode;
///
/// let tokens = vec![];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_program(input);
/// assert!(result.is_ok());
/// ```
pub fn parse_program(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ProgramNode> {
    let (input, statements) = many0(parse_statement)(input)?;
    Ok((input, ProgramNode { statements }))
}

/// Parses a pattern for use in match expressions.
///
/// Currently unimplemented; calling this function will result in a runtime error.
///
/// # Examples
///
/// ```should_panic
/// // This will panic because pattern parsing is not yet implemented.
/// let tokens = TokenSlice::new(&[]);
/// let _ = parse_pattern(tokens);
/// ```
pub fn parse_pattern(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    // TODO: Implement pattern parsing
    todo!("Pattern parsing not implemented yet");
}

/// ```
pub fn parse_for_statement(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // TODO: Implement for statement parsing
    todo!("For statement parsing not implemented yet");
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
/// ```should_panic
/// // This will panic because match statement parsing is not implemented.
/// let tokens = TokenSlice::new(&[]);
/// let _ = parse_match_statement(tokens);
/// ```
pub fn parse_match_statement(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // TODO: Implement match statement parsing
    todo!("Match statement parsing not implemented yet");
}

#[cfg(test)]
mod tests;
