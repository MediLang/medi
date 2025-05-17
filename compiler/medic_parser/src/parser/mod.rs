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

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenSlice(suffix), TokenSlice(prefix))
    }
}

impl Slice<ops::RangeTo<usize>> for TokenSlice<'_> {
    fn slice(&self, range: ops::RangeTo<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl Slice<ops::RangeFrom<usize>> for TokenSlice<'_> {
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

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.0.len()))
        }
    }
}

impl Compare<Token> for TokenSlice<'_> {
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

/// Helper function to take a token if it matches a predicate
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

/// Parse a block of statements
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
/// Returns the operator and whether it's right-associative (true) or left-associative (false)
pub fn get_binary_operator(token_type: &TokenType) -> Option<(BinaryOperator, bool)> {
    match token_type {
        // Logical OR (left-associative)
        TokenType::Or => Some((BinaryOperator::Or, false)),

        // Logical AND (left-associative)
        TokenType::And => Some((BinaryOperator::And, false)),

        // Equality (left-associative)
        TokenType::EqualEqual => Some((BinaryOperator::Eq, false)),
        TokenType::NotEqual => Some((BinaryOperator::Ne, false)),

        // Comparison (left-associative)
        TokenType::Less => Some((BinaryOperator::Lt, false)),
        TokenType::LessEqual => Some((BinaryOperator::Le, false)),
        TokenType::Greater => Some((BinaryOperator::Gt, false)),
        TokenType::GreaterEqual => Some((BinaryOperator::Ge, false)),

        // Addition/Subtraction (left-associative)
        TokenType::Plus => Some((BinaryOperator::Add, false)),
        TokenType::Minus => Some((BinaryOperator::Sub, false)),

        // Multiplication/Division/Modulo (left-associative)
        TokenType::Star => Some((BinaryOperator::Mul, false)),
        TokenType::Slash => Some((BinaryOperator::Div, false)),
        TokenType::Percent => Some((BinaryOperator::Mod, false)),

        // Not a binary operator
        _ => None,
    }
}

/// Get the precedence of a binary operator
/// Higher numbers mean higher precedence
pub fn get_operator_precedence(op: &BinaryOperator) -> u8 {
    match op {
        // Logical OR (lowest precedence)
        BinaryOperator::Or => 1,

        // Logical AND
        BinaryOperator::And => 2,

        // Null-coalescing (??)
        BinaryOperator::NullCoalesce => 3,

        // Elvis operator (?:)
        BinaryOperator::Elvis => 4,

        // Equality
        BinaryOperator::Eq | BinaryOperator::Ne => 5,

        // Comparison
        BinaryOperator::Lt | BinaryOperator::Le | BinaryOperator::Gt | BinaryOperator::Ge => 6,

        // Bitwise OR
        BinaryOperator::BitOr => 7,

        // Bitwise XOR
        BinaryOperator::BitXor => 8,

        // Bitwise AND
        BinaryOperator::BitAnd => 9,

        // Bit shifts
        BinaryOperator::Shl | BinaryOperator::Shr => 10,

        // Addition/Subtraction
        BinaryOperator::Add | BinaryOperator::Sub => 11,

        // Multiplication/Division/Modulo
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 12,

        // Exponentiation (highest precedence among binary operators)
        BinaryOperator::Pow => 13,

        // Range operator (has its own special handling in the parser)
        BinaryOperator::Range => 14,
    }
}

/// Parse a program (a list of statements)
pub fn parse_program(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ProgramNode> {
    let (input, statements) = many0(parse_statement)(input)?;
    Ok((input, ProgramNode { statements }))
}

/// Parse a pattern (used in match expressions)
pub fn parse_pattern(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    // TODO: Implement pattern parsing
    todo!("Pattern parsing not implemented yet");
}

/// Parse a for statement
pub fn parse_for_statement(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // TODO: Implement for statement parsing
    todo!("For statement parsing not implemented yet");
}

/// Parse a match statement
pub fn parse_match_statement(_input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // TODO: Implement match statement parsing
    todo!("Match statement parsing not implemented yet");
}

#[cfg(test)]
mod tests;
