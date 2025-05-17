use medic_lexer::{Token, TokenType};
use nom::{
    error::{Error, ErrorKind},
    Err as NomErr, IResult,
};

/// A stream of tokens from the lexer
pub struct TokenStream<'a> {
    /// The tokens being parsed
    tokens: &'a [Token],
    /// Current position in the token stream
    position: usize,
}

impl<'a> TokenStream<'a> {
    /// Create a new token stream from a slice of tokens
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenStream {
            tokens,
            position: 0,
        }
    }

    /// Get the current token without advancing
    pub fn peek(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    /// Get the next token and advance the position
    pub fn next(&mut self) -> Option<&Token> {
        let token = self.peek();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    /// Get the remaining tokens as a slice
    pub fn remaining(&self) -> &'a [Token] {
        &self.tokens[self.position..]
    }

    /// Create a nom error at the current position
    pub fn error<T>(&self, kind: nom::error::ErrorKind) -> nom::Err<nom::error::Error<&'a [Token]>> {
        nom::Err::Error(nom::error::Error::new(self.remaining(), kind))
    }

    /// Match a specific token type and consume it
    pub fn match_token(&mut self, expected_type: &TokenType) -> Result<&Token, nom::Err<nom::error::Error<&'a [Token]>>> {
        match self.peek() {
            Some(token) if &token.token_type == expected_type => {
                self.next();
                Ok(token)
            }
            _ => Err(self.error(nom::error::ErrorKind::Tag)),
        }
    }

    /// Match any token type from a list and consume it
    pub fn match_any(&mut self, expected: &[TokenType]) -> IResult<&'a [Token], &'a Token> {
        match self.peek() {
            Some(token) if expected.contains(&token.token_type) => {
                self.position += 1;
                Ok((self.remaining(), token))
            }
            _ => Err(self.error(ErrorKind::Tag)),
        }
    }

    /// Look ahead at the next token without consuming it
    pub fn peek_token(&self, expected: &TokenType) -> bool {
        matches!(self.peek(), Some(token) if token.token_type == *expected)
    }

    /// Check if we're at the end of input
    pub fn is_empty(&self) -> bool {
        self.position >= self.tokens.len()
    }
}
