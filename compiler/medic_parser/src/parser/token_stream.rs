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
    /// Constructs a `TokenStream` from a slice of tokens, starting at the beginning of the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo".into())];
    /// let stream = TokenStream::new(&tokens);
    /// assert!(!stream.is_empty());
    /// ```
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenStream {
            tokens,
            position: 0,
    }

    /// Returns a reference to the current token without advancing the stream.
    ///
    /// Returns `None` if the stream has been fully consumed.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo".into())];
    /// let stream = TokenStream::new(&tokens);
    /// assert_eq!(stream.peek().unwrap().token_type, TokenType::Identifier);
    /// ```    pub fn peek(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    /// Returns the current token and advances the stream position by one.
    ///
    /// Returns `None` if there are no more tokens.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Number, "42")];
    /// let mut stream = TokenStream::new(&tokens);
    /// assert_eq!(stream.next().unwrap().token_type, TokenType::Identifier);
    /// assert_eq!(stream.next().unwrap().token_type, TokenType::Number);
    /// assert!(stream.next().is_none());
    /// ```    pub fn next(&mut self) -> Option<&Token> {
        let token = self.peek();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    /// Returns a slice of the tokens from the current position to the end of the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Number, "42")];
    /// let mut stream = TokenStream::new(&tokens);
    /// stream.next();
    /// let remaining = stream.remaining();
    /// assert_eq!(remaining.len(), 1);
    /// assert_eq!(remaining[0].token_type, TokenType::Number);
    /// ```    pub fn remaining(&self) -> &'a [Token] {
        &self.tokens[self.position..]
    }

    /// Constructs a `nom` parsing error at the current position in the token stream with the specified error kind.
    ///
    /// # Examples
    ///
    /// ```
    /// use nom::error::ErrorKind;
    /// let tokens = vec![];
    /// let stream = TokenStream::new(&tokens);
    /// let err = stream.error::<()> (ErrorKind::Tag);
    /// assert!(matches!(err, nom::Err::Error(_)));
    /// ```    pub fn error<T>(&self, kind: nom::error::ErrorKind) -> nom::Err<nom::error::Error<&'a [Token]>> {
        nom::Err::Error(nom::error::Error::new(self.remaining(), kind))
    }

    /// Attempts to consume the next token if it matches the expected type.
    ///
    /// If the current token matches the provided `expected_type`, advances the stream and returns a reference to the token.
    /// Returns a `nom` error of kind `Tag` if the token does not match or if the stream is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = &[Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Plus, "+")];
    /// let mut stream = TokenStream::new(tokens);
    /// let token = stream.match_token(&TokenType::Identifier).unwrap();
    /// assert_eq!(token.token_type, TokenType::Identifier);
    /// ```    pub fn match_token(&mut self, expected_type: &TokenType) -> Result<&Token, nom::Err<nom::error::Error<&'a [Token]>>> {
        match self.peek() {
            Some(token) if &token.token_type == expected_type => {
                self.next();
                Ok(token)
            }
            _ => Err(self.error(nom::error::ErrorKind::Tag)),
        }
    }

    /// Attempts to match and consume the next token if its type is in the provided list.
    ///
    /// Returns the remaining tokens and the matched token if successful; otherwise, returns a `nom` error of kind `Tag`.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = &[Token::new(TokenType::Plus), Token::new(TokenType::Minus)];
    /// let mut stream = TokenStream::new(tokens);
    /// let result = stream.match_any(&[TokenType::Plus, TokenType::Star]);
    /// assert!(result.is_ok());
    /// ```    pub fn match_any(&mut self, expected: &[TokenType]) -> IResult<&'a [Token], &'a Token> {
        match self.peek() {
            Some(token) if expected.contains(&token.token_type) => {
                self.position += 1;
                Ok((self.remaining(), token))
            }
            _ => Err(self.error(ErrorKind::Tag)),
        }
    }

    /// Returns `true` if the next token matches the specified token type without advancing the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Plus, 0), Token::new(TokenType::Minus, 1)];
    /// let stream = TokenStream::new(&tokens);
    /// assert!(stream.peek_token(&TokenType::Plus));
    /// assert!(!stream.peek_token(&TokenType::Minus));
    /// ```    pub fn peek_token(&self, expected: &TokenType) -> bool {
        matches!(self.peek(), Some(token) if token.token_type == *expected)
    }

    /// Returns `true` if there are no more tokens to process in the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![];
    /// let stream = TokenStream::new(&tokens);
    /// assert!(stream.is_empty());
    /// ```    pub fn is_empty(&self) -> bool {
        self.position >= self.tokens.len()
    }
}
