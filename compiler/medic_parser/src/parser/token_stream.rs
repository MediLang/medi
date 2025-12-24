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
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            let token = &self.tokens[self.position];
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Get the remaining tokens as a slice
    pub fn remaining(&self) -> &'a [Token] {
        &self.tokens[self.position..]
    }

    /// Create a nom error at the current position
    pub fn error(&self, kind: nom::error::ErrorKind) -> nom::Err<nom::error::Error<&'a [Token]>> {
        nom::Err::Error(nom::error::Error::new(self.remaining(), kind))
    }

    /// Match a specific token type and consume it
    pub fn match_token(
        &mut self,
        expected_type: &TokenType,
    ) -> Result<&Token, nom::Err<nom::error::Error<&'a [Token]>>> {
        match self.tokens.get(self.position) {
            Some(token) if &token.token_type == expected_type => {
                self.position += 1;
                Ok(token)
            }
            _ => Err(self.error(nom::error::ErrorKind::Tag)),
        }
    }

    /// Match any token type from a list and consume it
    pub fn match_any(&mut self, expected: &[TokenType]) -> IResult<&'a [Token], &'a Token> {
        match self.tokens.get(self.position) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use medic_lexer::token::Location;

    fn tok(tt: TokenType) -> Token {
        Token::new(tt, "", Location::default())
    }

    #[test]
    fn peek_and_next_work() {
        let tokens = [tok(TokenType::Let), tok(TokenType::Identifier("x".into()))];
        let mut ts = TokenStream::new(&tokens);
        assert!(matches!(
            ts.peek().map(|t| &t.token_type),
            Some(TokenType::Let)
        ));
        assert!(matches!(
            ts.next().map(|t| &t.token_type),
            Some(TokenType::Let)
        ));
        assert!(matches!(
            ts.peek().map(|t| &t.token_type),
            Some(TokenType::Identifier(_))
        ));
        assert!(!ts.is_empty());
        ts.next();
        assert!(ts.is_empty());
    }

    #[test]
    fn remaining_slices_advance() {
        let tokens = [
            tok(TokenType::Fn),
            tok(TokenType::Identifier("f".into())),
            tok(TokenType::LeftParen),
        ];
        let mut ts = TokenStream::new(&tokens);
        assert_eq!(ts.remaining().len(), 3);
        ts.next();
        assert_eq!(ts.remaining().len(), 2);
    }

    #[test]
    fn match_token_ok_and_err() {
        let tokens = [tok(TokenType::If), tok(TokenType::Else)];
        let mut ts = TokenStream::new(&tokens);
        ts.match_token(&TokenType::If).expect("should match If");
        let err = ts.match_token(&TokenType::If).unwrap_err();
        // Ensure it's a nom::Err::Error
        match err {
            NomErr::Error(_) => {}
            _ => panic!("expected error variant"),
        }
    }

    #[test]
    fn match_any_ok_and_err() {
        let tokens = [tok(TokenType::Return), tok(TokenType::While)];
        let mut ts = TokenStream::new(&tokens);
        let (_rem, first) = ts
            .match_any(&[TokenType::Return, TokenType::Break])
            .expect("match any first");
        assert!(matches!(first.token_type, TokenType::Return));
        // Next token is While which isn't in expected set
        let err = ts
            .match_any(&[TokenType::Break, TokenType::Continue])
            .unwrap_err();
        match err {
            NomErr::Error(_) => {}
            _ => panic!("expected error variant"),
        }
    }

    #[test]
    fn peek_token_checks_without_consuming() {
        let tokens = [tok(TokenType::Match), tok(TokenType::LeftBrace)];
        let mut ts = TokenStream::new(&tokens);
        assert!(ts.peek_token(&TokenType::Match));
        // Still should match after peek
        ts.match_token(&TokenType::Match).unwrap();
        assert!(matches!(
            ts.peek().map(|t| &t.token_type),
            Some(TokenType::LeftBrace)
        ));
    }

    #[test]
    fn empty_stream_behaviour() {
        let mut ts = TokenStream::new(&[]);
        assert!(ts.peek().is_none());
        assert!(ts.next().is_none());
        assert!(ts.is_empty());
        // error() should return a nom::Err::Error on empty
        match ts.error(ErrorKind::Tag) {
            NomErr::Error(_) => {}
            _ => panic!("expected error variant"),
        }
    }

    #[test]
    fn match_any_on_empty_errors() {
        let mut ts = TokenStream::new(&[]);
        let err = ts.match_any(&[TokenType::If, TokenType::Else]).unwrap_err();
        match err {
            NomErr::Error(_) => {}
            _ => panic!("expected error variant"),
        }
    }
}
