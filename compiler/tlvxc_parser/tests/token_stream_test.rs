use tlvxc_lexer::token::{Location, Token, TokenType};
use tlvxc_parser::parser::token_stream::TokenStream;
use nom::Err as NomErr;

fn tok(tt: TokenType) -> Token {
    Token::new(tt, "", Location::default())
}

#[test]
fn token_stream_smoke_covers_core_methods() {
    let tokens = [tok(TokenType::Let), tok(TokenType::Identifier("x".into()))];
    let mut ts = TokenStream::new(&tokens);

    assert!(matches!(
        ts.peek().map(|t| &t.token_type),
        Some(TokenType::Let)
    ));
    assert!(ts.peek_token(&TokenType::Let));

    let matched = ts.match_token(&TokenType::Let).expect("should match Let");
    assert!(matches!(matched.token_type, TokenType::Let));

    assert!(!ts.is_empty());
    assert!(matches!(
        ts.next().map(|t| &t.token_type),
        Some(TokenType::Identifier(_))
    ));
    assert!(ts.is_empty());
    assert!(ts.next().is_none());
}

#[test]
fn token_stream_match_any_and_errors() {
    let tokens = [tok(TokenType::If), tok(TokenType::Else)];
    let mut ts = TokenStream::new(&tokens);

    let (_rem, first) = ts
        .match_any(&[TokenType::If, TokenType::While])
        .expect("match_any should accept If");
    assert!(matches!(first.token_type, TokenType::If));

    // Now Else is current; attempt match_token on If should error
    let err = ts.match_token(&TokenType::If).unwrap_err();
    match err {
        NomErr::Error(_) => {}
        _ => panic!("expected nom::Err::Error"),
    }

    // match_any should also error when no match
    let err = ts
        .match_any(&[TokenType::While, TokenType::Return])
        .unwrap_err();
    match err {
        NomErr::Error(_) => {}
        _ => panic!("expected nom::Err::Error"),
    }
}
