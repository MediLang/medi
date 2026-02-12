use tlvxc_ast::ast::StatementNode;
use tlvxc_lexer::string_interner::InternedString;
use tlvxc_lexer::token::{Location, Token, TokenType};
use tlvxc_parser::parser::{statements::parse_if_statement, TokenSlice};
use std::sync::Once;

// Initialize the logger only once for all tests
static INIT: Once = Once::new();

fn init_logger() {
    INIT.call_once(|| {
        let _ = env_logger::builder().is_test(true).try_init();
    });
}

#[test]
fn test_parse_simple_if_statement() {
    init_logger();

    // Create tokens for: if x {}
    let loc = Location {
        line: 1,
        column: 1,
        offset: 0,
    };
    let tokens = vec![
        Token::new(TokenType::If, "if", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::LeftBrace, "{", loc),
        Token::new(TokenType::RightBrace, "}", loc),
    ];

    log::debug!("Testing parse_if_statement with tokens: {tokens:?}");

    let slice = TokenSlice::new(&tokens);
    let result = parse_if_statement(slice);

    match &result {
        Ok((remaining, stmt)) => {
            log::debug!("Parse successful!");
            log::debug!("  Remaining tokens: {}", remaining.0.len());
            log::debug!("  Statement: {stmt:?}");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            assert!(
                matches!(stmt, StatementNode::If(_)),
                "Expected If statement"
            );
        }
        Err(e) => {
            log::error!("Parse failed: {e:?}");
            panic!("Parse failed: {e:?}");
        }
    }
}

#[test]
fn test_parse_if_else_statement() {
    init_logger();

    // Create tokens for: if x {} else {}
    let loc = Location {
        line: 1,
        column: 1,
        offset: 0,
    };
    let tokens = vec![
        Token::new(TokenType::If, "if", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::LeftBrace, "{", loc),
        Token::new(TokenType::RightBrace, "}", loc),
        Token::new(TokenType::Else, "else", loc),
        Token::new(TokenType::LeftBrace, "{", loc),
        Token::new(TokenType::RightBrace, "}", loc),
    ];

    log::debug!("Testing parse_if_statement with if-else tokens: {tokens:?}",);

    let slice = TokenSlice::new(&tokens);
    let result = parse_if_statement(slice);

    match &result {
        Ok((remaining, stmt)) => {
            log::debug!("Parse successful!");
            log::debug!("  Remaining tokens: {}", remaining.0.len());
            log::debug!("  Statement: {stmt:?}");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            assert!(
                matches!(stmt, StatementNode::If(_)),
                "Expected If statement"
            );
        }
        Err(e) => {
            log::error!("Parse failed: {e:?}");
            panic!("Parse failed: {e:?}");
        }
    }
}
