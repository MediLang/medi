use medic_ast::ast::StatementNode;
use medic_lexer::string_interner::InternedString;
use medic_lexer::token::{Location, Token, TokenType};
use medic_parser::parser::{statements::parse_while_statement, TokenSlice};
use std::sync::Once;

// Initialize the logger only once for all tests
static INIT: Once = Once::new();

fn init_logger() {
    INIT.call_once(|| {
        let _ = env_logger::builder().is_test(true).try_init();
    });
}

#[test]
fn test_parse_while_statement() {
    init_logger();

    // Test with non-parenthesized condition
    let loc = Location {
        line: 1,
        column: 1,
        offset: 0,
    };
    let tokens = vec![
        Token::new(TokenType::While, "while", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Less, "<", loc),
        Token::new(TokenType::Integer(10), "10", loc),
        Token::new(TokenType::LeftBrace, "{", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Equal, "=", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Plus, "+", loc),
        Token::new(TokenType::Integer(1), "1", loc),
        Token::new(TokenType::Semicolon, ";", loc),
        Token::new(TokenType::RightBrace, "}", loc),
    ];

    test_while_parsing(tokens, "Non-parenthesized condition");

    // Test with parenthesized condition
    let tokens = vec![
        Token::new(TokenType::While, "while", loc),
        Token::new(TokenType::LeftParen, "(", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Less, "<", loc),
        Token::new(TokenType::Integer(10), "10", loc),
        Token::new(TokenType::RightParen, ")", loc),
        Token::new(TokenType::LeftBrace, "{", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Equal, "=", loc),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", loc),
        Token::new(TokenType::Plus, "+", loc),
        Token::new(TokenType::Integer(1), "1", loc),
        Token::new(TokenType::Semicolon, ";", loc),
        Token::new(TokenType::RightBrace, "}", loc),
    ];

    test_while_parsing(tokens, "Parenthesized condition");
}

fn test_while_parsing(tokens: Vec<Token>, test_name: &str) {
    log::debug!("Testing while statement parsing: {}", test_name);
    log::debug!("Tokens: {:?}", tokens);

    let slice = TokenSlice::new(&tokens);
    let result = parse_while_statement(slice);

    match &result {
        Ok((remaining, stmt)) => {
            log::debug!("Parse successful for {}!", test_name);
            log::debug!("  Remaining tokens: {}", remaining.0.len());
            log::debug!("  Statement: {:?}", stmt);
            assert!(
                remaining.is_empty(),
                "Expected no remaining tokens for {}",
                test_name
            );
            assert!(
                matches!(stmt, StatementNode::While(_)),
                "Expected While statement for {}",
                test_name
            );
        }
        Err(e) => {
            log::error!("Parse failed for {}: {:?}", test_name, e);
            panic!("Parse failed for {}: {:?}", test_name, e);
        }
    }
}
