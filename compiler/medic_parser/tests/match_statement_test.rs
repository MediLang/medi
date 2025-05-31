use std::env;
use medic_lexer::token::{Token, TokenType};
use medic_lexer::token::TokenType::*;
use medic_lexer::token::Location;
use medic_lexer::string_interner::InternedString;
use medic_parser::parser::{TokenSlice, parse_match_statement};
use medic_ast::ast::StatementNode;

// Simple logger for tests
struct TestLogger;

impl log::Log for TestLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        if env::var("RUST_LOG").is_ok() {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

static LOGGER: TestLogger = TestLogger;

#[test]
fn test_parse_empty_match_statement() {
    // Initialize our test logger
    log::set_logger(&LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Debug);
    
    println!("Starting test_parse_empty_match_statement");
    
    let loc = Location { line: 1, column: 1, offset: 0 };
    let tokens = vec![
        Token::new(Match, "match", loc.clone()),
        Token::new(Identifier(InternedString::from("x")), "x", loc.clone()),
        Token::new(LeftBrace, "{", loc.clone()),
        Token::new(RightBrace, "}", loc.clone()),
    ];
    
    // Print the token stream for debugging
    println!("Token stream:");
    for (i, token) in tokens.iter().enumerate() {
        println!("  {}: {:?}", i, token.token_type);
    }
    
    let slice = TokenSlice::new(&tokens);
    println!("Testing parse_match_statement...");
    let result = parse_match_statement(slice);
    
    match &result {
        Ok((remaining, stmt)) => {
            println!("Parse successful!");
            println!("  Remaining tokens: {}", remaining.0.len());
            println!("  Statement: {:?}", stmt);
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            assert!(matches!(stmt, StatementNode::Match(_)), "Expected Match statement");
        },
        Err(e) => {
            println!("Parse failed: {:?}", e);
            panic!("Parse failed: {:?}", e);
        }
    }
}
