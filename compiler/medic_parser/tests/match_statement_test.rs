use medic_ast::ast::{
    ExpressionNode, IdentifierNode, LiteralNode, MatchArmNode,
    PatternNode, StatementNode
};

// Import specific TokenType variants we need
use medic_lexer::token::TokenType::{
    Integer, Identifier, LeftBrace, RightBrace, FatArrow, Comma, Underscore, String
};
use medic_lexer::string_interner::InternedString;
use medic_lexer::token::Location;
use medic_lexer::token::Token;
use medic_lexer::token::TokenType::*;
use medic_parser::parser::{parse_match_statement, TokenSlice};
use std::env;

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

fn create_loc(line: u32, column: u32) -> Location {
    Location {
        line: line as usize,
        column: column as usize,
        offset: 0,
    }
}

fn setup_test_logger() {
    let _ = log::set_logger(&LOGGER);
    log::set_max_level(log::LevelFilter::Debug);
}

#[test]
fn test_parse_empty_match_statement() {
    setup_test_logger();
    println!("Starting test_parse_empty_match_statement");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(LeftBrace, "{", loc),
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);

    match &result {
        Ok((remaining, stmt)) => {
            println!("Parse successful!");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            
            if let StatementNode::Match(match_node) = stmt {
                match &*match_node.expr {
                    ExpressionNode::Identifier(IdentifierNode { name }) if name == "x" => {}
                    _ => panic!("Expected identifier 'x', got {:?}", match_node.expr),
                };
                assert!(match_node.arms.is_empty(), "Expected no match arms");
            } else {
                panic!("Expected StatementNode::Match, got {:?}", stmt);
            }
        }
        Err(e) => panic!("Parse error: {}", e),
    }
}

#[test]
fn test_match_with_literal_arms() {
    setup_test_logger();
    println!("Starting test_match_with_literal_arms");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(LeftBrace, "{", loc),
        Token::new(Integer(1), "1", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("one")), "one", loc),
        Token::new(Comma, ",", loc),
        Token::new(Integer(2), "2", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("two")), "two", loc),
        Token::new(Comma, ",", loc),
        Token::new(Underscore, "_", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("other")), "other", loc),
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);

    match result {
        Ok((remaining, stmt)) => {
            println!("Parse successful!");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            
            if let StatementNode::Match(match_node) = stmt {
                // Check the matched expression
                match &*match_node.expr {
                    ExpressionNode::Identifier(IdentifierNode { name }) if name == "x" => {}
                    _ => panic!("Expected identifier 'x', got {:?}", match_node.expr),
                };
                
                // Check the arms
                assert_eq!(match_node.arms.len(), 3, "Expected 3 match arms");
                
                // First arm: 1 => one
                if let Some(MatchArmNode { pattern: PatternNode::Literal(lit), body }) = match_node.arms.first() {
                    match lit {
                        LiteralNode::Int(1) => {}
                        _ => panic!("Expected LiteralNode::Int(1), got {:?}", lit),
                    };
                    match &**body {
                        ExpressionNode::Identifier(IdentifierNode { name }) if name == "one" => {}
                        _ => panic!("Expected identifier 'one', got {:?}", body),
                    };
                } else {
                    panic!("First arm should be a literal pattern");
                }
                
                // Second arm: 2 => two
                if let Some(MatchArmNode { pattern: PatternNode::Literal(lit), body }) = match_node.arms.get(1) {
                    match lit {
                        LiteralNode::Int(2) => {}
                        _ => panic!("Expected LiteralNode::Int(2), got {:?}", lit),
                    };
                    match &**body {
                        ExpressionNode::Identifier(IdentifierNode { name }) if name == "two" => {}
                        _ => panic!("Expected identifier 'two', got {:?}", body),
                    };
                } else {
                    panic!("Second arm should be a literal pattern");
                }
                
                // Third arm: _ => other (wildcard pattern)
                if let Some(MatchArmNode { pattern: PatternNode::Wildcard, body }) = match_node.arms.get(2) {
                    match &**body {
                        ExpressionNode::Identifier(IdentifierNode { name }) if name == "other" => {}
                        _ => panic!("Expected identifier 'other', got {:?}", body),
                    };
                } else {
                    panic!("Third arm should be a wildcard pattern");
                }
            } else {
                panic!("Expected Match statement");
            }
        }
        Err(e) => panic!("Parse error: {}", e),
    }
}

#[test]
fn test_match_with_identifier_patterns() {
    setup_test_logger();
    println!("Starting test_match_with_identifier_patterns");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("result")), "result", loc),
        Token::new(LeftBrace, "{", loc),
        Token::new(Identifier(InternedString::from("Some")), "Some", loc),
        Token::new(LeftParen, "(", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(RightParen, ")", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(Comma, ",", loc),
        Token::new(Identifier(InternedString::from("None")), "None", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("default")), "default", loc),
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);

    match result {
        Ok((remaining, stmt)) => {
            println!("Parse successful!");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            
            if let StatementNode::Match(match_node) = stmt {
                match &*match_node.expr {
                    ExpressionNode::Identifier(IdentifierNode { name }) if name == "result" => {}
                    _ => panic!("Expected identifier 'result', got {:?}", match_node.expr),
                };
                assert_eq!(match_node.arms.len(), 2, "Expected 2 match arms");
                
                // First arm: Some(x) => x
                if let Some(MatchArmNode { pattern: PatternNode::Variant { name, inner }, body }) = match_node.arms.first() {
                    assert_eq!(name, "Some");
                    if let PatternNode::Identifier(IdentifierNode { name: inner_name }) = &**inner {
                        assert_eq!(inner_name, "x");
                    } else {
                        panic!("Expected inner pattern to be an identifier, got {:?}", inner);
                    }
                    match &**body {
                        ExpressionNode::Identifier(IdentifierNode { name }) if name == "x" => {}
                        _ => panic!("Expected identifier 'x', got {:?}", body),
                    };
                } else {
                    panic!("First arm should be a variant pattern");
                }
                
                // Second arm: None => default
                if let Some(arm) = match_node.arms.get(1) {
                    if let PatternNode::Identifier(IdentifierNode { name: pattern_name }) = &arm.pattern {
                        assert_eq!(pattern_name, "None");
                        match &*arm.body {
                            ExpressionNode::Identifier(IdentifierNode { name }) if name == "default" => {}
                            _ => panic!("Expected identifier 'default', got {:?}", arm.body),
                        };
                    } else {
                        panic!("Second arm should be an identifier pattern, got {:?}", arm.pattern);
                    }
                } else {
                    panic!("Expected a second arm");
                }
            } else {
                panic!("Expected Match statement");
            }
        }
        Err(e) => panic!("Parse error: {}", e),
    }
}

#[test]
fn test_match_with_complex_expressions() {
    setup_test_logger();
    println!("Starting test_match_with_complex_expressions");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("result")), "result", loc),
        Token::new(LeftBrace, "{", loc),
        
        // First arm: 0 => "zero"
        Token::new(Integer(0), "0", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(String("zero".into()), "\"zero\"", loc),
        Token::new(Comma, ",", loc),
        
        // Second arm: n => "positive"
        Token::new(Identifier(InternedString::from("n")), "n", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(String("positive".into()), "\"positive\"", loc),
        Token::new(Comma, ",", loc),
        
        // Third arm: _ => "negative"
        Token::new(Underscore, "_", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(String("negative".into()), "\"negative\"", loc),
        
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);

    match result {
        Ok((remaining, stmt)) => {
            println!("Parse successful!");
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            
            if let StatementNode::Match(match_node) = stmt {
                match &*match_node.expr {
                    ExpressionNode::Identifier(IdentifierNode { name }) if name == "result" => {}
                    _ => panic!("Expected identifier 'result', got {:?}", match_node.expr),
                };
                assert_eq!(match_node.arms.len(), 3, "Expected 3 match arms");
                
                // First arm: 0 => "zero"
                if let Some(MatchArmNode { pattern: PatternNode::Literal(lit), body }) = match_node.arms.first() {
                    match lit {
                        LiteralNode::Int(0) => {}
                        _ => panic!("Expected LiteralNode::Int(0), got {:?}", lit),
                    };
                    match &**body {
                        ExpressionNode::Literal(LiteralNode::String(s)) if s == "zero" => {}
                        _ => panic!("Expected string literal 'zero', got {:?}", body),
                    };
                } else {
                    panic!("First arm should be a literal pattern with no guard");
                }
                
                // Second arm: n => "positive"
                if let Some(MatchArmNode { 
                    pattern: PatternNode::Identifier(IdentifierNode { name: pattern_name }), 
                    body 
                }) = match_node.arms.get(1) {
                    assert_eq!(pattern_name, "n");
                    match &**body {
                        ExpressionNode::Literal(LiteralNode::String(s)) if s == "positive" => {}
                        _ => panic!("Expected string literal 'positive', got {:?}", body),
                    };
                } else {
                    panic!("Second arm should be an identifier pattern");
                }
                
                // Third arm: _ => "negative"
                if let Some(MatchArmNode { pattern: PatternNode::Wildcard, body }) = match_node.arms.get(2) {
                    match &**body {
                        ExpressionNode::Literal(LiteralNode::String(s)) if s == "negative" => {}
                        _ => panic!("Expected string literal 'negative', got {:?}", body),
                    };
                } else {
                    panic!("Third arm should be a wildcard pattern with no guard");
                }
            } else {
                panic!("Expected Match statement");
            }
        }
        Err(e) => panic!("Parse error: {}", e),
    }
}

#[test]
fn test_match_with_missing_brace() {
    setup_test_logger();
    println!("Starting test_match_with_missing_brace");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(LeftBrace, "{", loc),
        Token::new(Integer(1), "1", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("one")), "one", loc),
        // Missing closing brace
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);
    
    assert!(result.is_err(), "Expected parse error for missing closing brace");
}

#[test]
fn test_match_with_missing_arrow() {
    setup_test_logger();
    println!("Starting test_match_with_missing_arrow");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(LeftBrace, "{", loc),
        Token::new(Integer(1), "1", loc),
        // Missing fat arrow
        Token::new(Identifier(InternedString::from("one")), "one", loc),
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);
    
    assert!(result.is_err(), "Expected parse error for missing fat arrow");
}

#[test]
fn test_match_with_invalid_pattern() {
    setup_test_logger();
    println!("Starting test_match_with_invalid_pattern");

    let loc = create_loc(1, 1);
    let tokens = vec![
        Token::new(Match, "match", loc),
        Token::new(Identifier(InternedString::from("x")), "x", loc),
        Token::new(LeftBrace, "{", loc),
        // Invalid pattern: binary operator as pattern
        Token::new(Plus, "+", loc),
        Token::new(FatArrow, "=>", loc),
        Token::new(Identifier(InternedString::from("plus")), "plus", loc),
        Token::new(RightBrace, "}", loc),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_statement(slice);
    
    assert!(result.is_err(), "Expected parse error for invalid pattern");
}
