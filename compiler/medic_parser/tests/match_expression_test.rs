use medic_ast::ast::{
    ExpressionNode, IdentifierNode, LiteralNode, MatchArmNode, PatternNode, Spanned, StatementNode,
};
use medic_lexer::string_interner::InternedString;
use medic_lexer::token::{Location, Token, TokenType};
use medic_parser::parser::{expressions::parse_match_expression, parse_expression, TokenSlice};
use std::sync::Once;

// Initialize the logger only once for all tests
static INIT: Once = Once::new();
fn setup_test_logger() {
    INIT.call_once(|| {
        let _ = env_logger::builder().is_test(true).try_init();
    });
}

#[test]
fn test_match_expression_trailing_comma() {
    setup_test_logger();
    let l = loc(1, 1);
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(10), "10", l),
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        // trailing comma before closing brace
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    assert_eq!(m.arms.len(), 2, "Expected 2 arms even with trailing comma");
                } else {
                    panic!("Expected StatementNode::Match");
                }
            } else {
                panic!("Expected ExpressionNode::Statement wrapping a match");
            }
        }
        Err(e) => panic!("Parse error (trailing comma should be allowed): {e:?}"),
    }
}

#[test]
fn test_match_expression_missing_comma_error() {
    setup_test_logger();
    let l = loc(1, 1);
    // Missing comma between two arms: `1 => 1  _ => 0`
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(1), "1", l),
        // INTENTIONALLY no comma here
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    assert!(
        result.is_err(),
        "Expected parse error due to missing comma between match arms"
    );
}

#[test]
fn test_nested_match_in_arm_body() {
    setup_test_logger();
    let l = loc(1, 1);
    // match x { 1 => y { _ => 0 }, _ => 3 }  // nested match using concise syntax for inner
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        // arm 1 pattern
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        // nested match as body (concise syntax)
        Token::new(TokenType::Identifier(InternedString::from("y")), "y", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
        Token::new(TokenType::Comma, ",", l),
        // arm 2
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(3), "3", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty());
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    assert_eq!(m.arms.len(), 2);
                    // First arm body should be a nested match expression wrapped as statement
                    match &*m.arms[0].body {
                        ExpressionNode::Statement(inner_stmt) => match &*inner_stmt.node {
                            StatementNode::Match(_) => {}
                            other => {
                                panic!("Expected nested match in first arm body, got {other:?}")
                            }
                        },
                        other => panic!("Expected statement expression in arm body, got {other:?}"),
                    }
                } else {
                    panic!("Expected match statement");
                }
            } else {
                panic!("Expected expression wrapping statement");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}

#[test]
fn test_block_body_in_match_arm() {
    setup_test_logger();
    let l = loc(1, 1);
    // match x { 1 => { 42 }, _ => 0 }
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(42), "42", l),
        Token::new(TokenType::RightBrace, "}", l),
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty());
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    assert_eq!(m.arms.len(), 2);
                    // First arm body should parse as a block (wrapped as statement expression)
                    match &*m.arms[0].body {
                        ExpressionNode::Statement(inner_stmt) => match &*inner_stmt.node {
                            StatementNode::Block(_) => {}
                            other => panic!("Expected block in arm body, got {other:?}"),
                        },
                        other => panic!("Expected statement expression in arm body, got {other:?}"),
                    }
                } else {
                    panic!("Expected match statement");
                }
            } else {
                panic!("Expected expression wrapping statement");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}

fn loc(line: u32, column: u32) -> Location {
    Location {
        line: line as usize,
        column: column as usize,
        offset: 0,
    }
}

#[test]
fn test_match_expression_full_syntax_basic() {
    setup_test_logger();
    let l = loc(1, 1);
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty(), "Expected no remaining tokens");
            // Expression is created from a Statement::Match
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    // matched expression should be identifier x
                    match &*m.expr {
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            ..
                        }) if name == "x" => {}
                        other => panic!("Expected identifier 'x', got {other:?}"),
                    }
                    assert_eq!(m.arms.len(), 2);
                } else {
                    panic!("Expected StatementNode::Match");
                }
            } else {
                panic!("Expected ExpressionNode::Statement wrapping a match");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}

#[test]
fn test_match_expression_concise_syntax_basic() {
    setup_test_logger();
    let l = loc(1, 1);
    let tokens = vec![
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];

    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty());
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    // concise uses dummy match token; just assert arms and expr
                    match &*m.expr {
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            ..
                        }) if name == "x" => {}
                        other => panic!("Expected identifier 'x', got {other:?}"),
                    }
                    assert_eq!(m.arms.len(), 2);
                } else {
                    panic!("Expected StatementNode::Match");
                }
            } else {
                panic!("Expected ExpressionNode::Statement wrapping a match");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}

#[test]
fn test_match_expression_with_float_string_bool_patterns() {
    setup_test_logger();
    let l = loc(1, 1);
    let tokens = vec![
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("y")), "y", l),
        Token::new(TokenType::LeftBrace, "{", l),
        // float pattern
        Token::new(TokenType::Float(1.5), "1.5", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::Comma, ",", l),
        // string pattern
        Token::new(TokenType::String("ok".into()), "\"ok\"", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(2), "2", l),
        Token::new(TokenType::Comma, ",", l),
        // boolean pattern
        Token::new(TokenType::Boolean(true), "true", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(3), "3", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];
    let slice = TokenSlice::new(&tokens);
    let result = parse_match_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty());
            if let ExpressionNode::Statement(stmt) = expr {
                if let StatementNode::Match(m) = *stmt.node {
                    assert_eq!(m.arms.len(), 3);
                    // Validate pattern node kinds
                    match &m.arms[0] {
                        MatchArmNode {
                            pattern: PatternNode::Literal(LiteralNode::Float(f)),
                            ..
                        } => assert!((*f - 1.5).abs() < 1e-9),
                        other => panic!("Expected float literal pattern, got {other:?}"),
                    }
                    match &m.arms[1] {
                        MatchArmNode {
                            pattern: PatternNode::Literal(LiteralNode::String(s)),
                            ..
                        } => assert_eq!(s, "ok"),
                        other => panic!("Expected string literal pattern, got {other:?}"),
                    }
                    match &m.arms[2] {
                        MatchArmNode {
                            pattern: PatternNode::Literal(LiteralNode::Bool(b)),
                            ..
                        } => assert!(*b),
                        other => panic!("Expected bool literal pattern, got {other:?}"),
                    }
                } else {
                    panic!("Expected match statement");
                }
            } else {
                panic!("Expected expression wrapping statement");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}

#[test]
fn test_match_expression_in_binary_context() {
    setup_test_logger();
    // a + match x { 1 => 1, _ => 0 }
    let l = loc(1, 1);
    let tokens = vec![
        Token::new(TokenType::Identifier(InternedString::from("a")), "a", l),
        Token::new(TokenType::Plus, "+", l),
        Token::new(TokenType::Match, "match", l),
        Token::new(TokenType::Identifier(InternedString::from("x")), "x", l),
        Token::new(TokenType::LeftBrace, "{", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(1), "1", l),
        Token::new(TokenType::Comma, ",", l),
        Token::new(TokenType::Underscore, "_", l),
        Token::new(TokenType::FatArrow, "=>", l),
        Token::new(TokenType::Integer(0), "0", l),
        Token::new(TokenType::RightBrace, "}", l),
    ];
    let slice = TokenSlice::new(&tokens);
    let result = parse_expression(slice);
    match result {
        Ok((remaining, expr)) => {
            assert!(remaining.is_empty());
            // Expect a Binary expression where right side is a Statement::Match wrapped
            if let ExpressionNode::Binary(bin) = expr {
                // left should be identifier 'a'
                match &bin.node.left {
                    ExpressionNode::Identifier(Spanned {
                        node: IdentifierNode { name },
                        ..
                    }) if name == "a" => {}
                    other => panic!("Expected identifier 'a' on left, got {other:?}"),
                }
                // right should be a match statement wrapped as expression
                match &bin.node.right {
                    ExpressionNode::Statement(stmt) => match &*stmt.node {
                        StatementNode::Match(_) => {}
                        other => panic!("Expected match on right, got {other:?}"),
                    },
                    other => panic!("Expected statement expr on right, got {other:?}"),
                }
            } else {
                panic!("Expected binary expression with match on right");
            }
        }
        Err(e) => panic!("Parse error: {e:?}"),
    }
}
