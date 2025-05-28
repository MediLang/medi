use medic_ast::ast::{ExpressionNode, LiteralNode, StatementNode};
use medic_lexer::string_interner::InternedString;
use medic_lexer::token::{Location, Token, TokenType};
use medic_parser::parser::{statements::parse_assignment_statement, TokenSlice};

// Import test utilities
mod test_utils;
use test_utils::*;

// All token creation functions are now in test_utils.rs

#[test]
fn test_valid_identifier_assignment() {
    // Test simple identifier assignment: x = 42;
    let tokens = vec![
        create_identifier_token("x"),
        create_equals_token(),
        create_integer_token(42),
        create_semicolon_token(),
    ];

    let result = parse_assignment_statement(TokenSlice::new(&tokens));
    assert!(
        result.is_ok(),
        "Expected successful parse for valid identifier assignment"
    );

    let (remaining, stmt) = result.unwrap();
    assert!(remaining.is_empty(), "Expected all tokens to be consumed");

    if let StatementNode::Assignment(assign) = stmt {
        // Verify the target is an identifier
        assert!(
            matches!(assign.target, ExpressionNode::Identifier(_)),
            "Expected target to be an identifier"
        );

        // Verify the value is an integer literal
        if let ExpressionNode::Literal(lit) = &assign.value {
            if let LiteralNode::Int(value) = lit {
                assert_eq!(value, &42, "Expected value to be 42");
            } else {
                panic!("Expected integer literal value");
            }
        } else {
            panic!("Expected literal value");
        }
    } else {
        panic!("Expected assignment statement");
    }
}

#[test]
fn test_valid_member_expression_assignment() {
    // Test member expression assignment: obj.prop = 42;
    let tokens = vec![
        create_identifier_token("obj"),
        create_dot_token(),
        create_identifier_token("prop"),
        create_equals_token(),
        create_integer_token(42),
        create_semicolon_token(),
    ];

    let result = parse_assignment_statement(TokenSlice::new(&tokens));
    assert!(
        result.is_ok(),
        "Expected successful parse for valid member expression assignment"
    );

    let (remaining, stmt) = result.unwrap();
    assert!(remaining.is_empty(), "Expected all tokens to be consumed");

    if let StatementNode::Assignment(assign) = stmt {
        // Verify the target is a member expression
        assert!(
            matches!(assign.target, ExpressionNode::Member(_)),
            "Expected target to be a member expression"
        );

        // Verify the value is an integer literal
        if let ExpressionNode::Literal(lit) = &assign.value {
            if let LiteralNode::Int(value) = lit {
                assert_eq!(value, &42, "Expected value to be 42");
            } else {
                panic!("Expected integer literal value");
            }
        } else {
            panic!("Expected literal value");
        }
    } else {
        panic!("Expected assignment statement");
    }
}

#[test]
fn test_invalid_assignment_to_literal() {
    // Test invalid assignment to a literal: 42 = x;
    let tokens = vec![
        create_integer_token(42),
        create_equals_token(),
        create_identifier_token("x"),
        create_semicolon_token(),
    ];

    let result = parse_assignment_statement(TokenSlice::new(&tokens));
    assert!(
        result.is_err(),
        "Expected error when assigning to a literal"
    );
}

#[test]
fn test_invalid_assignment_to_binary_expression() {
    // Test invalid assignment to a binary expression: x + y = 42;
    let tokens = vec![
        create_identifier_token("x"),
        Token::new(
            TokenType::Plus,
            "+",
            Location {
                line: 1,
                column: 1,
                offset: 0,
            },
        ),
        create_identifier_token("y"),
        create_equals_token(),
        create_integer_token(42),
        create_semicolon_token(),
    ];

    let result = parse_assignment_statement(TokenSlice::new(&tokens));
    assert!(
        result.is_err(),
        "Expected error when assigning to a binary expression"
    );
}
