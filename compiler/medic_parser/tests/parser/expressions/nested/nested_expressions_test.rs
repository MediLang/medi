use medic_ast::ast::*;
use medic_lexer::token::{Token, TokenType};
use medic_parser::parser::{
    expressions::nested::binary_expressions::parse_nested_binary_expression,
    TokenSlice,
};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_binary_expression() {
    let tokens = vec![
        Token::new(TokenType::Int(1), "1".to_string(), Default::default()),
        Token::new(TokenType::Plus, "+".to_string(), Default::default()),
        Token::new(TokenType::Int(2), "2".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok());
    
    let (remaining, expr) = result.unwrap();
    assert!(remaining.is_empty());
    
    match expr {
        ExpressionNode::Binary(bin) => {
            assert_eq!(bin.operator, BinaryOperator::Add);
            
            match *bin.left {
                ExpressionNode::Literal(LiteralNode::Int(1)) => {}
                _ => panic!("Expected left operand to be integer 1"),
            }
            
            match *bin.right {
                ExpressionNode::Literal(LiteralNode::Int(2)) => {}
                _ => panic!("Expected right operand to be integer 2"),
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_operator_precedence() {
    let tokens = vec![
        Token::new(TokenType::Int(1), "1".to_string(), Default::default()),
        Token::new(TokenType::Plus, "+".to_string(), Default::default()),
        Token::new(TokenType::Int(2), "2".to_string(), Default::default()),
        Token::new(TokenType::Star, "*".to_string(), Default::default()),
        Token::new(TokenType::Int(3), "3".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok());
    
    let (remaining, expr) = result.unwrap();
    assert!(remaining.is_empty());
    
    // The expression should be 1 + (2 * 3) due to operator precedence
    match expr {
        ExpressionNode::Binary(bin) => {
            assert_eq!(bin.operator, BinaryOperator::Add);
            
            // Check left operand (1)
            match *bin.left {
                ExpressionNode::Literal(LiteralNode::Int(1)) => {}
                _ => panic!("Expected left operand to be integer 1"),
            }
            
            // Check right operand (2 * 3)
            match *bin.right {
                ExpressionNode::Binary(mul) => {
                    assert_eq!(mul.operator, BinaryOperator::Mul);
                    
                    match *mul.left {
                        ExpressionNode::Literal(LiteralNode::Int(2)) => {}
                        _ => panic!("Expected left operand of multiplication to be integer 2"),
                    }
                    
                    match *mul.right {
                        ExpressionNode::Literal(LiteralNode::Int(3)) => {}
                        _ => panic!("Expected right operand of multiplication to be integer 3"),
                    }
                }
                _ => panic!("Expected multiplication as right operand"),
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_comparison_operators() {
    let tokens = vec![
        Token::new(TokenType::Int(1), "1".to_string(), Default::default()),
        Token::new(TokenType::Less, "<".to_string(), Default::default()),
        Token::new(TokenType::Int(2), "2".to_string(), Default::default()),
        Token::new(TokenType::LessEqual, "<=".to_string(), Default::default()),
        Token::new(TokenType::Int(3), "3".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok());
    
    // This should parse as (1 < 2) <= 3
    let (_, expr) = result.unwrap();
    match expr {
        ExpressionNode::Binary(_) => {}
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_invalid_comparison_chaining() {
    let tokens = vec![
        Token::new(TokenType::Int(1), "1".to_string(), Default::default()),
        Token::new(TokenType::Less, "<".to_string(), Default::default()),
        Token::new(TokenType::Int(2), "2".to_string(), Default::default()),
        Token::new(TokenType::Less, "<".to_string(), Default::default()),
        Token::new(TokenType::Int(3), "3".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_err());
    
    // Check that we get the expected error
    if let Err(nom::Err::Error(e)) = result {
        // This is a bit of a hack since we can't directly compare the error types
        let error_str = format!("{:?}", e);
        assert!(error_str.contains("unexpected token"));
    } else {
        panic!("Expected a parsing error for chained comparisons");
    }
}

#[test]
fn test_medical_operators() {
    use TokenType::*;
    use ExpressionNode::*;
    use LiteralNode::*;
    use BinaryOperator::*;

    // Test 'of' operator with multiplication
    let tokens = vec![
        Token::new(Int(2), "2".to_string(), Default::default()),
        Token::new(Of, "of".to_string(), Default::default()),
        Token::new(Int(3), "3".to_string(), Default::default()),
        Token::new(Star, "*".to_string(), Default::default()),
        Token::new(Identifier("doses".to_string()), "doses".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok(), "Failed to parse expression: {:?}", result.err());
    
    let (remaining, expr) = result.unwrap();
    assert!(remaining.is_empty(), "Not all tokens were consumed");
    
    // The expression should be (2 of (3 * doses))
    match expr {
        Binary(bin) => {
            assert_eq!(bin.operator, Of, "Expected 'of' operator");
            
            // Check left operand (2)
            match *bin.left {
                Literal(Int(2)) => {},
                _ => panic!("Expected integer 2 on left of 'of' operator"),
            }
            
            // Check right operand (3 * doses)
            match *bin.right {
                Binary(mul) => {
                    assert_eq!(mul.operator, Mul, "Expected multiplication operator");
                    
                    // Check left operand (3)
                    match *mul.left {
                        Literal(Int(3)) => {},
                        _ => panic!("Expected integer 3 on left of multiplication"),
                    }
                    
                    // Check right operand (doses)
                    match *mul.right {
                        Identifier(ident) => assert_eq!(ident.name, "doses", "Expected identifier 'doses' on right of multiplication"),
                        _ => panic!("Expected identifier 'doses' on right of multiplication"),
                    }
                },
                _ => panic!("Expected multiplication on right of 'of' operator"),
            }
        },
        _ => panic!("Expected binary expression with 'of' operator"),
    }

    // Test 'per' operator with multiplication
    let tokens = vec![
        Token::new(Int(5), "5".to_string(), Default::default()),
        Token::new(Star, "*".to_string(), Default::default()),
        Token::new(Identifier("mg".to_string()), "mg".to_string(), Default::default()),
        Token::new(Per, "per".to_string(), Default::default()),
        Token::new(Identifier("day".to_string()), "day".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok(), "Failed to parse expression: {:?}", result.err());
    
    let (remaining, expr) = result.unwrap();
    assert!(remaining.is_empty(), "Not all tokens were consumed");
    
    // The expression should be ((5 * mg) per day)
    match expr {
        Binary(bin) => {
            assert_eq!(bin.operator, Per, "Expected 'per' operator");
            
            // Check left operand (5 * mg)
            match *bin.left {
                Binary(mul) => {
                    assert_eq!(mul.operator, Mul, "Expected multiplication operator");
                    
                    // Check left operand (5)
                    match *mul.left {
                        Literal(Int(5)) => {},
                        _ => panic!("Expected integer 5 on left of multiplication"),
                    }
                    
                    // Check right operand (mg)
                    match *mul.right {
                        Identifier(ident) => assert_eq!(ident.name, "mg", "Expected identifier 'mg' on right of multiplication"),
                        _ => panic!("Expected identifier 'mg' on right of multiplication"),
                    }
                },
                _ => panic!("Expected multiplication on left of 'per' operator"),
            }
            
            // Check right operand (day)
            match *bin.right {
                Identifier(ident) => assert_eq!(ident.name, "day", "Expected identifier 'day' on right of 'per' operator"),
                _ => panic!("Expected identifier 'day' on right of 'per' operator"),
            }
        },
        _ => panic!("Expected binary expression with 'per' operator"),
    }
    
    // Test operator precedence: 'of' has higher precedence than '*' and 'per' has lower
    let tokens = vec![
        Token::new(Int(2), "2".to_string(), Default::default()),
        Token::new(Of, "of".to_string(), Default::default()),
        Token::new(Int(3), "3".to_string(), Default::default()),
        Token::new(Star, "*".to_string(), Default::default()),
        Token::new(Int(4), "4".to_string(), Default::default()),
        Token::new(Per, "per".to_string(), Default::default()),
        Token::new(Int(5), "5".to_string(), Default::default()),
    ];
    
    let result = parse_nested_binary_expression(TokenSlice::new(&tokens), 0, false);
    assert!(result.is_ok(), "Failed to parse expression: {:?}", result.err());
    
    let (remaining, expr) = result.unwrap();
    assert!(remaining.is_empty(), "Not all tokens were consumed");
    
    // The expression should be: ((2 of (3 * 4)) per 5)
    match expr {
        Binary(per_bin) => {
            assert_eq!(per_bin.operator, Per, "Expected 'per' as the outer operator");
            
            // Left side should be (2 of (3 * 4))
            match *per_bin.left {
                Binary(of_bin) => {
                    assert_eq!(of_bin.operator, Of, "Expected 'of' operator");
                    
                    // Left of 'of' should be 2
                    match *of_bin.left {
                        Literal(Int(2)) => {},
                        _ => panic!("Expected integer 2 on left of 'of' operator"),
                    }
                    
                    // Right of 'of' should be (3 * 4)
                    match *of_bin.right {
                        Binary(mul_bin) => {
                            assert_eq!(mul_bin.operator, Mul, "Expected multiplication operator");
                            
                            // Left of '*' should be 3
                            match *mul_bin.left {
                                Literal(Int(3)) => {},
                                _ => panic!("Expected integer 3 on left of multiplication"),
                            }
                            
                            // Right of '*' should be 4
                            match *mul_bin.right {
                                Literal(Int(4)) => {},
                                _ => panic!("Expected integer 4 on right of multiplication"),
                            }
                        },
                        _ => panic!("Expected multiplication on right of 'of' operator"),
                    }
                },
                _ => panic!("Expected 'of' expression on left of 'per' operator"),
            }
            
            // Right of 'per' should be 5
            match *per_bin.right {
                Literal(Int(5)) => {},
                _ => panic!("Expected integer 5 on right of 'per' operator"),
            }
        },
        _ => panic!("Expected binary expression with 'per' operator as the outer operator"),
    }
}
