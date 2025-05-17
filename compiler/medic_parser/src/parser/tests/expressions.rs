use super::*;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

// Helper function to convert a string to a TokenSlice
fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let tokens_static = Box::new(tokens.clone());
    let tokens_ref = Box::leak(tokens_static);
    (TokenSlice(tokens_ref), tokens)
}

#[cfg(test)]
mod expressions_test {
    use super::*;
    use medic_ast::ast::{BinaryOperator, ExpressionNode, LiteralNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_operator_precedence() {
        // Test operator precedence: multiplication before addition
        let input = "2 + 3 * 4";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        
        // Expected: 2 + (3 * 4)
        match expr {
            ExpressionNode::Binary(binary) => {
                assert_eq!(binary.operator, BinaryOperator::Add);
                assert!(matches!(
                    binary.left,
                    ExpressionNode::Literal(LiteralNode::Int(2))
                ));
                match binary.right {
                    ExpressionNode::Binary(inner_binary) => {
                        assert_eq!(inner_binary.operator, BinaryOperator::Mul);
                        assert!(matches!(
                            inner_binary.left,
                            ExpressionNode::Literal(LiteralNode::Int(3))
                        ));
                        assert!(matches!(
                            inner_binary.right,
                            ExpressionNode::Literal(LiteralNode::Int(4))
                        ));
                    }
                    _ => panic!("Expected nested binary expression"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }
    
    #[test]
    fn test_operator_precedence_all_operators() {
        // Test all operators in order of increasing precedence
        // This expression is designed to test all precedence levels
        let input = "a || b && c ?? d ?: e == f != g < h <= i > j >= k | l ^ m & n << o >> p + q - r * s / t % u ** v .. w";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        
        // The expression should be parsed according to operator precedence
        // We'll walk the AST to verify the structure
        
        // Top level should be logical OR (lowest precedence)
        if let ExpressionNode::Binary(bin) = &expr {
            assert_eq!(bin.operator, BinaryOperator::Or);
            
            // Left side should be 'a'
            assert!(matches!(
                bin.left,
                ExpressionNode::Identifier(ref id) if id.name == "a"
            ));
            
            // Right side should be the rest of the expression
            if let ExpressionNode::Binary(bin) = &bin.right {
                // Next should be logical AND
                assert_eq!(bin.operator, BinaryOperator::And);
                
                // And so on... The full test would continue walking the AST
                // to verify each level of precedence
            } else {
                panic!("Expected logical AND expression");
            }
        } else {
            panic!("Expected logical OR expression");
        }
    }
    
    #[test]
    fn test_right_associative_operators() {
        // Test right-associative operators: ** (exponentiation), ?: (elvis), ?? (null-coalesce)
        
        // Exponentiation is right-associative: 2 ** 3 ** 4 = 2 ** (3 ** 4)
        let input = "2 ** 3 ** 4";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        
        // Expected: 2 ** (3 ** 4)
        if let ExpressionNode::Binary(bin) = &expr {
            assert_eq!(bin.operator, BinaryOperator::Pow);
            
            // Left side should be 2
            assert!(matches!(
                bin.left,
                ExpressionNode::Literal(LiteralNode::Int(2))
            ));
            
            // Right side should be another exponentiation
            if let ExpressionNode::Binary(inner_bin) = &bin.right {
                assert_eq!(inner_bin.operator, BinaryOperator::Pow);
                assert!(matches!(
                    inner_bin.left,
                    ExpressionNode::Literal(LiteralNode::Int(3))
                ));
                assert!(matches!(
                    inner_bin.right,
                    ExpressionNode::Literal(LiteralNode::Int(4))
                ));
            } else {
                panic!("Expected nested exponentiation");
            }
        } else {
            panic!("Expected exponentiation expression");
        }
    }
    
    #[test]
    fn test_mixed_precedence() {
        // Test mixed operators with different precedence and associativity
        let input = "a + b * c ** d ** e << f | g & h";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        
        // The structure should be: ((a + (b * (c ** (d ** e)))) << f) | (g & h)
        // We'll just verify the top level for this test
        if let ExpressionNode::Binary(bin) = &expr {
            assert_eq!(bin.operator, BinaryOperator::BitOr);
        } else {
            panic!("Expected bitwise OR expression");
        }
    }
    
    #[test]
    fn test_comparison_chain() {
        // Test that comparison operators don't chain (a < b < c is not allowed)
        let input = "a < b < c";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let result = parse_expression(token_slice);
        
        // This should fail because comparison operators don't chain in most languages
        assert!(result.is_err(), "Comparison chaining should not be allowed");
    }

    #[test]
    fn test_range_expression() {
        // Test range expression with spaces
        let input = "1 .. 10";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        match expr {
            ExpressionNode::Binary(binary) => {
                assert_eq!(binary.operator, BinaryOperator::Range);
                assert!(matches!(
                    binary.left,
                    ExpressionNode::Literal(LiteralNode::Int(1))
                ));
                assert!(matches!(
                    binary.right,
                    ExpressionNode::Literal(LiteralNode::Int(10))
                ));
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_nested_binary_expression() {
        // Test nested binary expression with parentheses
        let input = "(2 + 3) * 4";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        match expr {
            ExpressionNode::Binary(binary) => {
                assert_eq!(binary.operator, BinaryOperator::Mul);
                assert!(matches!(
                    binary.right,
                    ExpressionNode::Literal(LiteralNode::Int(4))
                ));
                match binary.left {
                    ExpressionNode::Binary(inner_binary) => {
                        assert_eq!(inner_binary.operator, BinaryOperator::Add);
                        assert!(matches!(
                            inner_binary.left,
                            ExpressionNode::Literal(LiteralNode::Int(2))
                        ));
                        assert!(matches!(
                            inner_binary.right,
                            ExpressionNode::Literal(LiteralNode::Int(3))
                        ));
                    }
                    _ => panic!("Expected nested binary expression"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_medical_code_assignment() {
        // Test assignment with medical code - should fail
        let input = "ICD10:A12.34 = true;";
        let (token_slice, _tokens) = str_to_token_slice(input);

        // This will print the error message to stderr
        let result = parse_assignment_statement(token_slice);

        // Check that the result is an error
        assert!(
            result.is_err(),
            "Expected an error for medical code assignment"
        );

        // The test passes if we get here, as the error is already printed to stderr
        // and we can't easily capture it in the test output
    }
}
