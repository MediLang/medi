use super::*;
use medic_ast::ast::{ExpressionNode, LiteralNode};
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
mod expressions {
    use super::*;
    use medic_ast::ast::{BinaryExpressionNode, BinaryOperator, ExpressionNode, LiteralNode};

    #[test]
    fn test_operator_precedence() {
        // Test operator precedence: multiplication before addition
        let input = "2 + 3 * 4";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
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
    fn test_range_expression() {
        // Test range expression
        let input = "1..10";
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
