use medic_ast::ast::{BinaryOperator, ExpressionNode, LiteralNode, Spanned};
use medic_parser::parser::expressions::parse_expression;

// Helper to unwrap Spanned types for testing
fn unwrap_spanned<T>(spanned: &Spanned<T>) -> &T {
    &spanned.node
}
use medic_parser::parser::test_utils::tokenize;
use medic_parser::parser::TokenSlice;

#[test]
fn test_operator_precedence() {
    // Test that multiplication has higher precedence than addition
    let tokens = tokenize("2 + 3 * 4");
    let input = TokenSlice::new(&tokens);
    let (_, expr) = parse_expression(input).unwrap();

    // The expression should be parsed as 2 + (3 * 4), not (2 + 3) * 4
    match &expr {
        ExpressionNode::Binary(spanned_bin) => {
            let bin_expr = unwrap_spanned(spanned_bin);
            assert_eq!(bin_expr.operator, BinaryOperator::Add);

            // Check left side is a literal 2
            match &bin_expr.left {
                ExpressionNode::Literal(spanned_lit) => {
                    assert_eq!(spanned_lit.node, LiteralNode::Int(2));
                    // Check right side is a binary expression with multiplication
                    match &bin_expr.right {
                        ExpressionNode::Binary(spanned_mul) => {
                            let mul_expr = unwrap_spanned(spanned_mul);
                            assert_eq!(mul_expr.operator, BinaryOperator::Mul);

                            // Check left side of multiplication is a literal 3
                            match &mul_expr.left {
                                ExpressionNode::Literal(LiteralNode::Int(3)) => {
                                    // Check right side of multiplication is a literal 4
                                    match &mul_expr.right {
                                        ExpressionNode::Literal(spanned_four) => {
                                            assert_eq!(spanned_four.node, LiteralNode::Int(4));
                                        }
                                        _ => panic!(
                                            "Expected right side of multiplication to be literal 4"
                                        ),
                                    }
                                }
                                _ => panic!("Expected left side of multiplication to be literal 3"),
                            }
                        }
                        _ => panic!("Expected right side to be a binary expression"),
                    }
                }
                _ => panic!("Expected left side to be literal 2"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }

    // Test left-associativity of addition
    let tokens = tokenize("1 - 2 + 3");
    let input = TokenSlice::new(&tokens);
    let (_, expr) = parse_expression(input).unwrap();

    // The expression should be parsed as (1 - 2) + 3, not 1 - (2 + 3)
    match &expr {
        ExpressionNode::Binary(spanned_bin) => {
            let bin_expr = unwrap_spanned(spanned_bin);
            assert_eq!(bin_expr.operator, BinaryOperator::Add);

            // Check left side is a subtraction expression
            match &bin_expr.left {
                ExpressionNode::Binary(spanned_sub) => {
                    let sub_expr = unwrap_spanned(spanned_sub);
                    assert_eq!(sub_expr.operator, BinaryOperator::Sub);

                    // Check left side of subtraction is a literal 1
                    match &sub_expr.left {
                        ExpressionNode::Literal(spanned_one) => {
                            assert_eq!(spanned_one.node, LiteralNode::Int(1));
                            // Check right side of subtraction is a literal 2
                            match &sub_expr.right {
                                ExpressionNode::Literal(spanned_two) => {
                                    assert_eq!(spanned_two.node, LiteralNode::Int(2));
                                    // Check right side of addition is a literal 3
                                    match &bin_expr.right {
                                        ExpressionNode::Literal(spanned_three) => {
                                            assert_eq!(spanned_three.node, LiteralNode::Int(3));
                                        }
                                        _ => panic!(
                                            "Expected right side of addition to be literal 3"
                                        ),
                                    }
                                }
                                _ => panic!("Expected right side of subtraction to be literal 2"),
                            }
                        }
                        _ => panic!("Expected left side of subtraction to be literal 1"),
                    }
                }
                _ => panic!("Expected left side to be a subtraction expression"),
            }
        }
        _ => panic!("Expected a binary expression"),
    }

    // Test right-associativity of exponentiation (if supported)
    let tokens = tokenize("2 ** 3 ** 2");
    let input = TokenSlice::new(&tokens);
    if let Ok((_, expr)) = parse_expression(input) {
        // The expression should be parsed as 2 ** (3 ** 2), not (2 ** 3) ** 2
        match &expr {
            ExpressionNode::Binary(spanned_pow1) => {
                let pow1_expr = unwrap_spanned(spanned_pow1);
                assert_eq!(pow1_expr.operator, BinaryOperator::Pow);

                // Check left side is a literal 2
                match &pow1_expr.left {
                    ExpressionNode::Literal(spanned_two) => {
                        assert_eq!(spanned_two.node, LiteralNode::Int(2));
                        
                        // Check right side is another exponentiation
                        match &pow1_expr.right {
                            ExpressionNode::Binary(spanned_pow2) => {
                                let pow2_expr = unwrap_spanned(spanned_pow2);
                                assert_eq!(pow2_expr.operator, BinaryOperator::Pow);

                                // Check left side of inner exponentiation is a literal 3
                                match &pow2_expr.left {
                                    ExpressionNode::Literal(spanned_three) => {
                                        assert_eq!(spanned_three.node, LiteralNode::Int(3));
                                        
                                        // Check right side of inner exponentiation is a literal 2
                                        match &pow2_expr.right {
                                            ExpressionNode::Literal(spanned_two) => {
                                                assert_eq!(spanned_two.node, LiteralNode::Int(2));
                                            }
                                            _ => panic!("Expected right side of inner exponentiation to be literal 2"),
                                        }
                                    }
                                    _ => panic!("Expected left side of inner exponentiation to be literal 3"),
                                }
                            }
                            _ => panic!("Expected right side to be a binary expression"),
                        }
                    }
                    _ => panic!("Expected left side to be literal 2"),
                }
            }
            _ => panic!("Expected a binary expression"),
        }
    }
}
