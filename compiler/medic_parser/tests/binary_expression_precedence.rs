use medic_ast::ast::{BinaryOperator, ExpressionNode, LiteralNode};
use medic_parser::parser::expressions::parse_expression;
use medic_parser::parser::test_utils::tokenize;
use medic_parser::parser::TokenSlice;

#[test]
fn test_operator_precedence() {
    // Test that multiplication has higher precedence than addition
    let tokens = tokenize("2 + 3 * 4");
    let input = TokenSlice::new(&tokens);
    let (_, expr) = parse_expression(input).unwrap();

    // The expression should be parsed as 2 + (3 * 4), not (2 + 3) * 4
    match expr {
        ExpressionNode::Binary(bin_expr) => {
            assert_eq!(bin_expr.operator, BinaryOperator::Add);

            // Check left side is a literal 2
            match &bin_expr.left {
                ExpressionNode::Literal(LiteralNode::Int(2)) => {
                    // Check right side is a binary expression with multiplication
                    match &bin_expr.right {
                        ExpressionNode::Binary(mul_expr) => {
                            assert_eq!(mul_expr.operator, BinaryOperator::Mul);

                            // Check left side of multiplication is a literal 3
                            match &mul_expr.left {
                                ExpressionNode::Literal(LiteralNode::Int(3)) => {
                                    // Check right side of multiplication is a literal 4
                                    match &mul_expr.right {
                                        ExpressionNode::Literal(LiteralNode::Int(4)) => { /* All checks passed */
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
    match expr {
        ExpressionNode::Binary(bin_expr) => {
            assert_eq!(bin_expr.operator, BinaryOperator::Add);

            // Check left side is a subtraction expression
            match &bin_expr.left {
                ExpressionNode::Binary(sub_expr) => {
                    assert_eq!(sub_expr.operator, BinaryOperator::Sub);

                    // Check left side of subtraction is a literal 1
                    match &sub_expr.left {
                        ExpressionNode::Literal(LiteralNode::Int(1)) => {
                            // Check right side of subtraction is a literal 2
                            match &sub_expr.right {
                                ExpressionNode::Literal(LiteralNode::Int(2)) => {
                                    // Check right side of addition is a literal 3
                                    match &bin_expr.right {
                                        ExpressionNode::Literal(LiteralNode::Int(3)) => { /* All checks passed */
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
        match expr {
            ExpressionNode::Binary(bin_expr) => {
                assert_eq!(bin_expr.operator, BinaryOperator::Pow);

                // Check left side is a literal 2
                match &bin_expr.left {
                    ExpressionNode::Literal(LiteralNode::Int(2)) => {
                        // Check right side is another exponentiation
                        match &bin_expr.right {
                            ExpressionNode::Binary(inner_expr) => {
                                assert_eq!(inner_expr.operator, BinaryOperator::Pow);

                                // Check left side of inner exponentiation is a literal 3
                                match &inner_expr.left {
                                    ExpressionNode::Literal(LiteralNode::Int(3)) => {
                                        // Check right side of inner exponentiation is a literal 2
                                        match &inner_expr.right {
                                            ExpressionNode::Literal(LiteralNode::Int(2)) => { /* All checks passed */ }
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
