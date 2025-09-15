use super::super::*;
use crate::parser::TokenSlice;
use medic_ast::ast::Spanned;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

/// Converts an input string into a `TokenSlice` and its corresponding vector of tokens.
fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let tokens_static = Box::new(tokens.clone());
    let tokens_ref = Box::leak(tokens_static);
    (TokenSlice(tokens_ref), tokens)
}

#[cfg(test)]
mod array_literals_test {
    use super::*;
    use medic_ast::ast::{ExpressionNode, LiteralNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_simple_array_literal() {
        let (input, _tokens) = str_to_token_slice("[1, 2, 3]");
        let (remaining, expr) = parse_expression(input).unwrap();
        // Fully consumed
        assert!(remaining.is_empty());

        match expr {
            ExpressionNode::Array(Spanned { node: arr, .. }) => {
                assert_eq!(arr.elements.len(), 3);
                // Verify element kinds
                for (idx, el) in arr.elements.iter().enumerate() {
                    match el {
                        ExpressionNode::Literal(Spanned {
                            node: LiteralNode::Int(n),
                            ..
                        }) => {
                            assert_eq!(*n as usize, idx + 1);
                        }
                        other => panic!("Expected int literal, got {other:?}"),
                    }
                }
            }
            other => panic!("Expected array literal, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_nested_array_in_struct_literal() {
        let (input, _tokens) = str_to_token_slice("Point { coords: [1, 2, [3, 4]] }");
        let (remaining, expr) = parse_expression(input).unwrap();
        // Fully consumed
        assert!(remaining.is_empty());

        match expr {
            ExpressionNode::Struct(Spanned { node: s, .. }) => {
                assert_eq!(s.type_name, "Point");
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name, "coords");
                match &s.fields[0].value {
                    ExpressionNode::Array(Spanned { node: arr, .. }) => {
                        assert_eq!(arr.elements.len(), 3);
                        // Third element is a nested array
                        match &arr.elements[2] {
                            ExpressionNode::Array(Spanned { node: inner, .. }) => {
                                assert_eq!(inner.elements.len(), 2);
                                match &inner.elements[0] {
                                    ExpressionNode::Literal(Spanned {
                                        node: LiteralNode::Int(3),
                                        ..
                                    }) => {}
                                    other => panic!("Expected 3, got {other:?}"),
                                }
                                match &inner.elements[1] {
                                    ExpressionNode::Literal(Spanned {
                                        node: LiteralNode::Int(4),
                                        ..
                                    }) => {}
                                    other => panic!("Expected 4, got {other:?}"),
                                }
                            }
                            other => panic!("Expected nested array, got {other:?}"),
                        }
                    }
                    other => panic!("Expected array for coords, got {other:?}"),
                }
            }
            other => panic!("Expected struct literal, got {other:?}"),
        }
    }
}

#[cfg(test)]
mod expressions_test {
    use super::*;
    use medic_ast::ast::{BinaryExpressionNode, BinaryOperator, ExpressionNode, LiteralNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_operator_precedence() {
        // Test operator precedence: multiplication before addition
        let (input, _) = str_to_token_slice("1 + 2 * 3");
        let (_, expr) = parse_expression(input).unwrap();

        match &expr {
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                match &bin.left {
                    ExpressionNode::Literal(Spanned {
                        node: LiteralNode::Int(1),
                        ..
                    }) => {
                        // Left operand is 1
                    }
                    _ => panic!("Expected literal 1 on left, got {:?}", bin.left),
                }

                assert_eq!(bin.operator, BinaryOperator::Add);

                match &bin.right {
                    ExpressionNode::Binary(Spanned {
                        node: inner_bin, ..
                    }) => {
                        match &inner_bin.left {
                            ExpressionNode::Literal(Spanned {
                                node: LiteralNode::Int(2),
                                ..
                            }) => {
                                // Inner left operand is 2
                            }
                            _ => {
                                panic!("Expected literal 2 on inner left, got {:?}", inner_bin.left)
                            }
                        }

                        assert_eq!(inner_bin.operator, BinaryOperator::Mul);

                        match &inner_bin.right {
                            ExpressionNode::Literal(Spanned {
                                node: LiteralNode::Int(3),
                                ..
                            }) => {
                                // Inner right operand is 3
                            }
                            _ => panic!(
                                "Expected literal 3 on inner right, got {:?}",
                                inner_bin.right
                            ),
                        }
                    }
                    _ => panic!("Expected binary expression on right, got {:?}", bin.right),
                }
            }
            _ => panic!("Expected binary expression, got {expr:?}"),
        }
    }
}

#[cfg(test)]
mod medical_operators_test {
    use super::*;
    use medic_ast::ast::{BinaryOperator, ExpressionNode, LiteralNode};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_of_operator() {
        // 2 of 3 doses should be parsed as (2 of 3) with "doses" left unparsed
        let (input, _) = str_to_token_slice("2 of 3 doses");
        let (remaining, expr) = parse_expression(input).unwrap();

        // The 'doses' token should still be in the remaining tokens
        assert!(
            !remaining.is_empty(),
            "Expected 'doses' token to remain unparsed"
        );
        assert_eq!(remaining.0[0].lexeme, "doses");

        match &expr {
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                assert_eq!(bin.operator, BinaryOperator::Of);

                // The left side should be a literal integer 2
                match &bin.left {
                    ExpressionNode::Literal(Spanned {
                        node: LiteralNode::Int(2),
                        ..
                    }) => {
                        // Correct
                    }
                    _ => panic!("Expected literal integer 2 on left, got {:?}", bin.left),
                }

                // The right side should be a literal integer 3
                match &bin.right {
                    ExpressionNode::Literal(Spanned {
                        node: LiteralNode::Int(3),
                        ..
                    }) => {
                        // Correct
                    }
                    _ => panic!("Expected literal integer 3 on right, got {:?}", bin.right),
                }
            }
            _ => panic!("Expected binary expression, got {expr:?}"),
        }
    }

    #[test]
    fn test_per_operator() {
        // 5 mg per day should be parsed as (5 mg) per day
        let (input, _) = str_to_token_slice("5 mg per day");
        let (_, expr) = parse_expression(input).unwrap();

        match &expr {
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                assert_eq!(bin.operator, BinaryOperator::Per);

                // The left side should be a quantity literal (5 mg)
                match &bin.left {
                    ExpressionNode::Quantity(Spanned { node: qty, .. }) => {
                        // Check the value is 5
                        match &qty.value {
                            LiteralNode::Int(5) => {
                                // Correct
                            }
                            _ => panic!(
                                "Expected literal integer 5 in quantity, got {:?}",
                                qty.value
                            ),
                        }

                        // Check the unit is "mg"
                        assert_eq!(qty.unit.name, "mg");
                    }
                    _ => panic!("Expected quantity literal on left, got {:?}", bin.left),
                }

                // The right side should be an identifier "day"
                match &bin.right {
                    ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                        assert_eq!(ident.name, "day");
                    }
                    _ => panic!("Expected identifier 'day' on right, got {:?}", bin.right),
                }
            }
            _ => panic!("Expected binary expression, got {expr:?}"),
        }
    }
}
