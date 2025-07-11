use super::super::*;
use crate::parser::TokenSlice;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

/// Converts an input string into a `TokenSlice` and its corresponding vector of tokens.
///
/// This function tokenizes the input string using the lexer, then creates a `TokenSlice`
/// referencing a leaked static slice of the tokens. The original vector of tokens is also returned
/// for further inspection or ownership.
///
/// # Examples
///
/// ```
/// let (slice, tokens) = str_to_token_slice("1 + 2 * 3");
/// assert!(!tokens.is_empty());
/// ```
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
        let (input, _) = str_to_token_slice("1 + 2 * 3");
        let (_, expr) = parse_expression(input).unwrap();

        assert_eq!(
            expr,
            ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(LiteralNode::Int(1)),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left: ExpressionNode::Literal(LiteralNode::Int(2)),
                    operator: BinaryOperator::Mul,
                    right: ExpressionNode::Literal(LiteralNode::Int(3)),
                })),
            }))
        );
    }

    // Add other test functions here...
}

#[cfg(test)]
mod medical_operators_test {
    use super::*;
    use medic_ast::ast::{BinaryOperator, ExpressionNode, LiteralNode};
    use pretty_assertions::assert_eq;

    #[test]
    /// Tests parsing of the "of" operator in medical expressions.
    ///
    /// Verifies that the expression "2 of 3 doses" is parsed with the "of" operator at the root,
    /// the left operand as the integer literal 2, and the right operand as the integer 3,
    /// leaving "doses" unparsed.
    ///
    /// # Examples
    ///
    /// ```
    /// test_of_operator();
    /// // Passes if the AST structure matches:
    /// // (2 of 3) with "doses" remaining unparsed
    /// ```
    fn test_of_operator() {
        // 2 of 3 doses should be parsed as (2 of 3) with "doses" left unparsed
        let (input, _) = str_to_token_slice("2 of 3 doses");
        let (remaining, expr) = parse_expression(input).unwrap();

        // Debug output
        eprintln!("Parsed expression: {:#?}", expr);

        // The 'doses' token should still be in the remaining tokens
        assert!(
            !remaining.is_empty(),
            "Expected 'doses' token to remain unparsed"
        );
        assert_eq!(remaining.0[0].lexeme, "doses");

        match &expr {
            ExpressionNode::Binary(bin) => {
                assert_eq!(bin.operator, BinaryOperator::Of);
                // The left side should be a literal integer 2
                if let ExpressionNode::Literal(lit) = &bin.left {
                    assert_eq!(format!("{:?}", lit), "Int(2)");
                } else {
                    panic!("Expected literal integer on left");
                }
                // The right side should be a literal integer 3
                if let ExpressionNode::Literal(lit) = &bin.right {
                    assert_eq!(format!("{:?}", lit), "Int(3)");
                } else {
                    panic!("Expected literal integer on right");
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    /// Tests that the parser correctly handles the "per" operator in medical expressions.
    ///
    /// Verifies that the expression "5 mg per day" is parsed as a binary "per" operation,
    /// where the left operand is a multiplication of the integer 5 and the identifier "mg",
    /// and the right operand is the identifier "day".
    ///
    /// # Examples
    ///
    /// ```
    /// test_per_operator();
    /// ```
    fn test_per_operator() {
        // 5 mg per day should be parsed as (5 mg) per day
        let (input, _) = str_to_token_slice("5 mg per day");
        let (_, expr) = parse_expression(input).unwrap();

        // Debug output
        eprintln!("Parsed expression: {:#?}", expr);

        match &expr {
            ExpressionNode::Binary(bin) => {
                assert_eq!(bin.operator, BinaryOperator::Per);

                // The left side should be a binary expression with multiplication
                if let ExpressionNode::Binary(left_bin) = &bin.left {
                    assert_eq!(left_bin.operator, BinaryOperator::Mul);

                    // The left side of the multiplication should be a literal integer 5
                    if let ExpressionNode::Literal(lit) = &left_bin.left {
                        assert_eq!(format!("{:?}", lit), "Int(5)");
                    } else {
                        panic!("Expected literal integer 5 on left of multiplication");
                    }

                    // The right side of the multiplication should be an identifier "mg"
                    if let ExpressionNode::Identifier(ident) = &left_bin.right {
                        assert_eq!(ident.name, "mg");
                    } else {
                        panic!("Expected identifier 'mg' on right of multiplication");
                    }
                } else {
                    panic!("Expected binary expression on left");
                }

                // The right side should be an identifier "day"
                if let ExpressionNode::Identifier(ident) = &bin.right {
                    assert_eq!(ident.name, "day");
                } else {
                    panic!("Expected identifier 'day' on right");
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }
}
