use super::super::*;
use crate::parser::TokenSlice;
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
