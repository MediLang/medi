use super::*;
use medic_ast::ast::{BlockNode, ExpressionNode, StatementNode};
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
mod statements {
    use super::*;
    use medic_ast::ast::{
        AssignmentNode, ExpressionNode, IdentifierNode, LiteralNode, StatementNode,
    };

    #[test]
    fn test_let_statement() {
        let input = "let x = 42;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_let_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert_eq!(let_stmt.name.name, "x");
                assert!(matches!(
                    let_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(42))
                ));
            }
            _ => panic!("Expected Let statement, got: {:?}", stmt),
        }
    }

    #[test]
    fn test_assignment_statement() {
        let input = "x = 42;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_assignment_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Assignment(assignment) => {
                assert!(matches!(assignment.target, ExpressionNode::Identifier(_)));
                assert!(matches!(
                    assignment.value,
                    ExpressionNode::Literal(LiteralNode::Int(42))
                ));
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_member_access_assignment() {
        let input = "patient.name = \"John\";";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_assignment_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Assignment(assignment) => {
                match &assignment.target {
                    ExpressionNode::Member(member) => {
                        assert_eq!(member.property.name, "name");
                        match &member.object {
                            ExpressionNode::Identifier(id) => {
                                assert_eq!(id.name, "patient");
                            }
                            _ => panic!("Expected identifier"),
                        }
                    }
                    _ => panic!("Expected member expression"),
                }
                match assignment.value {
                    ExpressionNode::Literal(LiteralNode::String(ref s)) => {
                        assert_eq!(s, "John");
                    }
                    _ => panic!("Expected string literal"),
                };
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_invalid_assignment_target() {
        // Test invalid l-value (should fail)
        let input = "42 = x;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        assert!(parse_assignment_statement(token_slice).is_err());
    }
}
