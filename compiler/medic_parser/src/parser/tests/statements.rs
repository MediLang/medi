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
mod statements_test {
    use super::*;
    use medic_ast::ast::{ExpressionNode, LiteralNode, StatementNode};

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

#[cfg(test)]
mod functions_test {
    use super::*;
    use medic_ast::ast::{StatementNode, ExpressionNode};

    #[test]
    fn test_fn_empty_params() {
        let input = "fn foo() { return 1; }";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_function_declaration(ts).expect("should parse fn decl");
        match stmt {
            StatementNode::Function(func) => {
                assert_eq!(func.name.name, "foo");
                assert!(func.params.is_empty());
                assert!(func.return_type.is_none());
                // Body should contain a return
                assert!(!func.body.statements.is_empty());
            }
            _ => panic!("expected function node"),
        }
    }

    #[test]
    fn test_fn_typed_params() {
        let input = "fn calc(x: int, y: float) { }";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_function_declaration(ts).expect("should parse typed params");
        match stmt {
            StatementNode::Function(func) => {
                assert_eq!(func.name.name, "calc");
                assert_eq!(func.params.len(), 2);
                assert_eq!(func.params[0].name.name, "x");
                assert!(func.params[0].type_annotation.is_some());
                assert_eq!(func.params[1].name.name, "y");
                assert!(func.params[1].type_annotation.is_some());
            }
            _ => panic!("expected function node"),
        }
    }

    #[test]
    fn test_fn_with_return_type() {
        let input = "fn answer() -> int { return 42; }";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_function_declaration(ts).expect("should parse return type");
        match stmt {
            StatementNode::Function(func) => {
                assert!(func.return_type.is_some());
            }
            _ => panic!("expected function node"),
        }
    }

    #[test]
    fn test_fn_nested_body_and_missing_semis_tolerated() {
        // let without semicolon is tolerated by parser; return without semicolon at end of block is also allowed
        let input = r#"
            fn flow(a: int) {
                let x = a + 1
                if a { return a }
            }
        "#;
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_function_declaration(ts).expect("should parse nested body");
        match stmt {
            StatementNode::Function(func) => {
                assert_eq!(func.name.name, "flow");
                assert_eq!(func.params.len(), 1);
                assert!(func.return_type.is_none());
                assert!(func.body.statements.len() >= 2);
            }
            _ => panic!("expected function node"),
        }
    }

    #[test]
    fn test_fn_rejects_default_param() {
        // Defaults are not supported by current grammar
        let input = "fn f(x = 1) {}";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_function_declaration(ts).is_err());
    }

    #[test]
    fn test_fn_rejects_variadic_like_ellipsis() {
        // No variadic/ellipsis supported
        let input = "fn f(x, ...) {}";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_function_declaration(ts).is_err());
    }
}
