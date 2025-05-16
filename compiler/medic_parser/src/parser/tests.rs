use super::*;

#[cfg(test)]
mod parser_tests {
    use super::*;
    use medic_ast::ast::{BlockNode, ExpressionNode, LiteralNode, StatementNode};
    use medic_lexer::lexer::Lexer;
    use medic_lexer::token::Token;
    use std::f64::consts::PI;

    // Helper function to convert a string to a TokenSlice
    fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
        let tokens: Vec<Token> = Lexer::new(input).collect();
        let tokens_static = Box::new(tokens.clone());
        let tokens_ref = Box::leak(tokens_static);
        (TokenSlice(tokens_ref), tokens)
    }

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
            StatementNode::Assignment(assign_stmt) => {
                // Check that the target is a member expression
                match &assign_stmt.target {
                    ExpressionNode::Member(member) => {
                        // Check that the object is an identifier "patient"
                        assert!(matches!(&member.object, 
                            ExpressionNode::Identifier(id) if id.name == "patient"));
                        // Check that the property is "name"
                        assert_eq!(member.property.name(), "name");
                    }
                    _ => panic!("Expected Member expression as target"),
                }

                // Check that the value is the string "John" (with quotes included)
                assert!(matches!(
                    assign_stmt.value,
                    ExpressionNode::Literal(LiteralNode::String(s)) if s == "\"John\""
                ));
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_block_statement() {
        let input = "{
        let x = 1;
        x = 2;
            44
        }";
        let (token_slice, _tokens) = str_to_token_slice(input);

        // Print out the tokens
        println!("Tokens: {:?}", token_slice.0);

        let (_, block) = parse_block(token_slice).unwrap();
        let BlockNode { statements } = block;
        assert_eq!(statements.len(), 3);
        assert!(matches!(statements[0], StatementNode::Let(_)));
        assert!(matches!(statements[1], StatementNode::Assignment(_)));
        assert!(matches!(
            statements[2],
            StatementNode::Expr(ExpressionNode::Literal(LiteralNode::Int(44)))
        ));
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if x == 42 {
            let y = 1;
        } else if x == 43 {
            let y = 2;
        } else {
            let y = 3;
        }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_if_statement(token_slice).unwrap();
        match stmt {
            StatementNode::If(if_node) => {
                // Check condition
                assert!(matches!(if_node.condition, ExpressionNode::Binary(_)));

                // Check then branch
                let BlockNode { statements } = if_node.then_branch;
                assert_eq!(statements.len(), 1);
                assert!(matches!(statements[0], StatementNode::Let(_)));

                // Check else branch
                let BlockNode {
                    statements: else_statements,
                } = if_node.else_branch.unwrap();
                assert_eq!(else_statements.len(), 1);

                // For an 'else if' chain, the else branch contains a nested if statement
                match &else_statements[0] {
                    StatementNode::If(nested_if) => {
                        // Check the nested if's condition
                        assert!(matches!(nested_if.condition, ExpressionNode::Binary(_)));

                        // Check the nested if's then branch
                        let BlockNode {
                            statements: nested_then_statements,
                        } = &nested_if.then_branch;
                        assert_eq!(nested_then_statements.len(), 1);
                        assert!(matches!(nested_then_statements[0], StatementNode::Let(_)));

                        // Check the nested if's else branch
                        let BlockNode {
                            statements: nested_else_statements,
                        } = nested_if.else_branch.as_ref().unwrap();
                        assert_eq!(nested_else_statements.len(), 1);
                        assert!(matches!(nested_else_statements[0], StatementNode::Let(_)));
                    }
                    _ => panic!("Expected nested If statement in else branch"),
                }
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    #[ignore] // Temporarily ignore this test until while statement parsing is implemented
    fn test_while_statement() {
        let input = "while x < 42 {
            x = x + 1;
            let y = x * 2;
        }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_while_statement(token_slice).unwrap();
        match stmt {
            StatementNode::While(while_stmt) => {
                // Check condition
                assert!(matches!(while_stmt.condition, ExpressionNode::Binary(_)));

                // Check body
                let BlockNode { statements } = while_stmt.body;
                assert_eq!(statements.len(), 2);
                assert!(matches!(statements[0], StatementNode::Assignment(_)));
                assert!(matches!(statements[1], StatementNode::Let(_)));
            }
            _ => panic!("Expected While statement"),
        }
    }

    #[test]
    #[ignore] // Temporarily ignore this test until for statement parsing is implemented
    fn test_for_statement() {
        let input = "for x in 1..10 {
            let y = x * 2;
        }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_for_statement(token_slice).unwrap();
        match stmt {
            StatementNode::For(for_node) => {
                assert_eq!(for_node.var.name, "x");
            }
            _ => panic!("Expected For statement"),
        }
    }

    #[test]
    #[ignore] // Temporarily ignore this test until match statement parsing is implemented
    fn test_match_statement() {
        let input = "match x {
            42 => true,
            _ => false
        }";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_match_statement(token_slice).unwrap();
        assert!(matches!(stmt, StatementNode::Match(_)));
    }

    #[test]
    fn test_return_statement() {
        let input = "return 42;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_return_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Return(return_stmt) => match return_stmt.value {
                Some(expr) => match *expr {
                    ExpressionNode::Literal(LiteralNode::Int(42)) => (),
                    _ => panic!("Expected literal 42"),
                },
                None => panic!("Expected Some value"),
            },
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    fn test_return_unit_statement() {
        let input = "return;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_return_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Return(return_stmt) => {
                assert!(return_stmt.value.is_none());
            }
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    fn test_literal() {
        // Test integer literal
        let input = "42";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(lit, LiteralNode::Int(42)));

        // Test float literal
        let input = "3.141592653589793";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(lit, LiteralNode::Float(PI)));

        // Test string literal
        let input = "\"hello\"";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(lit, LiteralNode::String(_)));
        if let LiteralNode::String(s) = lit {
            assert_eq!(s, "\"hello\"");
        }

        // Test boolean literal
        let input = "true";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(lit, LiteralNode::Bool(true)));
    }

    #[test]
    fn test_expression() {
        // Test simple expression
        let input = "42";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::Literal(_)));

        // Test binary expression
        let input = "1 + 2";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::Binary(_)));

        // Test member expression
        let input = "patient.name";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::Member(_)));
    }

    #[test]
    #[ignore] // Temporarily ignore this test until medical code parsing is implemented
    fn test_medical_codes() {
        // Test ICD code
        let input = "ICD10:A01.1";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::IcdCode(code) if code == "ICD10:A01.1"));

        // Test CPT code
        let input = "CPT:12345";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::CptCode(code) if code == "CPT:12345"));

        // Test SNOMED code
        let input = "SNOMED:123456";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::SnomedCode(code) if code == "SNOMED:123456"));
    }
}
