use super::*;
use tlvxc_lexer::lexer::Lexer;
use tlvxc_lexer::token::Token;

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
    use tlvxc_ast::ast::{ExpressionNode, LiteralNode, StatementNode};

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
    fn test_let_with_annotation_and_initializer() {
        let input = "let x: Int = 1;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_let_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert!(let_stmt.type_annotation.is_some());
                assert!(matches!(
                    let_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(1))
                ));
            }
            _ => panic!("Expected Let statement, got: {:?}", stmt),
        }
    }

    #[test]
    fn test_let_with_annotation_no_initializer() {
        let input = "let x: Int;";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let (_, stmt) = parse_let_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert!(let_stmt.type_annotation.is_some());
                assert!(let_stmt.value.is_none());
            }
            _ => panic!("Expected Let statement, got: {:?}", stmt),
        }
    }

    #[test]
    fn test_let_with_annotation_and_initializer_missing_semicolon() {
        // Parser intentionally tolerates missing semicolon after let
        let input = "let x: Int = 1";
        let (token_slice, _tokens) = str_to_token_slice(input);
        let res = parse_let_statement(token_slice);
        assert!(res.is_ok());
    }

    // Negative and edge-case tests for malformed annotations
    #[test]
    fn test_let_annotation_missing_type_after_colon() {
        let input = "let x: = 1;";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    #[test]
    fn test_let_annotation_missing_name_before_colon() {
        let input = "let : Int = 1;";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    #[test]
    fn test_let_annotation_double_colon() {
        let input = "let x:: Int = 1;";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    #[test]
    fn test_let_annotation_missing_colon_between_name_and_type() {
        let input = "let x Int = 1;";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    #[test]
    fn test_let_annotation_extra_tokens_in_annotation() {
        // Extra identifier after a valid type annotation should cause parse error
        let input = "let x: Int Int = 1;";
        let (ts, _tokens) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    // Additional coverage per request
    #[test]
    fn test_let_annotation_unknown_type_identifier_parses_ok() {
        // Parser only checks syntax; unknown type names are for the type checker
        let input = "let x: Foo = 1;";
        let (ts, _toks) = str_to_token_slice(input);
        let (_rem, stmt) = parse_let_statement(ts).expect("parser should accept unknown type name");
        if let StatementNode::Let(let_stmt) = stmt {
            match let_stmt.type_annotation {
                Some(ExpressionNode::Identifier(id)) => assert_eq!(id.name, "Foo"),
                other => panic!("expected identifier type ann, got {other:?}"),
            }
        } else {
            panic!("expected let stmt");
        }
    }

    #[test]
    fn test_let_annotation_allows_whitespace_after_colon() {
        let input = "let x:    Int = 1;";
        let (ts, _toks) = str_to_token_slice(input);
        let res = parse_let_statement(ts);
        assert!(res.is_ok());
    }

    #[test]
    fn test_let_annotation_only_whitespace_then_semicolon_errors() {
        let input = "let x:   ;";
        let (ts, _toks) = str_to_token_slice(input);
        assert!(parse_let_statement(ts).is_err());
    }

    #[test]
    fn test_let_annotation_trailing_comma_leaves_remaining_tokens() {
        let input = "let x: Int, = 1;";
        let (ts, _toks) = str_to_token_slice(input);
        let (rem, stmt) = parse_let_statement(ts).expect("parser should produce let stmt");
        if let StatementNode::Let(let_stmt) = stmt {
            assert!(matches!(let_stmt.type_annotation, Some(ExpressionNode::Identifier(_))));
        } else { panic!("expected let stmt"); }
        // Ensure comma remains for caller to handle
        assert!(matches!(rem.peek().map(|t| &t.token_type), Some(tlvxc_lexer::token::TokenType::Comma)));
    }

    #[test]
    fn test_let_annotation_complex_brackets_not_consumed() {
        // Only simple identifier is parsed as type; '[' remains
        let input = "let x: List[Int] = [];";
        let (ts, _toks) = str_to_token_slice(input);
        let (rem, stmt) = parse_let_statement(ts).expect("parser should accept simple id type");
        if let StatementNode::Let(let_stmt) = stmt {
            match let_stmt.type_annotation {
                Some(ExpressionNode::Identifier(id)) => assert_eq!(id.name, "List"),
                other => panic!("expected identifier type ann, got {other:?}"),
            }
        } else { panic!("expected let stmt"); }
        assert!(matches!(rem.peek().map(|t| &t.token_type), Some(tlvxc_lexer::token::TokenType::LeftBracket)));
    }

    #[test]
    fn test_let_annotation_complex_generics_not_consumed() {
        // Only simple identifier is parsed as type; '<' remains
        let input = "let x: List<Int>;";
        let (ts, _toks) = str_to_token_slice(input);
        let (rem, stmt) = parse_let_statement(ts).expect("parser should accept simple id type");
        if let StatementNode::Let(let_stmt) = stmt {
            match let_stmt.type_annotation {
                Some(ExpressionNode::Identifier(id)) => assert_eq!(id.name, "List"),
                other => panic!("expected identifier type ann, got {other:?}"),
            }
        } else { panic!("expected let stmt"); }
        assert!(matches!(rem.peek().map(|t| &t.token_type), Some(tlvxc_lexer::token::TokenType::Less)));
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
mod type_decls_test {
    use super::*;
    use tlvxc_ast::ast::{StatementNode, ExpressionNode, IdentifierNode};

    #[test]
    fn test_parse_type_declaration_basic() {
        let input = "type Person { name: String, age: Int }";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_type_declaration(ts).expect("should parse type decl");
        match stmt {
            StatementNode::TypeDecl(td) => {
                assert_eq!(td.name.name, "Person");
                assert_eq!(td.fields.len(), 2);
                assert_eq!(td.fields[0].name, "name");
                assert!(matches!(td.fields[0].type_annotation, ExpressionNode::Identifier(_)));
                assert_eq!(td.fields[1].name, "age");
                assert!(matches!(td.fields[1].type_annotation, ExpressionNode::Identifier(_)));
            }
            _ => panic!("expected type decl"),
        }
    }

    #[test]
    fn test_parse_type_declaration_with_semicolon() {
        let input = "type Vital { value: Float, unit: String };";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_type_declaration(ts).expect("should parse type decl with semicolon");
        if let StatementNode::TypeDecl(td) = stmt { 
            assert_eq!(td.name.name, "Vital");
            assert_eq!(td.fields.len(), 2);
        } else { panic!("expected type decl"); }
    }

    #[test]
    fn test_parse_empty_type_declaration() {
        let input = "type Empty { }";
        let (ts, _tokens) = str_to_token_slice(input);
        let (_rem, stmt) = parse_type_declaration(ts).expect("should parse empty type decl");
        if let StatementNode::TypeDecl(td) = stmt { 
            assert_eq!(td.name.name, "Empty");
            assert!(td.fields.is_empty());
        } else { panic!("expected type decl"); }
    }
}

#[cfg(test)]
mod functions_test {
    use super::*;
    use tlvxc_ast::ast::{StatementNode, ExpressionNode};

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
