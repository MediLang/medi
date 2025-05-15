use super::*;

#[cfg(test)]
mod parser_tests {
    use super::*;
    use medic_ast::ast::{BlockNode, ExpressionNode, LiteralNode, LiteralValueNode, StatementNode};
    use medic_lexer::lexer::Lexer;
    use medic_lexer::token::Token;

    /// Converts a source string into a `TokenSlice` by lexing it into tokens with a static lifetime.
    ///
    /// This helper is intended for use in tests, allowing parser functions to operate on tokenized input.
    ///
    /// # Examples
    ///
    /// ```
    /// let slice = str_to_token_slice("let x = 42;");
    /// assert!(!slice.0.is_empty());
    /// ```
    fn str_to_token_slice(input: &str) -> TokenSlice<'static>
    fn str_to_token_slice(input: &str) -> TokenSlice<'static> {
        let tokens: Vec<Token> = Lexer::new(input).collect();
        // Note: This is a workaround for testing purposes only.
        // In a real application, we would need to ensure the lifetime of the tokens
        // matches the lifetime of the TokenSlice.
        let tokens_box = Box::new(tokens);
        let tokens_static = Box::leak(tokens_box);
        TokenSlice(tokens_static)
    }

    #[test]
    /// ```
    fn test_let_statement() {
        let input = "let x = 42;";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_let_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert_eq!(let_stmt.name.name, "x");
                assert!(matches!(
                    let_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(LiteralValueNode::Int(42)))
                ));
            }
            _ => panic!("Expected Let statement"),
        }
    }

    #[test]
    /// Tests parsing of an assignment statement and verifies the AST node structure.
    ///
    /// # Examples
    ///
    /// ```
    /// test_assignment_statement();
    /// ```
    fn test_assignment_statement() {
    fn test_assignment_statement() {
        let input = "x = 42;";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_assignment_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Assignment(assign_stmt) => {
                assert!(
                    matches!(assign_stmt.target, ExpressionNode::Identifier(id) if id.name == "x")
                );
                assert!(matches!(
                    assign_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(LiteralValueNode::Int(42)))
                ));
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    /// Tests parsing of a block statement containing a let declaration, an assignment, and an integer expression.
    ///
    /// Asserts that the parsed block contains three statements in the expected order and types.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "{
    ///     let x = 42;
    ///     x = 43;
    ///     44
    /// }";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, block) = parse_block(token_slice).unwrap();
    /// let BlockNode { statements } = block;
    /// assert_eq!(statements.len(), 3);
    /// assert!(matches!(statements[0], StatementNode::Let(_)));
    /// assert!(matches!(statements[1], StatementNode::Assignment(_)));
    /// assert!(matches!(
    ///     statements[2],
    ///     StatementNode::Expr(ExpressionNode::Literal(LiteralNode::Int(
    ///         LiteralValueNode::Int(44)
    ///     )))
    /// ));
    /// ```
    fn test_block_statement() {
        let input = "{
            let x = 42;
            x = 43;
            44
        }";
        let token_slice = str_to_token_slice(input);
        let (_, block) = parse_block(token_slice).unwrap();
        let BlockNode { statements } = block;
        assert_eq!(statements.len(), 3);
        assert!(matches!(statements[0], StatementNode::Let(_)));
        assert!(matches!(statements[1], StatementNode::Assignment(_)));
        assert!(matches!(
            statements[2],
            StatementNode::Expr(ExpressionNode::Literal(LiteralNode::Int(
                LiteralValueNode::Int(44)
            )))
        ));
    }

    #[test]
    #[ignore] /// Tests parsing of an `if-else` statement with nested `else if` and verifies the structure of the resulting AST.
    ///
    /// The test checks that the parsed node is an `If` statement with a binary condition, a then branch containing a `let` statement, and an else branch also containing a `let` statement.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "if x == 42 {
    ///     let y = 1;
    /// } else if x == 43 {
    ///     let y = 2;
    /// } else {
    ///     let y = 3;
    /// }";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_if_statement(token_slice).unwrap();
    /// match stmt {
    ///     StatementNode::If(if_node) => {
    ///         assert!(matches!(if_node.condition, ExpressionNode::Binary(_)));
    ///         assert_eq!(if_node.then_branch.statements.len(), 1);
    ///         assert!(matches!(if_node.then_branch.statements[0], StatementNode::Let(_)));
    ///         let else_branch = if_node.else_branch.unwrap();
    ///         assert_eq!(else_branch.statements.len(), 1);
    ///         assert!(matches!(else_branch.statements[0], StatementNode::Let(_)));
    ///     }
    ///     _ => panic!("Expected If statement"),
    /// }
    /// ```
    fn test_if_else_statement() {
        let input = "if x == 42 {
            let y = 1;
        } else if x == 43 {
            let y = 2;
        } else {
            let y = 3;
        }";
        let token_slice = str_to_token_slice(input);
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
                assert!(matches!(else_statements[0], StatementNode::Let(_)));
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    #[ignore] /// Tests parsing of a `while` statement, verifying the condition and body statements.
    ///
    /// The test parses a `while` loop with a binary condition and a body containing an assignment and a `let` statement. It asserts that the parsed node is a `While` statement with the expected structure.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "while x < 42 {
    ///     x = x + 1;
    ///     let y = x * 2;
    /// }";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_while_statement(token_slice).unwrap();
    /// match stmt {
    ///     StatementNode::While(while_stmt) => {
    ///         assert!(matches!(while_stmt.condition, ExpressionNode::Binary(_)));
    ///         let BlockNode { statements } = while_stmt.body;
    ///         assert_eq!(statements.len(), 2);
    ///         assert!(matches!(statements[0], StatementNode::Assignment(_)));
    ///         assert!(matches!(statements[1], StatementNode::Let(_)));
    ///     }
    ///     _ => panic!("Expected While statement"),
    /// }
    /// ```
    fn test_while_statement() {
        let input = "while x < 42 {
            x = x + 1;
            let y = x * 2;
        }";
        let token_slice = str_to_token_slice(input);
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
    #[ignore] /// Tests parsing of a `for` loop statement and verifies the loop variable name.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "for x in 1..10 {
    ///     let y = x * 2;
    /// }";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_for_statement(token_slice).unwrap();
    /// match stmt {
    ///     StatementNode::For(for_node) => {
    ///         assert_eq!(for_node.var.name, "x");
    ///     }
    ///     _ => panic!("Expected For statement"),
    /// }
    /// ```
    fn test_for_statement() {
        let input = "for x in 1..10 {
            let y = x * 2;
        }";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_for_statement(token_slice).unwrap();
        match stmt {
            StatementNode::For(for_node) => {
                assert_eq!(for_node.var.name, "x");
            }
            _ => panic!("Expected For statement"),
        }
    }

    #[test]
    #[ignore] /// Tests parsing of a `match` statement and verifies the resulting AST node is a `Match` statement.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "match x {
    ///     42 => true,
    ///     _ => false
    /// }";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_match_statement(token_slice).unwrap();
    /// assert!(matches!(stmt, StatementNode::Match(_)));
    /// ```
    fn test_match_statement() {
        let input = "match x {
            42 => true,
            _ => false
        }";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_match_statement(token_slice).unwrap();
        assert!(matches!(stmt, StatementNode::Match(_)));
    }

    #[test]
    /// Tests parsing of a return statement with a value.
    ///
    /// Asserts that parsing `return 42;` produces a `Return` statement node with a non-empty return value.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "return 42;";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_return_statement(token_slice).unwrap();
    /// match stmt {
    ///     StatementNode::Return(return_node) => {
    ///         assert!(return_node.value.is_some());
    ///     }
    ///     _ => panic!("Expected Return statement"),
    /// }
    /// ```
    fn test_return_statement() {
        let input = "return 42;";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_return_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Return(return_node) => {
                assert!(matches!(return_node.value.is_some(), true));
            }
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    /// Tests parsing of a `return` statement without a value, ensuring it produces a `Return` node with no return value.
    ///
    /// # Examples
    ///
    /// ```
    /// let input = "return;";
    /// let token_slice = str_to_token_slice(input);
    /// let (_, stmt) = parse_return_statement(token_slice).unwrap();
    /// match stmt {
    ///     StatementNode::Return(return_node) => {
    ///         assert!(return_node.value.is_none());
    ///     }
    ///     _ => panic!("Expected Return statement"),
    /// }
    /// ```
    fn test_return_unit_statement() {
        let input = "return;";
        let token_slice = str_to_token_slice(input);
        let (_, stmt) = parse_return_statement(token_slice).unwrap();
        match stmt {
            StatementNode::Return(return_node) => {
                assert!(return_node.value.is_none());
            }
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    /// Tests parsing of boolean and integer literals, ensuring correct AST node construction.
    ///
    /// This test verifies that the parser correctly recognizes and constructs AST nodes for
    /// the boolean literals `true` and `false`, as well as for integer literals.
    ///
    /// # Examples
    ///
    /// ```
    /// let token_slice = str_to_token_slice("true");
    /// let (_, lit) = parse_literal(token_slice).unwrap();
    /// assert!(matches!(lit, LiteralNode::Bool(LiteralValueNode::Bool(true))));
    ///
    /// let token_slice = str_to_token_slice("42");
    /// let (_, lit) = parse_literal(token_slice).unwrap();
    /// assert!(matches!(lit, LiteralNode::Int(LiteralValueNode::Int(42))));
    /// ```
    fn test_literal() {
        // Test true literal
        let token_slice = str_to_token_slice("true");
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(
            lit,
            LiteralNode::Bool(LiteralValueNode::Bool(true))
        ));

        // Test false literal
        let token_slice = str_to_token_slice("false");
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(
            lit,
            LiteralNode::Bool(LiteralValueNode::Bool(false))
        ));

        // Test integer literal
        let token_slice = str_to_token_slice("42");
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(matches!(lit, LiteralNode::Int(LiteralValueNode::Int(42))));
    }

    #[test]
    #[ignore] /// Tests parsing of a floating-point literal and verifies the parsed value matches the expected float.
    ///
    /// # Examples
    ///
    /// ```
    /// let token_slice = str_to_token_slice("3.14");
    /// let (_, lit) = parse_literal(token_slice).unwrap();
    /// assert!(
    ///     matches!(lit, LiteralNode::Float(LiteralValueNode::Float(val)) if (val - 3.14).abs() < f64::EPSILON)
    /// );
    /// ```
    fn test_float_literal() {
        let token_slice = str_to_token_slice("3.14");
        let (_, lit) = parse_literal(token_slice).unwrap();
        assert!(
            matches!(lit, LiteralNode::Float(LiteralValueNode::Float(val)) if (val - 3.14).abs() < f64::EPSILON)
        );
    }

    #[test]
    #[ignore] /// Tests parsing of medical code expressions for ICD10, CPT, and SNOMED codes.
    ///
    /// Verifies that the parser correctly recognizes and constructs AST nodes for each supported medical code type.
    ///
    /// # Examples
    ///
    /// ```
    /// let token_slice = str_to_token_slice("ICD10:A01.1");
    /// let (_, expr) = parse_expression(token_slice).unwrap();
    /// assert!(matches!(expr, ExpressionNode::IcdCode(code) if code == "ICD10:A01.1"));
    ///
    /// let token_slice = str_to_token_slice("CPT:12345");
    /// let (_, expr) = parse_expression(token_slice).unwrap();
    /// assert!(matches!(expr, ExpressionNode::CptCode(code) if code == "CPT:12345"));
    ///
    /// let token_slice = str_to_token_slice("SNOMED:123456");
    /// let (_, expr) = parse_expression(token_slice).unwrap();
    /// assert!(matches!(expr, ExpressionNode::SnomedCode(code) if code == "SNOMED:123456"));
    /// ```
    fn test_medical_codes() {
        // Test ICD code
        let token_slice = str_to_token_slice("ICD10:A01.1");
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::IcdCode(code) if code == "ICD10:A01.1"));

        // Test CPT code
        let token_slice = str_to_token_slice("CPT:12345");
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::CptCode(code) if code == "CPT:12345"));

        // Test SNOMED code
        let token_slice = str_to_token_slice("SNOMED:123456");
        let (_, expr) = parse_expression(token_slice).unwrap();
        assert!(matches!(expr, ExpressionNode::SnomedCode(code) if code == "SNOMED:123456"));
    }
}
