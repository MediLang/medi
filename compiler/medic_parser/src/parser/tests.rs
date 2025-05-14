use super::*;

#[cfg(test)]
mod parser_tests {
    use super::*;
    use medic_ast::ast::{BlockNode, StatementNode};

    #[test]
    fn test_let_statement() {
        let input = "let x = 42;";
        let (_, stmt) = parse_let_statement(input).unwrap();
        match stmt {
            StatementNode::Let(let_stmt) => {
                assert_eq!(let_stmt.name, "x");
                assert!(matches!(
                    let_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(42))
                ));
            }
            _ => panic!("Expected Let statement"),
        }
    }

    #[test]
    fn test_assignment_statement() {
        let input = "x = 42;";
        let (_, stmt) = parse_assignment(input).unwrap();
        match stmt {
            StatementNode::Assignment(assign_stmt) => {
                assert!(matches!(assign_stmt.target, ExpressionNode::Identifier(id) if id == "x"));
                assert!(matches!(
                    assign_stmt.value,
                    ExpressionNode::Literal(LiteralNode::Int(42))
                ));
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_block_statement() {
        let input = "{
            let x = 42;
            x = 43;
            44
        }";
        let (_, stmt) = parse_block(input).unwrap();
        match stmt {
            StatementNode::Block(BlockNode { statements }) => {
                assert_eq!(statements.len(), 3);
                assert!(matches!(statements[0], StatementNode::Let(_)));
                assert!(matches!(statements[1], StatementNode::Assignment(_)));
                assert!(matches!(
                    statements[2],
                    StatementNode::Expr(ExpressionNode::Literal(LiteralNode::Int(44)))
                ));
            }
            _ => panic!("Expected Block statement"),
        }
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
        let (_, stmt) = parse_if_statement(input).unwrap();
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
    fn test_while_statement() {
        let input = "while x < 42 {
            x = x + 1;
            let y = x * 2;
        }";
        let (_, stmt) = parse_while_statement(input).unwrap();
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
    fn test_for_statement() {
        let input = "for x in 1..10 {
            let y = x * 2;
        }";
        let (_, stmt) = parse_for_statement(input).unwrap();
        match stmt {
            StatementNode::For(for_node) => {
                assert_eq!(for_node.var, "x");
            }
            _ => panic!("Expected For statement"),
        }
    }

    #[test]
    fn test_match_statement() {
        let input = "match x {
            42 => true,
            _ => false
        }";
        let (_, stmt) = parse_match_statement(input).unwrap();
        assert!(matches!(stmt, StatementNode::Match(_)));
    }

    #[test]
    fn test_return_statement() {
        let input = "return 42;";
        let (_, stmt) = parse_return_statement(input).unwrap();
        match stmt {
            StatementNode::Return(return_node) => {
                assert!(matches!(
                    return_node.value,
                    Some(ExpressionNode::Literal(LiteralNode::Int(42)))
                ));
            }
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    fn test_return_unit_statement() {
        let input = "return;";
        let (_, stmt) = parse_return_statement(input).unwrap();
        match stmt {
            StatementNode::Return(return_node) => {
                assert!(return_node.value.is_none());
            }
            _ => panic!("Expected Return statement"),
        }
    }

    #[test]
    fn test_bool_literal() {
        assert!(matches!(
            parse_bool_literal("true").unwrap().1,
            ExpressionNode::Literal(LiteralNode::Bool(true))
        ));
        assert!(matches!(
            parse_bool_literal("false").unwrap().1,
            ExpressionNode::Literal(LiteralNode::Bool(false))
        ));
    }

    #[test]
    fn test_float_literal() {
        let (_, expr) = parse_float_literal("3.14").unwrap();
        assert!(matches!(
            expr,
            ExpressionNode::Literal(LiteralNode::Float(std::f64::consts::PI))
        ));
    }

    #[test]
    fn test_icd_code() {
        let (_, expr) = parse_member_expr("ICD10:A01.1").unwrap();
        assert!(matches!(expr, ExpressionNode::IcdCode(code) if code == "ICD10:A01.1"));
    }

    #[test]
    fn test_cpt_code() {
        let (_, expr) = parse_member_expr("CPT:12345").unwrap();
        assert!(matches!(expr, ExpressionNode::CptCode(code) if code == "CPT:12345"));
    }

    #[test]
    fn test_snomed_code() {
        let (_, expr) = parse_member_expr("SNOMED:123456").unwrap();
        assert!(matches!(
            expr,
            ExpressionNode::SnomedCode(code) if code == "SNOMED:123456"
        ));
    }
}
