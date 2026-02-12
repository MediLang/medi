// Unit tests for the Medi parser (Rust)
use tlvxc::parser::{parse_expression, parse_program};
use tlvxc::ast::{ExpressionNode, LiteralNode, BinaryOperator, BinaryExpressionNode, StatementNode, LetStatementNode, AssignmentNode, BlockNode, IfNode};

#[test]
fn test_simple_binary_expression() {
    let input = "1 + 2 * 3";
    let (_rest, expr) = parse_expression(input).unwrap();
    assert_eq!(expr, ExpressionNode::Binary(Box::new(BinaryExpressionNode {
        left: ExpressionNode::Literal(LiteralNode::Int(1)),
        operator: BinaryOperator::Add,
        right: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
            left: ExpressionNode::Literal(LiteralNode::Int(2)),
            operator: BinaryOperator::Mul,
            right: ExpressionNode::Literal(LiteralNode::Int(3)),
        })),
    })));
}

#[test]
fn test_function_call_expression() {
    use tlvxc::ast::CallExpressionNode;
    let input = "foo(bar, 42)";
    let (_rest, expr) = parse_expression(input).unwrap();
    assert_eq!(expr, ExpressionNode::Call(Box::new(CallExpressionNode {
        callee: ExpressionNode::Identifier("foo".to_string()),
        arguments: vec![
            ExpressionNode::Identifier("bar".to_string()),
            ExpressionNode::Literal(LiteralNode::Int(42)),
        ],
    })));
}

#[test]
fn test_healthcare_query_expression() {
    use tlvxc::ast::HealthcareQueryNode;
    let input = "fhir_query(\"Patient\", age > 65)";
    let (_rest, expr) = parse_expression(input).unwrap();
    assert_eq!(expr, ExpressionNode::HealthcareQuery(Box::new(HealthcareQueryNode {
        query_type: "fhir_query".to_string(),
        arguments: vec![
            ExpressionNode::Literal(LiteralNode::String("Patient".to_string())),
            ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                left: ExpressionNode::Identifier("age".to_string()),
                operator: BinaryOperator::Gt,
                right: ExpressionNode::Literal(LiteralNode::Int(65)),
            })),
        ],
    })));
}

#[test]
fn test_member_access_expression() {
    use tlvxc::ast::MemberExpressionNode;
    let input = "patient.name.first";
    let (_rest, expr) = parse_expression(input).unwrap();
    assert_eq!(expr, ExpressionNode::Member(Box::new(MemberExpressionNode {
        object: ExpressionNode::Member(Box::new(MemberExpressionNode {
            object: ExpressionNode::Identifier("patient".to_string()),
            property: "name".to_string(),
        })),
        property: "first".to_string(),
    })));
}
#[test]
fn test_parenthesized_expression() {
    let input = "(1 + 2) * 3";
    let (_rest, expr) = parse_expression(input).unwrap();
    assert_eq!(expr, ExpressionNode::Binary(Box::new(BinaryExpressionNode {
        left: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
            left: ExpressionNode::Literal(LiteralNode::Int(1)),
            operator: BinaryOperator::Add,
            right: ExpressionNode::Literal(LiteralNode::Int(2)),
        })),
        operator: BinaryOperator::Mul,
        right: ExpressionNode::Literal(LiteralNode::Int(3)),
    })));
}


#[test]
fn test_parse_program_integration() {
    let input = r#"
        let a = 1;
        let b = 2;
        a = a + b;
        if (a > b) { let c = 3; } else { let c = 4; }
    "#;
    let (_rest, stmts) = parse_program(input).unwrap();
    assert_eq!(stmts.len(), 4);
    assert!(matches!(stmts[0], StatementNode::Let(_)));
    assert!(matches!(stmts[1], StatementNode::Let(_)));
    assert!(matches!(stmts[2], StatementNode::Assignment(_)));
    assert!(matches!(stmts[3], StatementNode::If(_)));
}
