use tlvxc_ast::ast::*;
use tlvxc_ast::visit::{Span, VisitResult, Visitable, Visitor};
use tlvxc_ast::{to_json, Spanned};
use std::error::Error;

// Helper function to create a test span
fn test_span() -> Span {
    Span {
        start: 0,
        end: 0,
        line: 1,
        column: 1,
    }
}

/// A test visitor that counts the number of nodes visited
struct NodeCounter {
    count: usize,
}

impl NodeCounter {
    fn new() -> Self {
        NodeCounter { count: 0 }
    }
}

impl Visitor for NodeCounter {
    type Output = ();

    fn visit_identifier(&mut self, _node: &IdentifierNode) -> VisitResult<Self::Output> {
        self.count += 1;
        Ok(())
    }

    fn visit_literal(&mut self, _node: &LiteralNode) -> VisitResult<Self::Output> {
        self.count += 1;
        Ok(())
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpressionNode) -> VisitResult<Self::Output> {
        self.count += 1;
        self.visit_children(node)
    }
}

#[test]
fn test_spanned_node() {
    // Test creating a spanned node
    let span = test_span();

    let node = Spanned::new(42, span);
    assert_eq!(node.node, 42);
    assert_eq!(node.span, span);

    // Test mapping
    let node = node.map(|n| n * 2);
    assert_eq!(node.node, 84);
    assert_eq!(node.span, span);
}

#[test]
fn test_expression_node_visitor() -> Result<(), Box<dyn Error>> {
    // Create a simple expression: 42
    let expr = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(42), test_span()));

    // Visit the expression
    let mut counter = NodeCounter::new();
    expr.accept(&mut counter)?;

    // Verify the visitor was called
    assert_eq!(counter.count, 1);

    // Test serialization
    let json = to_json(&expr).map_err(|e| e.to_string())?;
    assert!(json.contains("42"));

    Ok(())
}

#[test]
fn test_binary_expression_visitor() -> Result<(), Box<dyn Error>> {
    // Create a binary expression: 1 + 2
    let left = LiteralNode::Int(1);
    let right = LiteralNode::Int(2);

    let expr = ExpressionNode::Binary(Spanned::new(
        Box::new(BinaryExpressionNode {
            left: ExpressionNode::Literal(Spanned::new(left, test_span())),
            operator: BinaryOperator::Add,
            right: ExpressionNode::Literal(Spanned::new(right, test_span())),
        }),
        test_span(),
    ));

    // Visit the expression
    let mut counter = NodeCounter::new();
    expr.accept(&mut counter)?;

    // Verify the visitor was called for both literals and the binary expression
    assert_eq!(counter.count, 3);

    // Test serialization
    let json = to_json(&expr).map_err(|e| e.to_string())?;
    assert!(json.contains("1"));
    assert!(json.contains("2"));
    assert!(json.contains("Add"));

    Ok(())
}
