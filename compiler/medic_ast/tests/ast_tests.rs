use medic_ast::ast::*;
use medic_ast::visit::{Visitor, VisitResult, Span};
use medic_ast::{to_json, Spanned};

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
    
    // Add other visit methods as needed
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
fn test_expression_node_visitor() -> Result<(), Box<dyn std::error::Error>> {
    // Create a simple expression: 42
    let span = test_span();
    
    let lit = LiteralNode::Int(42);
    let expr = ExpressionNode::Literal(Spanned::new(lit, span));
    
    // Test visitor
    let mut counter = NodeCounter::new();
    expr.accept(&mut counter)?;
    assert_eq!(counter.count, 1);
    
    // Test serialization
    let json = to_json(&expr)?;
    assert!(json.contains("42"));
    
    Ok(())
}

#[test]
fn test_binary_expression_visitor() -> Result<(), Box<dyn std::error::Error>> {
    // Create a binary expression: 1 + 2
    let span = test_span();
    
    let left = Spanned::new(LiteralNode::Int(1), span);
    let right = Spanned::new(LiteralNode::Int(2), span);
    
    let bin_expr = BinaryExpressionNode {
        left: Box::new(ExpressionNode::Literal(left)),
        operator: BinaryOperator::Plus,
        right: Box::new(ExpressionNode::Literal(right)),
    };
    
    let expr = ExpressionNode::Binary(Spanned::new(Box::new(bin_expr), span));
    
    // Test visitor
    let mut counter = NodeCounter::new();
    expr.accept(&mut counter)?;
    assert_eq!(counter.count, 3);  // 2 literals + 1 binary expression
    
    // Test serialization
    let json = to_json(&expr)?;
    assert!(json.contains("1"));
    assert!(json.contains("2"));
    assert!(json.contains("+"));
    
    Ok(())
}
