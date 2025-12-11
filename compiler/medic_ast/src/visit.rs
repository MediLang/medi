//! Visitor pattern implementation for traversing and transforming the AST.
//!
//! This module provides a visitor pattern implementation for traversing the AST.
//! It includes a base `Visitor` trait that can be implemented to perform different
//! operations on the AST nodes, and a `Visitable` trait that AST nodes implement
//! to accept visitors.

use crate::ast::*;
/// The result type for visitor operations.
pub type VisitResult<T = ()> = Result<T, VisitError>;

/// An error that can occur during AST traversal.
#[derive(Debug, thiserror::Error)]
pub enum VisitError {
    /// An error with a custom message.
    #[error("{0}")]
    Custom(String),

    /// An error that occurred at a specific location in the source.
    #[error("{message} at {location:?}")]
    Located {
        /// The error message.
        message: String,
        /// The source location where the error occurred.
        location: Option<Span>,
    },
}

impl VisitError {
    /// Creates a new custom error with the given message.
    pub fn custom<T: Into<String>>(msg: T) -> Self {
        VisitError::Custom(msg.into())
    }

    /// Creates a new located error.
    pub fn located<T: Into<String>>(msg: T, location: Option<Span>) -> Self {
        VisitError::Located {
            message: msg.into(),
            location,
        }
    }
}

/// A trait for types that can be visited by a `Visitor`.
pub trait Visitable {
    /// Accepts a visitor and calls the appropriate visit method.
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output>;

    /// Visits the children of this node with the given visitor.
    ///
    /// The default implementation does nothing.
    fn visit_children<V: Visitor + ?Sized>(&self, _visitor: &mut V) -> VisitResult<V::Output> {
        Ok(Default::default())
    }
}

/// A visitor for traversing the AST.
///
/// Implement this trait to perform operations on the AST nodes.
/// The default implementations do nothing and return `Ok(())`.
pub trait Visitor {
    /// The output type of the visitor.
    type Output: Default;

    // Expression nodes
    fn visit_identifier(&mut self, node: &IdentifierNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_literal(&mut self, node: &LiteralNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpressionNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_call_expr(&mut self, node: &CallExpressionNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_member_expr(&mut self, node: &MemberExpressionNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_index_expr(&mut self, node: &IndexExpressionNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_healthcare_query(&mut self, node: &HealthcareQueryNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_struct_literal(&mut self, node: &StructLiteralNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_array_literal(&mut self, node: &ArrayLiteralNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_quantity_literal(&mut self, node: &QuantityLiteralNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Statement nodes
    fn visit_let_stmt(&mut self, node: &LetStatementNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_block(&mut self, node: &BlockNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_if_stmt(&mut self, node: &IfNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_while_loop(&mut self, node: &WhileNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_for_loop(&mut self, node: &ForNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_match(&mut self, node: &MatchNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_return(&mut self, node: &ReturnNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Function-related nodes
    fn visit_parameter(&mut self, node: &ParameterNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_function(&mut self, node: &FunctionNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Type declarations
    fn visit_type_decl(&mut self, node: &TypeDeclNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_regulate_stmt(&mut self, node: &RegulateNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Program
    fn visit_program(&mut self, node: &ProgramNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Pattern matching
    fn visit_pattern(&mut self, node: &PatternNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_match_arm(&mut self, node: &MatchArmNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    // Helper to visit children of a node
    fn visit_children<T: Visitable + ?Sized>(&mut self, node: &T) -> VisitResult<Self::Output> {
        node.visit_children(self)
    }
}

// Visitable implementations are now in ast.rs with the type definitions

/// A span in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Span {
    /// The starting byte index (inclusive).
    pub start: usize,
    /// The ending byte index (exclusive).
    pub end: usize,
    /// The line number (1-based).
    pub line: u32,
    /// The column number (1-based).
    pub column: u32,
}

impl Span {
    /// Creates a new span.
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Creates a span that covers both this span and another.
    pub fn to(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            column: self.column,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }
}

/// A trait for AST nodes that have a source location.
pub trait Spanned {
    /// Returns the source span of this node.
    fn span(&self) -> Span;
}

/// A visitor that collects all nodes of a specific type.
pub struct NodeCollector<T> {
    /// The collected nodes.
    pub nodes: Vec<T>,
}

impl<T> Default for NodeCollector<T> {
    fn default() -> Self {
        Self { nodes: Vec::new() }
    }
}

impl<T: Clone> NodeCollector<T> {
    /// Creates a new node collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a node to the collection.
    pub fn collect(&mut self, node: &T) {
        self.nodes.push(node.clone());
    }
}

/// A visitor that finds all nodes of a specific type.
pub struct NodeFinder<T, F> {
    /// The predicate used to match nodes.
    predicate: F,
    /// The found nodes.
    nodes: Vec<T>,
}

impl<T, F> NodeFinder<T, F>
where
    F: FnMut(&T) -> bool,
{
    /// Creates a new node finder with the given predicate.
    pub fn new(predicate: F) -> Self {
        Self {
            predicate,
            nodes: Vec::new(),
        }
    }

    /// Returns the found nodes.
    pub fn into_inner(self) -> Vec<T> {
        self.nodes
    }

    /// Checks if the given node matches the predicate and collects it if it does.
    pub fn check(&mut self, node: &T) -> bool
    where
        T: Clone,
    {
        if (self.predicate)(node) {
            self.nodes.push(node.clone());
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // Removed unused import

    #[test]
    fn test_visitor_pattern() {
        use crate::ast::Spanned;

        // Create a simple AST: 1 + 2 * 3
        let ast = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), Span::default())),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Binary(Spanned::new(
                    Box::new(BinaryExpressionNode {
                        left: ExpressionNode::Literal(Spanned::new(
                            LiteralNode::Int(2),
                            Span::default(),
                        )),
                        operator: BinaryOperator::Mul,
                        right: ExpressionNode::Literal(Spanned::new(
                            LiteralNode::Int(3),
                            Span::default(),
                        )),
                    }),
                    Span::default(),
                )),
            }),
            Span::default(),
        ));

        // Count the number of binary expressions
        struct BinaryCounter {
            count: usize,
        }

        impl Visitor for BinaryCounter {
            type Output = ();

            fn visit_binary_expr(&mut self, node: &BinaryExpressionNode) -> VisitResult<()> {
                self.count += 1;
                self.visit_children(node)
            }
        }

        let mut counter = BinaryCounter { count: 0 };
        ast.accept(&mut counter).unwrap();
        assert_eq!(counter.count, 2);
    }
}
