use super::ast::{ExpressionNode, StatementNode};
use super::visit::Span;

impl From<StatementNode> for ExpressionNode {
    fn from(stmt: StatementNode) -> Self {
        ExpressionNode::from_statement(stmt)
    }
}

impl From<medic_lexer::Location> for Span {
    fn from(location: medic_lexer::Location) -> Self {
        // For a single token, start and end are the same position
        // The offset in Location is 0-based, and line/column are 1-based, which matches Span
        Span {
            start: location.offset,
            end: location.offset, // Same as start for a single token
            line: location.line as u32,
            column: location.column as u32,
        }
    }
}
