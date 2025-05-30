use super::ast::{ExpressionNode, StatementNode};

impl From<StatementNode> for ExpressionNode {
    fn from(stmt: StatementNode) -> Self {
        ExpressionNode::from_statement(stmt)
    }
}
