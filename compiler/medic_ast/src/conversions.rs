use super::ast::{ExpressionNode, StatementNode};

impl From<StatementNode> for ExpressionNode {
    fn from(stmt: StatementNode) -> Self {
        match stmt {
            StatementNode::Expr(expr) => expr,
            _ => ExpressionNode::Literal(crate::ast::LiteralNode::String(
                "Statement cannot be converted to expression".to_string(),
            )),
        }
    }
}
