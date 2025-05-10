// Abstract Syntax Tree (AST) definitions for the Medi language in Rust
// This is a starting point, mapping the core node types from your TypeScript AST

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Identifier(String),
    Literal(LiteralNode),
    Binary(Box<BinaryExpressionNode>),
    Call(Box<CallExpressionNode>),
    Member(Box<MemberExpressionNode>),
    HealthcareQuery(Box<HealthcareQueryNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralNode {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpressionNode {
    pub left: ExpressionNode,
    pub operator: BinaryOperator,
    pub right: ExpressionNode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, Lt, Gt, Le, Ge,
    And, Or, Assign,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpressionNode {
    pub callee: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpressionNode {
    pub object: ExpressionNode,
    pub property: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HealthcareQueryNode {
    pub query_type: String,
    pub arguments: Vec<ExpressionNode>,
}

// Add more AST nodes as needed (statements, declarations, etc.)
