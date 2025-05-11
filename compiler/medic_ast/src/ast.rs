// Abstract Syntax Tree (AST) definitions for the Medi language in Rust
// This is a starting point, mapping the core node types from your TypeScript AST

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Identifier(String),
    IcdCode(String),
    CptCode(String),
    SnomedCode(String),
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
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Assign,
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

#[derive(Debug, Clone, PartialEq)]
pub enum StatementNode {
    Let(Box<LetStatementNode>),
    Assignment(Box<AssignmentNode>),
    Expr(ExpressionNode),
    Block(BlockNode),
    If(Box<IfNode>),
    While(Box<WhileNode>),
    For(Box<ForNode>),
    Match(Box<MatchNode>),
    Return(Box<ReturnNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatementNode {
    pub name: String,
    pub value: ExpressionNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentNode {
    pub target: ExpressionNode, // Usually Identifier or Member
    pub value: ExpressionNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockNode {
    pub statements: Vec<StatementNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfNode {
    pub condition: ExpressionNode,
    pub then_branch: BlockNode,
    pub else_branch: Option<BlockNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileNode {
    pub condition: ExpressionNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub var: String,
    pub iter: ExpressionNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchNode {
    pub expr: ExpressionNode,
    pub arms: Vec<(ExpressionNode, BlockNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnNode {
    pub value: Option<ExpressionNode>,
}

// Add more AST nodes as needed (statements, declarations, etc.)
