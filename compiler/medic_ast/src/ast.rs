// Abstract Syntax Tree (AST) definitions for the Medi language in Rust
// This is a starting point, mapping the core node types from your TypeScript AST

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    Identifier(IdentifierNode),
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
    // Direct representation of literal values
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
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Range,
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
    pub name: IdentifierNode,
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
    pub var: IdentifierNode,
    pub iter: ExpressionNode,
    pub body: BlockNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchNode {
    pub expr: Box<ExpressionNode>,
    pub arms: Vec<MatchArmNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnNode {
    pub value: Option<Box<ExpressionNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierNode {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternNode {
    Literal(LiteralNode),       // Pattern can be a literal
    Identifier(IdentifierNode), // Pattern can be an identifier
    Wildcard,                   // Represents the '_' pattern
                                // TODO: Add other pattern types like StructPattern, TuplePattern, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArmNode {
    pub pattern: PatternNode,
    pub body: Box<ExpressionNode>, // Body of the arm, could also be a BlockNode in more complex scenarios
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramNode {
    pub statements: Vec<StatementNode>,
}

// Add more AST nodes as needed (statements, declarations, etc.)
