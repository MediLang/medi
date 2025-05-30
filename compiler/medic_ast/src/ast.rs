// Abstract Syntax Tree (AST) definitions for the Medi language in Rust
// This is a starting point, mapping the core node types from your TypeScript AST

/// Represents an expression in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionNode {
    /// An identifier (variable name, function name, etc.)
    Identifier(IdentifierNode),
    /// An ICD code literal
    IcdCode(String),
    /// A CPT code literal
    CptCode(String),
    /// A SNOMED CT code literal
    SnomedCode(String),
    /// A literal value (number, string, boolean, etc.)
    Literal(LiteralNode),
    /// A binary operation (e.g., 1 + 2, x * y)
    Binary(Box<BinaryExpressionNode>),
    /// A function call (e.g., add(1, 2))
    Call(Box<CallExpressionNode>),
    /// A member access (e.g., object.property)
    Member(Box<MemberExpressionNode>),
    /// A healthcare-specific query
    HealthcareQuery(Box<HealthcareQueryNode>),
    /// A statement expression (e.g., a block expression)
    Statement(Box<StatementNode>),
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

/// Binary operators in order of increasing precedence
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Logical OR (left-associative)
    Or,

    // Logical AND (left-associative)
    And,

    // Medical-specific operators (left-associative)
    Of,  // 'of' for temporal quantities (e.g., '2 of 3 doses')
    Per, // 'per' for rates (e.g., '5 mg per day')

    // Equality (left-associative)
    Eq, // ==
    Ne, // !=

    // Comparison (left-associative)
    Lt, // <
    Le, // <=
    Gt, // >
    Ge, // >=

    // Unit conversion (right-associative, highest precedence)
    UnitConversion, // → (e.g., '5 mg→g')

    // Bitwise OR (left-associative)
    BitOr, // |

    // Bitwise XOR (left-associative)
    BitXor, // ^

    // Bitwise AND (left-associative)
    BitAnd, // &

    // Bit shifts (left-associative)
    Shl, // <<
    Shr, // >>

    // Addition/Subtraction (left-associative)
    Add, // +
    Sub, // -

    // Multiplication/Division/Modulo (left-associative)
    Mul, // *
    Div, // /
    Mod, // %

    // Exponentiation (right-associative)
    Pow, // **

    // Range (right-associative)
    Range, // ..

    // Null-coalescing (right-associative)
    NullCoalesce, // ??

    // Elvis operator (right-associative)
    Elvis, // ?:
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpressionNode {
    pub callee: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpressionNode {
    pub object: ExpressionNode,
    pub property: IdentifierNode,
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

use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentifierNode {
    pub name: String,
}

impl IdentifierNode {
    pub fn new(name: String) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
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

impl ExpressionNode {
    /// Creates an expression from a statement
    /// 
    /// If the statement is an expression statement, returns the inner expression.
    /// Otherwise, wraps the statement in a `Statement` variant.
    pub fn from_statement(stmt: StatementNode) -> Self {
        match stmt {
            StatementNode::Expr(expr) => expr,
            _ => ExpressionNode::Statement(Box::new(stmt)),
        }
    }
}
