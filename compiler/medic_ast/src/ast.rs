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
    /// A struct literal (e.g., `Patient { id: 1, name: "John" }`)
    Struct(Box<StructLiteralNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralNode {
    // Direct representation of literal values
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl std::fmt::Display for LiteralNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LiteralNode::Int(i) => write!(f, "{}", i),
            LiteralNode::Float(fl) => write!(f, "{}", fl),
            LiteralNode::Bool(b) => write!(f, "{}", b),
            LiteralNode::String(s) => write!(f, "\"{}\"", s),
        }
    }
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
    // Assignment (right-associative, lowest precedence)
    Assign,

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

impl BlockNode {
    /// Formats the block with the specified indentation level.
    /// 
    /// # Arguments
    /// * `f` - The formatter to write to
    /// * `indent_level` - The current indentation level (number of indents, not spaces)
    /// * `indent_size` - Number of spaces per indentation level (default: 4)
    pub fn fmt_indented(
        &self, 
        f: &mut std::fmt::Formatter,
        indent_level: usize,
        indent_size: usize
    ) -> std::fmt::Result {
        let indent = " ".repeat(indent_level * indent_size);
        let stmt_indent = " ".repeat((indent_level + 1) * indent_size);
        
        writeln!(f, "{}{{", indent)?;
        for stmt in &self.statements {
            // Handle nested blocks by passing increased indentation
            if let StatementNode::Block(block) = stmt {
                block.fmt_indented(f, indent_level + 1, indent_size)?;
            } else {
                write!(f, "{}{};", stmt_indent, stmt)?;
            }
            writeln!(f)?;
        }
        write!(f, "{}}}", indent)
    }
}

impl std::fmt::Display for BlockNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_indented(f, 0, 4)  // Default to 4-space indentation, starting at level 0
    }
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

/// Represents a field in a struct literal
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub value: ExpressionNode,
}

/// Represents a struct literal expression (e.g., `Patient { id: 1, name: "John" }`)
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralNode {
    pub type_name: String,
    pub fields: Vec<StructField>,
}

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
    Variant {                    // Represents a variant pattern like Some(x)
        name: String,           // The variant name (e.g., "Some")
        inner: Box<PatternNode>  // The inner pattern (e.g., the x in Some(x))
    },
    // TODO: Add other pattern types like StructPattern, TuplePattern, etc.
}

impl std::fmt::Display for PatternNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PatternNode::Literal(lit) => write!(f, "{}", lit),
            PatternNode::Identifier(ident) => write!(f, "{}", ident.name),
            PatternNode::Wildcard => write!(f, "_"),
            PatternNode::Variant { name, inner } => write!(f, "{}({})", name, inner),
        }
    }
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

impl std::fmt::Display for StatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StatementNode::Let(stmt) => write!(f, "let {} = {};", stmt.name.name, stmt.value),
            StatementNode::Assignment(assign) => write!(f, "{} = {};", assign.target, assign.value),
            StatementNode::Expr(expr) => write!(f, "{};", expr),
            StatementNode::Block(block) => {
                // Use the indentation-aware formatting for blocks
                // Start with 1 level of indentation since this is inside a statement
                block.fmt_indented(f, 1, 4)
            }
            StatementNode::If(if_stmt) => {
                write!(f, "if {} ", if_stmt.condition)?;
                // Use fmt_indented for the then branch with base indentation
                if_stmt.then_branch.fmt_indented(f, 0, 4)?;
                
                if let Some(else_branch) = &if_stmt.else_branch {
                    write!(f, " else ")?;
                    if let StatementNode::Block(_) = **else_branch {
                        // If it's a block, use fmt_indented with base indentation
                        else_branch.fmt(f)?;
                    } else {
                        // For single-line else, just format it directly
                        write!(f, "{}", else_branch)?;
                    }
                }
                Ok(())
            }
            StatementNode::While(while_stmt) => {
                write!(f, "while {} ", while_stmt.condition)?;
                // Use fmt_indented for the loop body with base indentation
                while_stmt.body.fmt_indented(f, 0, 4)
            }
            StatementNode::For(for_stmt) => {
                write!(f, "for {} in {} ", for_stmt.var.name, for_stmt.iter)?;
                // Use fmt_indented for the loop body with base indentation
                for_stmt.body.fmt_indented(f, 0, 4)
            }
            StatementNode::Match(match_stmt) => {
                write!(f, "match {} {{", match_stmt.expr)?;
                for arm in &match_stmt.arms {
                    write!(f, " {} => {},", arm.pattern, arm.body)?;
                }
                write!(f, " }}")
            }
            StatementNode::Return(ret) => {
                if let Some(expr) = &ret.value {
                    write!(f, "return {};", expr)
                } else {
                    write!(f, "return;")
                }
            }
        }
    }
}

impl std::fmt::Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionNode::Statement(stmt) => write!(f, "{}", stmt),
            ExpressionNode::Struct(s) => {
                write!(f, "{} {{", s.type_name)?;
                for (i, field) in s.fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.value)?;
                }
                write!(f, "}}")
            }
            _ => todo!(),
        }
    }
}
