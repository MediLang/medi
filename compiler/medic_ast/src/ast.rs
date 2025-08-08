//! Abstract Syntax Tree (AST) definitions for the Medi language in Rust
//!
//! This module defines the AST nodes used to represent Medi programs, along with
//! implementations for visiting, displaying, and serializing the AST.

use crate::visit::{VisitResult, Visitable, Visitor};
use serde::{Deserialize, Serialize};

/// Implement Visitable for String to handle string literals
impl Visitable for String {
    fn accept<V: Visitor + ?Sized>(&self, _visitor: &mut V) -> VisitResult<V::Output> {
        // For string literals, just return the default output
        Ok(Default::default())
    }
}

/// Represents an expression in the AST
use crate::visit::Span;

/// A node in the AST with source location information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Spanned<T> {
    /// The actual node value
    pub node: T,
    /// The source code span this node was parsed from
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned node
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }

    /// Map the inner value while preserving the span
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Combine two spans into a single span that covers both
    pub fn combine<U>(first: &Spanned<T>, second: &Spanned<U>) -> Span {
        let start = std::cmp::min(first.span.start, second.span.start);
        let end = std::cmp::max(first.span.end, second.span.end);
        let line = first.span.line; // Use the line from the first span
        let column = first.span.column; // Use the column from the first span

        Span {
            start,
            end,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum ExpressionNode {
    /// An identifier (variable name, function name, etc.)
    Identifier(Spanned<IdentifierNode>),
    /// An ICD code literal
    IcdCode(Spanned<String>),
    /// A CPT code literal
    CptCode(Spanned<String>),
    /// A SNOMED CT code literal
    SnomedCode(Spanned<String>),
    /// A literal value (number, string, boolean, etc.)
    Literal(Spanned<LiteralNode>),
    /// A binary operation (e.g., 1 + 2, x * y)
    Binary(Spanned<Box<BinaryExpressionNode>>),
    /// A function call (e.g., add(1, 2))
    Call(Spanned<Box<CallExpressionNode>>),
    /// A member access (e.g., object.property)
    Member(Spanned<Box<MemberExpressionNode>>),
    /// A healthcare-specific query
    HealthcareQuery(Spanned<Box<HealthcareQueryNode>>),
    /// A statement expression (e.g., a block expression)
    Statement(Spanned<Box<StatementNode>>),
    /// A struct literal (e.g., `Patient { id: 1, name: "John" }`)
    Struct(Spanned<Box<StructLiteralNode>>),
    /// An array literal (e.g., `[1, 2, 3]`)
    Array(Spanned<Box<ArrayLiteralNode>>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
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
            LiteralNode::Int(i) => write!(f, "{i}"),
            LiteralNode::Float(fl) => write!(f, "{fl}"),
            LiteralNode::Bool(b) => write!(f, "{b}"),
            LiteralNode::String(s) => write!(f, "\"{s}\""),
        }
    }
}

impl Visitable for LiteralNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_literal(self)
    }
}

impl Visitable for BinaryExpressionNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_binary_expr(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.left.accept(visitor)?;
        self.right.accept(visitor)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpressionNode {
    pub left: ExpressionNode,
    pub operator: BinaryOperator,
    pub right: ExpressionNode,
}

/// Binary operators in order of increasing precedence
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

    // Arithmetic (left-associative)
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    // Unit conversion (right-associative, highest precedence)
    UnitConversion, // → (e.g., '5 mg→g')

    // Bitwise operations (left-associative)
    BitOr,  // |
    BitXor, // ^
    BitAnd, // &
    Shl,    // <<
    Shr,    // >>

    // Exponentiation (right-associative)
    Pow, // **

    // Range (right-associative)
    Range, // ..

    // Null-coalescing (right-associative)
    NullCoalesce, // ??

    // Elvis operator (right-associative)
    Elvis, // ?:
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Assign => write!(f, "="),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Of => write!(f, "of"),
            BinaryOperator::Per => write!(f, "per"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::Ne => write!(f, "!="),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::Le => write!(f, "<="),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::Ge => write!(f, ">="),
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::UnitConversion => write!(f, "→"),
            BinaryOperator::BitOr => write!(f, "|"),
            BinaryOperator::BitXor => write!(f, "^"),
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::Shl => write!(f, "<<"),
            BinaryOperator::Shr => write!(f, ">>"),
            BinaryOperator::Pow => write!(f, "**"),
            BinaryOperator::Range => write!(f, ".."),
            BinaryOperator::NullCoalesce => write!(f, "??"),
            BinaryOperator::Elvis => write!(f, "?:"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallExpressionNode {
    pub callee: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MemberExpressionNode {
    pub object: ExpressionNode,
    pub property: IdentifierNode,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HealthcareQueryNode {
    pub query_type: String,
    pub arguments: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetStatementNode {
    pub name: IdentifierNode,
    pub value: ExpressionNode,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssignmentNode {
    pub target: ExpressionNode, // Usually Identifier or Member
    pub value: ExpressionNode,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlockNode {
    pub statements: Vec<StatementNode>,
    pub span: Span,
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
        indent_size: usize,
    ) -> std::fmt::Result {
        let indent = " ".repeat(indent_level * indent_size);
        let stmt_indent = " ".repeat((indent_level + 1) * indent_size);

        writeln!(f, "{indent}{{")?;
        for stmt in &self.statements {
            // Handle nested blocks by passing increased indentation
            if let StatementNode::Block(block) = stmt {
                block.fmt_indented(f, indent_level + 1, indent_size)?;
            } else {
                write!(f, "{stmt_indent}{stmt}; ")?;
            }
            writeln!(f)?;
        }
        write!(f, "{indent}}}")
    }
}

impl std::fmt::Display for BlockNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_indented(f, 0, 4) // Default to 4-space indentation, starting at level 0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfNode {
    pub condition: ExpressionNode,
    pub then_branch: BlockNode,
    pub else_branch: Option<Box<StatementNode>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhileNode {
    pub condition: ExpressionNode,
    pub body: BlockNode,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForNode {
    pub variable: IdentifierNode,
    pub iterable: ExpressionNode,
    pub body: BlockNode,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchNode {
    pub expr: Box<ExpressionNode>,
    pub arms: Vec<MatchArmNode>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReturnNode {
    pub value: Option<ExpressionNode>,
    pub span: Span,
}

use std::hash::Hash;

/// Represents a field in a struct literal
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub value: ExpressionNode,
}

/// Represents a struct literal expression (e.g., `Patient { id: 1, name: "John" }`)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructLiteralNode {
    pub type_name: String,
    pub fields: Vec<StructField>,
}

/// Represents an array literal expression (e.g., `[1, 2, 3]`)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayLiteralNode {
    pub elements: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum PatternNode {
    Literal(LiteralNode),       // Pattern can be a literal
    Identifier(IdentifierNode), // Pattern can be an identifier
    Wildcard,                   // Represents the '_' pattern
    Variant {
        // Represents a variant pattern like Some(x)
        name: String,            // The variant name (e.g., "Some")
        inner: Box<PatternNode>, // The inner pattern (e.g., the x in Some(x))
    },
    Struct {
        // Represents a struct pattern like Point { x, y }
        type_name: String,               // The struct type name
        fields: Vec<StructFieldPattern>, // The field patterns
    },
}

/// Represents a field pattern in a struct pattern
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructFieldPattern {
    pub name: String,         // The field name
    pub pattern: PatternNode, // The pattern for this field
}

impl std::fmt::Display for PatternNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternNode::Literal(lit) => write!(f, "{lit}"),
            PatternNode::Identifier(ident) => write!(f, "{}", ident.name),
            PatternNode::Wildcard => write!(f, "_"),
            PatternNode::Variant { name, inner } => write!(f, "{name}({inner})"),
            PatternNode::Struct { type_name, fields } => {
                write!(f, "{type_name} {{ ")?;
                let fields_str = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.pattern))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{fields_str}}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArmNode {
    pub pattern: PatternNode,
    pub body: Box<ExpressionNode>, // Body of the arm, could also be a BlockNode in more complex scenarios
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProgramNode {
    pub statements: Vec<StatementNode>,
}

// Implement Visitable for all AST nodes

impl Visitable for ExpressionNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        match self {
            ExpressionNode::Binary(Spanned { node: expr, .. }) => expr.accept(visitor),
            ExpressionNode::Call(Spanned { node: expr, .. }) => expr.accept(visitor),
            ExpressionNode::Member(Spanned { node: expr, .. }) => expr.accept(visitor),
            ExpressionNode::Literal(Spanned { node: lit, .. }) => lit.accept(visitor),
            ExpressionNode::Identifier(Spanned { node: ident, .. }) => ident.accept(visitor),
            ExpressionNode::HealthcareQuery(Spanned { node: query, .. }) => query.accept(visitor),
            ExpressionNode::Struct(Spanned { node: lit, .. }) => lit.accept(visitor),
            ExpressionNode::Array(Spanned { node: arr, .. }) => arr.accept(visitor),
            ExpressionNode::IcdCode(spanned) => spanned.node.accept(visitor),
            ExpressionNode::CptCode(spanned) => spanned.node.accept(visitor),
            ExpressionNode::SnomedCode(spanned) => spanned.node.accept(visitor),
            ExpressionNode::Statement(spanned) => spanned.node.accept(visitor),
        }
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        match self {
            ExpressionNode::Binary(Spanned { node: expr, .. }) => expr.visit_children(visitor),
            ExpressionNode::Call(Spanned { node: expr, .. }) => expr.visit_children(visitor),
            ExpressionNode::Member(Spanned { node: expr, .. }) => expr.visit_children(visitor),
            ExpressionNode::Literal(Spanned { node: lit, .. }) => lit.visit_children(visitor),
            ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                ident.visit_children(visitor)
            }
            ExpressionNode::HealthcareQuery(Spanned { node: query, .. }) => {
                query.visit_children(visitor)
            }
            ExpressionNode::Struct(Spanned { node: lit, .. }) => lit.visit_children(visitor),
            ExpressionNode::Array(Spanned { node: arr, .. }) => arr.visit_children(visitor),
            ExpressionNode::IcdCode(spanned) => spanned.node.visit_children(visitor),
            ExpressionNode::CptCode(spanned) => spanned.node.visit_children(visitor),
            ExpressionNode::SnomedCode(spanned) => spanned.node.visit_children(visitor),
            ExpressionNode::Statement(spanned) => spanned.node.visit_children(visitor),
        }
    }
}

impl Visitable for CallExpressionNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_call_expr(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.callee.accept(visitor)?;
        for arg in &self.arguments {
            arg.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for MemberExpressionNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_member_expr(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.object.accept(visitor)?;
        self.property.accept(visitor)
    }
}

impl Visitable for HealthcareQueryNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_healthcare_query(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        for arg in &self.arguments {
            arg.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for StatementNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        match self {
            StatementNode::Let(stmt) => stmt.accept(visitor),
            StatementNode::Assignment(stmt) => stmt.accept(visitor),
            StatementNode::Expr(expr) => expr.accept(visitor),
            StatementNode::Block(block) => block.accept(visitor),
            StatementNode::If(stmt) => stmt.accept(visitor),
            StatementNode::While(stmt) => stmt.accept(visitor),
            StatementNode::For(stmt) => stmt.accept(visitor),
            StatementNode::Match(stmt) => stmt.accept(visitor),
            StatementNode::Return(stmt) => stmt.accept(visitor),
        }
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        match self {
            StatementNode::Let(stmt) => stmt.visit_children(visitor),
            StatementNode::Assignment(stmt) => stmt.visit_children(visitor),
            StatementNode::Expr(expr) => expr.visit_children(visitor),
            StatementNode::Block(block) => block.visit_children(visitor),
            StatementNode::If(stmt) => stmt.visit_children(visitor),
            StatementNode::While(stmt) => stmt.visit_children(visitor),
            StatementNode::For(stmt) => stmt.visit_children(visitor),
            StatementNode::Match(stmt) => stmt.visit_children(visitor),
            StatementNode::Return(stmt) => stmt.visit_children(visitor),
        }
    }
}

impl StatementNode {
    /// Returns the span of this statement node
    pub fn span(&self) -> &Span {
        match self {
            StatementNode::Let(stmt) => &stmt.span,
            StatementNode::Assignment(stmt) => &stmt.span,
            StatementNode::Expr(expr) => expr.span(),
            StatementNode::Block(block) => &block.span,
            StatementNode::If(stmt) => &stmt.span,
            StatementNode::While(stmt) => &stmt.span,
            StatementNode::For(stmt) => &stmt.span,
            StatementNode::Match(stmt) => &stmt.span,
            StatementNode::Return(stmt) => &stmt.span,
        }
    }
}

impl Visitable for LetStatementNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_let_stmt(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.name.accept(visitor)?;
        self.value.accept(visitor)
    }
}

impl Visitable for AssignmentNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_assignment(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.target.accept(visitor)?;
        self.value.accept(visitor)
    }
}

impl Visitable for BlockNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_block(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        for stmt in &self.statements {
            stmt.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for IfNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_if_stmt(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.condition.accept(visitor)?;
        self.then_branch.accept(visitor)?;
        if let Some(else_branch) = &self.else_branch {
            else_branch.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for WhileNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_while_loop(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.condition.accept(visitor)?;
        self.body.accept(visitor)
    }
}

impl Visitable for ForNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_for_loop(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.variable.accept(visitor)?;
        self.iterable.accept(visitor)?;
        self.body.accept(visitor)
    }
}

impl Visitable for MatchNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_match(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.expr.accept(visitor)?;
        for arm in &self.arms {
            arm.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for ReturnNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_return(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        if let Some(expr) = &self.value {
            expr.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for StructField {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.value.accept(visitor)
    }
}

impl Visitable for StructLiteralNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_struct_literal(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        for field in &self.fields {
            field.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for ArrayLiteralNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_array_literal(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        for el in &self.elements {
            el.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

impl Visitable for IdentifierNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_identifier(self)
    }
}

impl Visitable for PatternNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_pattern(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        match self {
            PatternNode::Literal(lit) => lit.accept(visitor),
            PatternNode::Identifier(ident) => ident.accept(visitor),
            PatternNode::Wildcard => Ok(Default::default()),
            PatternNode::Variant { inner, .. } => inner.accept(visitor),
            PatternNode::Struct { fields, .. } => {
                for field in fields {
                    field.pattern.accept(visitor)?;
                }
                Ok(Default::default())
            }
        }
    }
}

impl Visitable for MatchArmNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_match_arm(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        self.pattern.accept(visitor)?;
        self.body.accept(visitor)
    }
}

impl Visitable for ProgramNode {
    fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        visitor.visit_program(self)
    }

    fn visit_children<V: Visitor + ?Sized>(&self, visitor: &mut V) -> VisitResult<V::Output> {
        for stmt in &self.statements {
            stmt.accept(visitor)?;
        }
        Ok(Default::default())
    }
}

// Add more AST nodes as needed (statements, declarations, etc.)

impl ExpressionNode {
    /// Returns the span of this expression node
    pub fn span(&self) -> &Span {
        match self {
            ExpressionNode::Identifier(span) => &span.span,
            ExpressionNode::IcdCode(span) => &span.span,
            ExpressionNode::CptCode(span) => &span.span,
            ExpressionNode::SnomedCode(span) => &span.span,
            ExpressionNode::Literal(span) => &span.span,
            ExpressionNode::Binary(span) => &span.span,
            ExpressionNode::Call(span) => &span.span,
            ExpressionNode::Member(span) => &span.span,
            ExpressionNode::HealthcareQuery(span) => &span.span,
            ExpressionNode::Statement(span) => &span.span,
            ExpressionNode::Struct(span) => &span.span,
            ExpressionNode::Array(span) => &span.span,
        }
    }

    /// Creates an expression from a statement
    ///
    /// If the statement is an expression statement, returns the inner expression.
    /// Otherwise, wraps the statement in a `Statement` variant.
    pub fn from_statement(stmt: StatementNode) -> Self {
        match stmt {
            StatementNode::Expr(expr) => expr,
            _ => ExpressionNode::Statement(Spanned::new(Box::new(stmt), Span::default())),
        }
    }
}

impl std::fmt::Display for StatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StatementNode::Let(stmt) => write!(f, "let {} = {};", stmt.name.name, stmt.value),
            StatementNode::Assignment(assign) => write!(f, "{} = {};", assign.target, assign.value),
            StatementNode::Expr(expr) => write!(f, "{expr};"),
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
                    // Format the else branch directly without trying to match on its type
                    else_branch.fmt(f)?;
                }
                Ok(())
            }
            StatementNode::While(while_stmt) => {
                write!(f, "while {} ", while_stmt.condition)?;
                // Use fmt_indented for the loop body with base indentation
                while_stmt.body.fmt_indented(f, 0, 4)
            }
            StatementNode::For(for_stmt) => {
                write!(
                    f,
                    "for {} in {} ",
                    for_stmt.variable.name, for_stmt.iterable
                )?;
                write!(f, "{}", for_stmt.body)
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
                    write!(f, "return {expr};")
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
            ExpressionNode::Identifier(Spanned { node: ident, .. }) => write!(f, "{}", ident.name),
            ExpressionNode::IcdCode(Spanned { node: code, .. }) => write!(f, "ICD:{code}"),
            ExpressionNode::CptCode(Spanned { node: code, .. }) => write!(f, "CPT:{code}"),
            ExpressionNode::SnomedCode(Spanned { node: code, .. }) => write!(f, "SNOMED:{code}"),
            ExpressionNode::Literal(Spanned { node: lit, .. }) => write!(f, "{lit}"),
            ExpressionNode::Binary(Spanned { node: expr, .. }) => {
                write!(f, "({} {} {})", expr.left, expr.operator, expr.right)
            }
            ExpressionNode::Call(Spanned { node: call, .. }) => {
                write!(f, "{}", call.callee)?;
                write!(f, "(")?;
                for (i, arg) in call.arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            ExpressionNode::Member(Spanned { node: member, .. }) => {
                write!(f, "{}.{}", member.object, member.property.name)
            }
            ExpressionNode::HealthcareQuery(Spanned { node: query, .. }) => {
                write!(f, "{}(", query.query_type)?;
                for (i, arg) in query.arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            ExpressionNode::Statement(Spanned { node: stmt, .. }) => write!(f, "{stmt}"),
            ExpressionNode::Struct(Spanned { node: s, .. }) => {
                write!(f, "{} {{", s.type_name)?;
                for (i, field) in s.fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.value)?;
                }
                write!(f, "}}")
            }
            ExpressionNode::Array(Spanned { node: a, .. }) => {
                write!(f, "[")?;
                for (i, el) in a.elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el}")?;
                }
                write!(f, "]")
            }
        }
    }
}
