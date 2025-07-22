//! Example of using the visitor pattern to traverse and process the AST.

use std::collections::HashSet;
use medic_ast::ast::*;
use medic_ast::visit::*;
use medic_ast::Spanned;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create a sample AST
    let ast = create_sample_ast();
    
    // Count nodes in the AST
    let mut counter = NodeCounter::default();
    ast.accept(&mut counter)?;
    
    println!("AST Node Counts:");
    println!("  Statements: {}", counter.statement_count);
    println!("  Identifiers: {}", counter.identifier_count);
    println!("  Literals: {}", counter.literal_count);
    println!("  Binary Operations: {}", counter.binary_op_count);
    
    // Collect variables
    let mut collector = VariableCollector::new();
    ast.accept(&mut collector)?;
    
    println!("\nVariables used:");
    for var in &collector.variables {
        println!("  {}", var);
    }
    
    // Print the AST
    println!("\nPretty-printed AST:");
    let mut printer = AstPrinter::new();
    ast.accept(&mut printer)?;
    println!("{}", printer.into_inner());
    
    Ok(())
}

/// A visitor that counts the number of nodes of each type in the AST.
#[derive(Default)]
pub struct NodeCounter {
    pub expression_count: usize,
    pub statement_count: usize,
    pub identifier_count: usize,
    pub literal_count: usize,
    pub binary_op_count: usize,
}

impl Visitor for NodeCounter {
    type Output = ();
    
    fn visit_identifier(&mut self, _node: &IdentifierNode) -> VisitResult<Self::Output> {
        self.identifier_count += 1;
        Ok(())
    }
    
    fn visit_literal(&mut self, _node: &LiteralNode) -> VisitResult<Self::Output> {
        self.literal_count += 1;
        Ok(())
    }
    
    fn visit_binary_expr(&mut self, node: &BinaryExpressionNode) -> VisitResult<Self::Output> {
        self.binary_op_count += 1;
        self.visit_children(node)
    }
    
    fn visit_let_stmt(&mut self, node: &LetStatementNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_assignment(&mut self, node: &AssignmentNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_block(&mut self, node: &BlockNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }
    
    fn visit_if_stmt(&mut self, node: &IfNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_while_loop(&mut self, node: &WhileNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_for_loop(&mut self, node: &ForNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_match(&mut self, node: &MatchNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_return(&mut self, node: &ReturnNode) -> VisitResult<Self::Output> {
        self.statement_count += 1;
        self.visit_children(node)
    }
    
    fn visit_call_expr(&mut self, node: &CallExpressionNode) -> VisitResult<Self::Output> {
        self.expression_count += 1;
        self.visit_children(node)
    }
    
    fn visit_member_expr(&mut self, node: &MemberExpressionNode) -> VisitResult<Self::Output> {
        self.expression_count += 1;
        self.visit_children(node)
    }
    
    fn visit_healthcare_query(&mut self, node: &HealthcareQueryNode) -> VisitResult<Self::Output> {
        self.expression_count += 1;
        self.visit_children(node)
    }
    
    fn visit_struct_literal(&mut self, node: &StructLiteralNode) -> VisitResult<Self::Output> {
        self.expression_count += 1;
        self.visit_children(node)
    }
    
    fn visit_program(&mut self, node: &ProgramNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }
    
    fn visit_pattern(&mut self, node: &PatternNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }
    
    fn visit_match_arm(&mut self, node: &MatchArmNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }
}

/// A visitor that collects all variable names in the AST.
pub struct VariableCollector {
    pub variables: Vec<String>,
}

impl VariableCollector {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
        }
    }
}

impl Visitor for VariableCollector {
    type Output = ();
    
    fn visit_identifier(&mut self, node: &IdentifierNode) -> VisitResult<Self::Output> {
        self.variables.push(node.name.clone());
        Ok(())
    }
    
    fn visit_let_stmt(&mut self, node: &LetStatementNode) -> VisitResult<Self::Output> {
        // Don't visit the name node directly to avoid double-counting
        node.value.accept(self)
    }
}

/// A visitor that pretty-prints the AST.
pub struct AstPrinter {
    indent: usize,
    output: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            output: String::new(),
        }
    }
    
    pub fn into_inner(self) -> String {
        self.output
    }
    
    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }
    
    fn write_line(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }
    
    fn with_indent<F>(&mut self, f: F) -> VisitResult<()>
    where
        F: FnOnce(&mut Self) -> VisitResult<()>,
    {
        self.indent += 1;
        let result = f(self);
        self.indent -= 1;
        result
    }
}

impl Visitor for AstPrinter {
    type Output = ();
    
    fn visit_identifier(&mut self, node: &IdentifierNode) -> VisitResult<Self::Output> {
        self.write_line(&format!("Identifier: {}", node.name));
        Ok(())
    }
    
    fn visit_literal(&mut self, node: &LiteralNode) -> VisitResult<Self::Output> {
        match node {
            LiteralNode::Int(n) => self.write_line(&format!("Literal: {}", n)),
            LiteralNode::Float(f) => self.write_line(&format!("Literal: {}", f)),
            LiteralNode::Bool(b) => self.write_line(&format!("Literal: {}", b)),
            LiteralNode::String(s) => self.write_line(&format!("Literal: \"{}\"", s)),
        }
        Ok(())
    }
    
    fn visit_binary_expr(&mut self, node: &BinaryExpressionNode) -> VisitResult<Self::Output> {
        self.write_line(&format!("Binary: {}", node.operator));
        self.with_indent(|this| {
            this.write_line("Left:");
            this.with_indent(|this| node.left.accept(this))?;
            this.write_line("Right:");
            this.with_indent(|this| node.right.accept(this))
        })
    }
    
    fn visit_let_stmt(&mut self, node: &LetStatementNode) -> VisitResult<Self::Output> {
        self.write_line("Let statement:");
        self.with_indent(|this| {
            this.write_line(&format!("Name: {}", node.name.name));
            this.write_line("Value:");
            this.with_indent(|this| node.value.accept(this))
        })
    }
    
    fn visit_block(&mut self, node: &BlockNode) -> VisitResult<Self::Output> {
        self.write_line("Block:");
        self.with_indent(|this| {
            for stmt in &node.statements {
                stmt.accept(this)?;
            }
            Ok(())
        })
    }
    
    fn visit_program(&mut self, node: &ProgramNode) -> VisitResult<Self::Output> {
        self.write_line("Program:");
        self.with_indent(|this| {
            for stmt in &node.statements {
                stmt.accept(this)?;
            }
            Ok(())
        })
    }
    
    // Default implementation for other node types
    fn visit_children<T: ?Sized + Visitable>(&mut self, node: &T) -> VisitResult<Self::Output> {
        node.visit_children(self)
    }
}



fn create_sample_ast() -> ProgramNode {
    let span = Span { start: 0, end: 0, line: 1, column: 1 };
    
    // let x = 42;
    // let y = x + 1;
    // y
    
    let stmt1 = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode { name: "x".to_string() },
        value: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(42), span)),
    }));
    
    let stmt2 = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode { name: "y".to_string() },
        value: ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Identifier(Spanned::new(IdentifierNode { name: "x".to_string() }, span)),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), span)),
            }),
            span
        )),
    }));
    
    let stmt3 = StatementNode::Expr(ExpressionNode::Identifier(Spanned::new(
        IdentifierNode { name: "y".to_string() },
        span
    )));
    
    ProgramNode {
        statements: vec![stmt1, stmt2, stmt3],
    }
}
