//! Example of using the visitor pattern to traverse and process the AST.

use medic_ast::ast::*;
use medic_ast::visit::*;

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
    fn visit_children<T: Visitable>(&mut self, node: &T) -> VisitResult<Self::Output> {
        node.visit_children(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use medic_ast::ast::*;
    
    fn create_sample_ast() -> ProgramNode {
        // let x = 1 + 2 * 3;
        // if x > 5 {
        //     println("x is greater than 5");
        // }
        ProgramNode {
            statements: vec![
                StatementNode::Let(Box::new(LetStatementNode {
                    name: IdentifierNode { name: "x".to_string() },
                    value: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left: ExpressionNode::Literal(LiteralNode::Int(1)),
                        operator: BinaryOperator::Add,
                        right: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left: ExpressionNode::Literal(LiteralNode::Int(2)),
                            operator: BinaryOperator::Multiply,
                            right: ExpressionNode::Literal(LiteralNode::Int(3)),
                        })),
                    })),
                })),
                StatementNode::If(Box::new(IfNode {
                    condition: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left: ExpressionNode::Identifier(IdentifierNode { name: "x".to_string() }),
                        operator: BinaryOperator::GreaterThan,
                        right: ExpressionNode::Literal(LiteralNode::Int(5)),
                    })),
                    then_branch: BlockNode {
                        statements: vec![
                            StatementNode::Expression(ExpressionNode::Call(Box::new(CallExpressionNode {
                                callee: ExpressionNode::Identifier(IdentifierNode { name: "println".to_string() }),
                                arguments: vec![
                                    ExpressionNode::Literal(LiteralNode::String("x is greater than 5".to_string())),
                                ],
                            }))),
                        ],
                    },
                    else_branch: None,
                })),
            ],
        }
    }
    
    #[test]
    fn test_node_counter() {
        let ast = create_sample_ast();
        let mut counter = NodeCounter::default();
        ast.accept(&mut counter).unwrap();
        
        assert_eq!(counter.expression_count, 5); // 1+2*3, x>5, println, "x is greater than 5"
        assert_eq!(counter.statement_count, 2);  // let, if
        assert_eq!(counter.identifier_count, 3);  // x, x, println
        assert_eq!(counter.literal_count, 4);     // 1, 2, 3, "x is greater than 5"
        assert_eq!(counter.binary_op_count, 2);   // +, * in 1+2*3, > in x>5
    }
    
    #[test]
    fn test_variable_collector() {
        let ast = create_sample_ast();
        let mut collector = VariableCollector::new();
        ast.accept(&mut collector).unwrap();
        
        assert_eq!(collector.variables, vec!["x", "x", "println"]);
    }
    
    #[test]
    fn test_ast_printer() {
        let ast = create_sample_ast();
        let mut printer = AstPrinter::new();
        ast.accept(&mut printer).unwrap();
        let output = printer.into_inner();
        
        // Just check that the output contains some expected strings
        assert!(output.contains("Let statement"));
        assert!(output.contains("Binary: +"));
        assert!(output.contains("Binary: *"));
        assert!(output.contains("If statement"));
        assert!(output.contains("x is greater than 5"));
    }
}
