// Type checker for Medi language in Rust
// This is a basic structure; expand with more rules as needed

use crate::ast::*;
use crate::types::*;
use crate::env::TypeEnv;

pub struct TypeChecker<'a> {
    env: &'a mut TypeEnv,
}

impl<'a> TypeChecker<'a> {
    pub fn new(env: &'a mut TypeEnv) -> Self {
        TypeChecker { env }
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) -> MediType {
        match expr {
            ExpressionNode::Identifier(name) => {
                self.env.get(name).cloned().unwrap_or(MediType::Unknown)
            }
            ExpressionNode::Literal(lit) => match lit {
                LiteralNode::Int(_) => MediType::Int,
                LiteralNode::Float(_) => MediType::Float,
                LiteralNode::Bool(_) => MediType::Bool,
                LiteralNode::String(_) => MediType::String,
            },
            ExpressionNode::Binary(bin) => {
                let left = self.check_expr(&bin.left);
                let right = self.check_expr(&bin.right);
                match bin.operator {
                    BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else if (left == MediType::Int || left == MediType::Float) && (right == MediType::Int || right == MediType::Float) {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    BinaryOperator::Eq | BinaryOperator::Neq | BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Le | BinaryOperator::Ge => {
                        MediType::Bool
                    }
                    BinaryOperator::And | BinaryOperator::Or => MediType::Bool,
                    BinaryOperator::Assign => left, // Assignment returns the type of the left side
                }
            }
            ExpressionNode::Call(call) => {
                let callee_type = self.check_expr(&call.callee);
                if let MediType::Function { params, return_type } = callee_type {
                    if params.len() == call.arguments.len() {
                        // TODO: Check argument types
                        *return_type
                    } else {
                        MediType::Unknown
                    }
                } else {
                    MediType::Unknown
                }
            }
            ExpressionNode::Member(mem) => {
                // TODO: Implement member access type rules
                MediType::Unknown
            }
            ExpressionNode::HealthcareQuery(_) => {
                // TODO: Implement healthcare query type rules
                MediType::Unknown
            }
        }
    }
}
