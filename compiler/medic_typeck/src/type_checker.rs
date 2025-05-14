// Type checker for Medic language in Rust
// This is a basic structure; expand with more rules as needed

use medic_ast::ast::*;
use medic_env::env::TypeEnv;
use medic_type::types::*;

pub struct TypeChecker<'a> {
    env: &'a mut TypeEnv,
}

impl<'a> TypeChecker<'a> {
    pub fn new(env: &'a mut TypeEnv) -> Self {
        TypeChecker { env }
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) -> MediType {
        match expr {
            ExpressionNode::IcdCode(_)
            | ExpressionNode::CptCode(_)
            | ExpressionNode::SnomedCode(_) => MediType::String,
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
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Mod => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else if (left == MediType::Int || left == MediType::Float)
                            && (right == MediType::Int || right == MediType::Float)
                        {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    BinaryOperator::Eq
                    | BinaryOperator::Neq
                    | BinaryOperator::Lt
                    | BinaryOperator::Gt
                    | BinaryOperator::Le
                    | BinaryOperator::Ge => MediType::Bool,
                    BinaryOperator::And | BinaryOperator::Or => MediType::Bool,
                    BinaryOperator::Range => {
                        if left == right {
                            MediType::Range(Box::new(left))
                        } else {
                            MediType::Unknown
                        }
                    }
                    BinaryOperator::Assign => left, // Assignment returns the type of the left side
                }
            }
            ExpressionNode::Call(call) => {
                let callee_type = self.check_expr(&call.callee);
                if let MediType::Function {
                    params,
                    return_type,
                } = callee_type
                {
                    if params.len() == call.arguments.len() {
                        // Check argument types
                        for (arg, param_type) in call.arguments.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg);
                            if arg_type != *param_type {
                                return MediType::Unknown;
                            }
                        }
                        *return_type
                    } else {
                        MediType::Unknown
                    }
                } else {
                    MediType::Unknown
                }
            }
            ExpressionNode::Member(mem) => {
                // Implement member access type rules
                let object_type = self.check_expr(&mem.object);
                match object_type {
                    MediType::Struct(ref fields) => fields
                        .get(&mem.property)
                        .cloned()
                        .unwrap_or(MediType::Unknown),
                    _ => MediType::Unknown, // Member access is only valid on structs
                }
            }
            ExpressionNode::HealthcareQuery(query) => {
                // Implement healthcare query type rules
                match query.query_type.as_str() {
                    "PatientData" => MediType::Record(vec![
                        ("id".to_string(), MediType::Int),
                        ("name".to_string(), MediType::String),
                        ("age".to_string(), MediType::Int),
                        (
                            "conditions".to_string(),
                            MediType::List(Box::new(MediType::String)),
                        ),
                    ]),
                    "AppointmentData" => MediType::List(Box::new(MediType::Record(vec![
                        ("appointment_id".to_string(), MediType::Int),
                        ("date".to_string(), MediType::String),
                        ("doctor".to_string(), MediType::String),
                    ]))),
                    _ => MediType::Unknown, // Fallback for unsupported query types
                }
            }
        }
    }
}
