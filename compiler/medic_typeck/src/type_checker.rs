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

    /// Infers and returns the type of a given expression node in the Medic language.
    ///
    /// This method analyzes the provided expression node and determines its type according to Medic's type system rules. It supports literals, identifiers, binary operations (including arithmetic, comparison, logical, medical, range, unit conversion, bitwise, and null-coalescing operators), function calls, member access on structs, and healthcare-specific query expressions. If the type cannot be determined or is unsupported, `MediType::Unknown` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_ast::ast::{ExpressionNode, IdentifierNode};
    /// use medic_env::env::TypeEnv;
    /// use medic_type::types::MediType;
    /// use medic_typeck::type_checker::TypeChecker;
    ///
    /// let mut env = TypeEnv::new();
    /// env.insert("x".to_string(), MediType::Int);
    /// let mut checker = TypeChecker::new(&mut env);
    /// let expr = ExpressionNode::Identifier(IdentifierNode { name: "x".to_string() });
    /// assert_eq!(checker.check_expr(&expr), MediType::Int);
    /// ```
    pub fn check_expr(&mut self, expr: &ExpressionNode) -> MediType {
        match expr {
            ExpressionNode::IcdCode(_)
            | ExpressionNode::CptCode(_)
            | ExpressionNode::SnomedCode(_) => MediType::String,
            ExpressionNode::Identifier(name) => self
                .env
                .get(&name.name)
                .cloned()
                .unwrap_or(MediType::Unknown),
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
                    // Arithmetic operators
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Mod
                    | BinaryOperator::Shl
                    | BinaryOperator::Shr => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else if left.is_numeric() && right.is_numeric() {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Power operator (always returns float)
                    BinaryOperator::Pow => {
                        if left.is_numeric() && right.is_numeric() {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Comparison operators
                    BinaryOperator::Eq
                    | BinaryOperator::Ne
                    | BinaryOperator::Lt
                    | BinaryOperator::Gt
                    | BinaryOperator::Le
                    | BinaryOperator::Ge => {
                        if left.is_comparable_with(&right) {
                            MediType::Bool
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Logical operators
                    BinaryOperator::And | BinaryOperator::Or => {
                        if left == MediType::Bool && right == MediType::Bool {
                            MediType::Bool
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Medical operators
                    BinaryOperator::Of | BinaryOperator::Per => {
                        // These operators are used for medical quantities
                        // For now, we'll assume they return the type of the left operand
                        // More specific type checking can be added later
                        left
                    }
                    // Range operator
                    BinaryOperator::Range => {
                        if left == right {
                            MediType::Range(Box::new(left))
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Unit conversion operator
                    BinaryOperator::UnitConversion => {
                        // Check if both sides are unit types that can be converted
                        // For now, just return the right-hand type
                        right
                    }
                    // Bitwise operators
                    BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Null-coalescing and Elvis operators
                    BinaryOperator::NullCoalesce | BinaryOperator::Elvis => {
                        // Return the type of the right operand
                        right
                    }
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
                        .get(mem.property.name())
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
