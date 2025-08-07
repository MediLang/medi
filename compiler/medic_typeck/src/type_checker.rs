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
            ExpressionNode::IcdCode(Spanned { node: _, .. })
            | ExpressionNode::CptCode(Spanned { node: _, .. })
            | ExpressionNode::SnomedCode(Spanned { node: _, .. }) => MediType::String,
            ExpressionNode::Identifier(Spanned { node: name, .. }) => self
                .env
                .get(&name.name)
                .cloned()
                .unwrap_or(MediType::Unknown),
            ExpressionNode::Literal(Spanned { node: lit, .. }) => match lit {
                LiteralNode::Int(_) => MediType::Int,
                LiteralNode::Float(_) => MediType::Float,
                LiteralNode::Bool(_) => MediType::Bool,
                LiteralNode::String(_) => MediType::String,
            },
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
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
                    // Assignment operator
                    BinaryOperator::Assign => {
                        // The type of an assignment is the type of the right-hand side
                        // First check if the left-hand side is a valid lvalue (Identifier or Member)
                        if let ExpressionNode::Binary(Spanned { node: bin_expr, .. }) = expr {
                            match &bin_expr.left {
                                ExpressionNode::Identifier(_) | ExpressionNode::Member(_) => {}
                                _ => {
                                    // Not a valid lvalue
                                    log::error!("Left side of assignment must be an identifier or member expression");
                                    return MediType::Unknown;
                                }
                            }
                        } else {
                            // This should never happen for a binary expression with assign operator
                            log::error!(
                                "Internal error: Expected binary expression for assignment"
                            );
                            return MediType::Unknown;
                        }

                        // TODO: Check if the types are compatible
                        // For now, just return the right-hand type
                        right
                    }
                }
            }
            ExpressionNode::Call(Spanned { node: call, .. }) => {
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
            ExpressionNode::Member(Spanned { node: mem, .. }) => {
                let object_type = self.check_expr(&mem.object);
                if let MediType::Struct(fields) = object_type {
                    fields
                        .get(mem.property.name())
                        .cloned()
                        .unwrap_or(MediType::Unknown)
                } else {
                    MediType::Unknown // Member access is only valid on structs
                }
            }
            ExpressionNode::HealthcareQuery(Spanned { node: query, .. }) => {
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
            ExpressionNode::Statement(Spanned { node: stmt, .. }) => {
                // For statement expressions, check the inner statement
                match &**stmt {
                    StatementNode::Expr(expr) => self.check_expr(expr),
                    _ => MediType::Void, // Other statements don't produce values
                }
            }
            ExpressionNode::Struct(Spanned {
                node: struct_lit, ..
            }) => {
                // For struct literals, we return a struct type with field types
                // TODO: Look up the actual struct definition for more precise type checking
                let mut fields = std::collections::HashMap::new();
                for field in &struct_lit.fields {
                    let field_type = self.check_expr(&field.value);
                    fields.insert(field.name.clone(), field_type);
                }
                MediType::Struct(fields)
            }
        }
    }
}
