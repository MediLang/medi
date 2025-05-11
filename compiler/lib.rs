pub mod medic_ast;
pub mod medic_types;
pub mod medic_env;
pub mod medic_type;
pub mod medic_parser;
pub mod medic_lexer;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExpressionNode, LiteralNode};
    use crate::types::MediType;
    use crate::env::TypeEnv;
    use crate::type_checker::TypeChecker;

    #[test]
    fn test_type_env_insert_and_get() {
        let mut env = TypeEnv::new();
        env.insert("x".to_string(), MediType::Int);
        assert_eq!(env.get("x"), Some(&MediType::Int));
        assert_eq!(env.get("y"), None);
    }

    #[test]
    fn test_type_checker_literal() {
        let mut env = TypeEnv::new();
        let mut checker = TypeChecker::new(&mut env);
        let expr = ExpressionNode::Literal(LiteralNode::Int(42));
        assert_eq!(checker.check_expr(&expr), MediType::Int);
    }
}
