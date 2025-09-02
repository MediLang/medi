use medic_ast::ast::*;
use medic_ast::visit::Span;
use medic_env::env::TypeEnv;
use medic_typeck::type_checker::{TypeChecker, TypeError};

#[test]
fn annotated_let_type_mismatch_reports_error() {
    // Build: let x: String = 1;
    let span = Span::default();
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("String"),
        span,
    ));
    let val = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), span));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("x"),
        type_annotation: Some(ann),
        value: Some(val),
        span,
    }));

    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);

    let err = tc.check_stmt(&stmt).unwrap_err();
    match err {
        TypeError::TypeMismatch { expected, found } => {
            assert_eq!(expected, medic_type::types::MediType::String);
            assert_eq!(found, medic_type::types::MediType::Int);
        }
        other => panic!("expected TypeMismatch, got {other:?}"),
    }
}

#[test]
fn annotated_let_with_unknown_type_reports_error() {
    // Build: let y: Foo;
    let span = Span::default();
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("Foo"),
        span,
    ));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("y"),
        type_annotation: Some(ann),
        value: None,
        span,
    }));

    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);

    let err = tc.check_stmt(&stmt).unwrap_err();
    match err {
        TypeError::UnknownTypeName(name) => assert_eq!(name, "Foo"),
        other => panic!("expected UnknownTypeName, got {other:?}"),
    }
}
