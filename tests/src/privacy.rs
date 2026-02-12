use tlvxc_ast::ast::*;
use tlvxc_ast::visit::Span;
use tlvxc_env::env::TypeEnv;
use tlvxc_type::types::PrivacyAnnotation;
use tlvxc_typeck::type_checker::TypeChecker;

#[test]
fn privacy_anonymized_literal_sets_env_and_side_table() {
    // let a = "anon";  String literal is Anonymized
    let let_span = Span {
        start: 600,
        end: 620,
        line: 20,
        column: 1,
    };
    let val_span = Span {
        start: 610,
        end: 616,
        line: 20,
        column: 11,
    };
    let val = ExpressionNode::Literal(Spanned::new(
        LiteralNode::String("anon".to_string()),
        val_span,
    ));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("a"),
        type_annotation: None,
        value: Some(val),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let side_priv = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt).expect("anonymized literal ok");
        tc.get_privacy_at_span(&val_span).cloned()
    };

    // Env privacy set from initializer
    assert_eq!(env.get_privacy("a"), Some(PrivacyAnnotation::Anonymized));
    // Side privacy table recorded for the initializer expression span
    assert_eq!(side_priv.as_ref(), Some(&PrivacyAnnotation::Anonymized));
}

#[test]
fn privacy_phi_from_snomed_sets_env_and_side_table() {
    // let b = SNOMED("123456");  Healthcare code literal is PHI
    let let_span = Span {
        start: 630,
        end: 660,
        line: 21,
        column: 1,
    };
    let code_span = Span {
        start: 640,
        end: 655,
        line: 21,
        column: 11,
    };
    let code = ExpressionNode::SnomedCode(Spanned::new("123456".to_string(), code_span));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("b"),
        type_annotation: None,
        value: Some(code),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let side_priv = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt).expect("phi code literal ok");
        tc.get_privacy_at_span(&code_span).cloned()
    };

    // Env privacy set from initializer
    assert_eq!(env.get_privacy("b"), Some(PrivacyAnnotation::PHI));
    // Side privacy table recorded for the code literal expression span
    assert_eq!(side_priv.as_ref(), Some(&PrivacyAnnotation::PHI));
}
