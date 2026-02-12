use tlvxc_ast::ast::*;
use tlvxc_ast::visit::Span;
use tlvxc_env::env::{SinkKind, TypeEnv};
use tlvxc_typeck::type_checker::TypeChecker;
use tlvxc_typeck::runtime::{verify_sink, PrivacyViolation as RtPrivacyViolation};
use tlvxc_type::types::PrivacyAnnotation;
use tlvxc_type::types::SinkClass;

#[test]
fn hipaa_sink_print_phi_violation() {
    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    // print(SNOMED("38341003")) -> should violate
    let call = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("print"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let stmt = StatementNode::Expr(call);
    assert!(tc.check_stmt(&stmt).is_ok());
    let errs = tc.take_errors();
    assert!(errs.iter().any(|e| matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

#[test]
fn runtime_verify_sink_mirrors_checker() {
    let mut env = TypeEnv::with_prelude();
    env.set_sink_fn("upload", SinkKind::Network);
    env.set_sink_fn("store", SinkKind::File);

    // Anonymized allowed to any sink we test
    assert!(verify_sink(&env, "print", PrivacyAnnotation::Anonymized).is_ok());
    assert!(verify_sink(&env, "upload", PrivacyAnnotation::Anonymized).is_ok());

    // PHI denied to all sinks
    assert!(matches!(
        verify_sink(&env, "print", PrivacyAnnotation::PHI),
        Err(RtPrivacyViolation::Violation(_))
    ));
    assert!(matches!(
        verify_sink(&env, "upload", PrivacyAnnotation::PHI),
        Err(RtPrivacyViolation::Violation(_))
    ));

    // Pseudonymized allowed to Print, denied to Network/File
    assert!(verify_sink(&env, "print", PrivacyAnnotation::Pseudonymized).is_ok());
    assert!(verify_sink(&env, "log", PrivacyAnnotation::Pseudonymized).is_ok());
    assert!(matches!(
        verify_sink(&env, "upload", PrivacyAnnotation::Pseudonymized),
        Err(RtPrivacyViolation::Violation(_))
    ));
    assert!(matches!(
        verify_sink(&env, "store", PrivacyAnnotation::Pseudonymized),
        Err(RtPrivacyViolation::Violation(_))
    ));

    // Authorized allowed (simple model)
    assert!(verify_sink(&env, "print", PrivacyAnnotation::Authorized).is_ok());
    assert!(verify_sink(&env, "upload", PrivacyAnnotation::Authorized).is_ok());
}

#[test]
fn pseudonymized_allowed_to_print_but_not_network_or_file() {
    let mut env = TypeEnv::with_prelude();
    // Ensure default sinks exist via prelude; also add a custom 'upload' as Network and 'store' as File
    env.set_sink_fn("upload", SinkKind::Network);
    env.set_sink_fn("store", SinkKind::File);
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    // print(pseudonymize(SNOMED("38341003"))) -> allowed
    let pseudo = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("pseudonymize"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let print_call = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("print"),
                span,
            )),
            arguments: vec![pseudo.clone()],
        }),
        span,
    ));
    let stmt_ok = StatementNode::Expr(print_call);
    assert!(tc.check_stmt(&stmt_ok).is_ok());
    let errs_ok = tc.take_errors();
    assert!(errs_ok.iter().all(|e| !matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));

    // upload(pseudonymize(...)) -> violation (Network)
    let upload_call = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("upload"),
                span,
            )),
            arguments: vec![pseudo.clone()],
        }),
        span,
    ));
    let stmt_violate_net = StatementNode::Expr(upload_call);
    assert!(tc.check_stmt(&stmt_violate_net).is_ok());
    let errs_net = tc.take_errors();
    assert!(errs_net.iter().any(|e| matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));

    // store(pseudonymize(...)) -> violation (File)
    let store_call = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("store"),
                span,
            )),
            arguments: vec![pseudo],
        }),
        span,
    ));
    let stmt_violate_file = StatementNode::Expr(store_call);
    assert!(tc.check_stmt(&stmt_violate_file).is_ok());
    let errs_file = tc.take_errors();
    assert!(errs_file.iter().any(|e| matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

#[test]
fn hipaa_sink_print_after_anonymize_allowed() {
    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    // print(anonymize(SNOMED("38341003"))) -> allowed due to de-id
    let inner = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("anonymize"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let outer = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("print"),
                span,
            )),
            arguments: vec![inner],
        }),
        span,
    ));
    let stmt = StatementNode::Expr(outer);
    assert!(tc.check_stmt(&stmt).is_ok());
    let errs = tc.take_errors();
    assert!(errs.iter().all(|e| !matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

#[test]
fn hipaa_return_phi_violation() {
    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    let ret = StatementNode::Return(Box::new(ReturnNode {
        value: Some(ExpressionNode::SnomedCode(Spanned::new(
            "38341003".to_string(),
            span,
        ))),
        span,
    }));
    assert!(tc.check_stmt(&ret).is_ok());
    let errs = tc.take_errors();
    assert!(errs.iter().any(|e| matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

#[test]
fn hipaa_return_authorized_and_anonymized_allowed() {
    let mut env = TypeEnv::with_prelude();
    let mut tc = TypeChecker::new(&mut env);
    let span = Span::default();

    // let a: Authorized = "ok"; return a; -> allowed
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("Authorized"),
        span,
    ));
    let val = ExpressionNode::Literal(Spanned::new(LiteralNode::String("ok".to_string()), span));
    let let_stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("a"),
        type_annotation: Some(ann),
        value: Some(val),
        span,
    }));
    assert!(tc.check_stmt(&let_stmt).is_ok());
    let r1 = StatementNode::Return(Box::new(ReturnNode {
        value: Some(ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("a"),
            span,
        ))),
        span,
    }));
    assert!(tc.check_stmt(&r1).is_ok());

    // let b = anonymize(SNOMED(...)); return b; -> allowed
    let inner = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("anonymize"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let let_b = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("b"),
        type_annotation: None,
        value: Some(inner),
        span,
    }));
    assert!(tc.check_stmt(&let_b).is_ok());
    let r2 = StatementNode::Return(Box::new(ReturnNode {
        value: Some(ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("b"),
            span,
        ))),
        span,
    }));
    assert!(tc.check_stmt(&r2).is_ok());

    // No privacy violations expected
    let errs = tc.take_errors();
    assert!(errs.iter().all(|e| !matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

// TODO: Consider richer sink identification (e.g., env/imports providing symbol metadata)
// When implemented, add tests to register a sink-like function via env and ensure PHI is flagged.

#[test]
fn hipaa_env_registered_sink_flags_phi() {
    let mut env = TypeEnv::with_prelude();
    // Register a custom sink function 'emit' as Network sink
    env.set_sink_fn("emit", SinkKind::Network);
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    // emit(SNOMED("38341003")) -> should violate via env sink metadata
    let call = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("emit"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let stmt = StatementNode::Expr(call);
    assert!(tc.check_stmt(&stmt).is_ok());
    let errs = tc.take_errors();
    assert!(errs.iter().any(|e| matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}

#[test]
fn hipaa_env_registered_deid_allows_sink() {
    let mut env = TypeEnv::with_prelude();
    // Register a custom de-identification function 'mask'
    env.set_deid_fn("mask");
    // Also register a sink 'emit'
    env.set_sink_fn("emit", SinkKind::Network);
    let mut tc = TypeChecker::new(&mut env);

    let span = Span::default();
    // emit(mask(SNOMED("38341003"))) -> allowed due to env de-id downgrading to Anonymized
    let inner = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("mask"),
                span,
            )),
            arguments: vec![ExpressionNode::SnomedCode(Spanned::new(
                "38341003".to_string(),
                span,
            ))],
        }),
        span,
    ));
    let outer = ExpressionNode::Call(Spanned::new(
        Box::new(CallExpressionNode {
            callee: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("emit"),
                span,
            )),
            arguments: vec![inner],
        }),
        span,
    ));
    let stmt = StatementNode::Expr(outer);
    assert!(tc.check_stmt(&stmt).is_ok());
    let errs = tc.take_errors();
    assert!(errs.iter().all(|e| !matches!(e, tlvxc_typeck::type_checker::TypeError::PrivacyViolation { .. })));
}
