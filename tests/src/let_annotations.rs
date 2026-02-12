#![allow(clippy::useless_conversion)]
use tlvxc_ast::ast::*;
use tlvxc_ast::visit::Span;
use tlvxc_env::env::TypeEnv;
use tlvxc_type::types::{MediType, PrivacyAnnotation};
use tlvxc_typeck::type_checker::{TypeChecker, TypeError};

#[test]
fn annotated_let_type_mismatch_reports_error() {
    // Build: let x: String = 1;
    let span = Span::default();
    let ann =
        ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("String"), span));
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
            assert_eq!(expected, tlvxc_type::types::MediType::String);
            assert_eq!(found, tlvxc_type::types::MediType::Int);
        }
        other => panic!("expected TypeMismatch, got {other:?}"),
    }
}

#[test]
fn complex_nested_type_annotations_across_typedecls() {
    // Declare inner type: VitalReading { value: Float, unit: String }
    let type_span = Span {
        start: 300,
        end: 360,
        line: 7,
        column: 1,
    };
    let fld_val_ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("Float"),
        Span {
            start: 310,
            end: 315,
            line: 7,
            column: 12,
        },
    ));
    let fld_unit_ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("String"),
        Span {
            start: 320,
            end: 326,
            line: 7,
            column: 22,
        },
    ));
    let vital_decl = StatementNode::TypeDecl(Box::new(TypeDeclNode {
        name: IdentifierNode::from_str_name("VitalReading"),
        fields: vec![
            TypeField {
                name: IdentifierNode::from_str_name("value").name,
                type_annotation: fld_val_ann,
            },
            TypeField {
                name: IdentifierNode::from_str_name("unit").name,
                type_annotation: fld_unit_ann,
            },
        ]
        .into(),
        span: type_span,
    }));

    // Declare outer type: Measurement { reading: VitalReading }
    let outer_span = Span {
        start: 365,
        end: 420,
        line: 8,
        column: 1,
    };
    let reading_ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("VitalReading"),
        Span {
            start: 380,
            end: 392,
            line: 8,
            column: 16,
        },
    ));
    let outer_decl = StatementNode::TypeDecl(Box::new(TypeDeclNode {
        name: IdentifierNode::from_str_name("Measurement"),
        fields: vec![TypeField {
            name: IdentifierNode::from_str_name("reading").name,
            type_annotation: reading_ann,
        }]
        .into(),
        span: outer_span,
    }));

    // let m: Measurement = { reading: { value: 98.6, unit: "F" } };
    let let_span = Span {
        start: 430,
        end: 520,
        line: 9,
        column: 1,
    };
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("Measurement"),
        Span {
            start: 440,
            end: 451,
            line: 9,
            column: 10,
        },
    ));
    let inner_struct = ExpressionNode::Struct(Spanned::new(
        Box::new(StructLiteralNode {
            type_name: IdentifierNode::from_str_name("VitalReading").name,
            fields: vec![
                StructField {
                    name: IdentifierNode::from_str_name("value").name,
                    value: ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Float(98.6),
                        Span {
                            start: 470,
                            end: 475,
                            line: 9,
                            column: 30,
                        },
                    )),
                },
                StructField {
                    name: IdentifierNode::from_str_name("unit").name,
                    value: ExpressionNode::Literal(Spanned::new(
                        LiteralNode::String("F".to_string()),
                        Span {
                            start: 480,
                            end: 483,
                            line: 9,
                            column: 40,
                        },
                    )),
                },
            ]
            .into(),
        }),
        let_span,
    ));
    let outer_struct = ExpressionNode::Struct(Spanned::new(
        Box::new(StructLiteralNode {
            type_name: IdentifierNode::from_str_name("Measurement").name,
            fields: vec![StructField {
                name: IdentifierNode::from_str_name("reading").name,
                value: inner_struct,
            }]
            .into(),
        }),
        let_span,
    ));
    let let_stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("m"),
        type_annotation: Some(ann),
        value: Some(outer_struct),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&vital_decl).expect("inner type decl ok");
        tc.check_stmt(&outer_decl).expect("outer type decl ok");
        tc.check_stmt(&let_stmt)
            .expect("nested struct annotated let ok");
        tc.type_table()
            .get(&(let_span.start, let_span.end))
            .cloned()
    };
    let measurement_ty = env.get("Measurement").cloned().expect("outer type in env");
    assert_eq!(env.get("m"), Some(&measurement_ty));
    assert_eq!(ty_from_table.as_ref(), Some(&measurement_ty));
}

#[test]
fn annotated_let_type_match_records_binding_and_side_table() {
    // Build: let x: Int = 1;
    let let_span = Span {
        start: 0,
        end: 10,
        line: 1,
        column: 1,
    };
    let ann_span = Span {
        start: 7,
        end: 10,
        line: 1,
        column: 8,
    };
    let val_span = Span {
        start: 13,
        end: 14,
        line: 1,
        column: 14,
    };
    let ann =
        ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("Int"), ann_span));
    let val = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), val_span));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("x"),
        type_annotation: Some(ann),
        value: Some(val),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt).expect("type match should succeed");
        tc.type_table()
            .get(&(let_span.start, let_span.end))
            .cloned()
    };
    // Env binding
    assert_eq!(env.get("x"), Some(&MediType::Int));
    assert_eq!(ty_from_table, Some(MediType::Int));
}

#[test]
fn inference_without_annotation_uses_initializer_type_and_records_side_table() {
    // Build: let y = 3.14;
    let let_span = Span {
        start: 20,
        end: 35,
        line: 2,
        column: 1,
    };
    let val_span = Span {
        start: 28,
        end: 32,
        line: 2,
        column: 9,
    };
    let val = ExpressionNode::Literal(Spanned::new(
        LiteralNode::Float(std::f64::consts::PI),
        val_span,
    ));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("y"),
        type_annotation: None,
        value: Some(val),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt).expect("inference should succeed");
        tc.type_table()
            .get(&(let_span.start, let_span.end))
            .cloned()
    };
    assert_eq!(env.get("y"), Some(&MediType::Float));
    assert_eq!(ty_from_table, Some(MediType::Float));
}

#[test]
fn healthcare_specific_annotation_binds_without_initializer() {
    // Build: let pid: PatientId;
    let let_span = Span {
        start: 40,
        end: 55,
        line: 3,
        column: 1,
    };
    let ann_span = Span {
        start: 48,
        end: 57,
        line: 3,
        column: 9,
    };
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("PatientId"),
        ann_span,
    ));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("pid"),
        type_annotation: Some(ann),
        value: None,
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt)
            .expect("annotation-only let should succeed");
        tc.type_table()
            .get(&(let_span.start, let_span.end))
            .cloned()
    };
    assert_eq!(env.get("pid"), Some(&MediType::PatientId));
    assert_eq!(ty_from_table, Some(MediType::PatientId));
}

#[test]
fn struct_annotation_via_typedecl_and_matching_initializer() {
    // type VitalReading { value: Float, unit: String }
    let type_span = Span {
        start: 60,
        end: 120,
        line: 4,
        column: 1,
    };
    let field_value_ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("Float"),
        Span {
            start: 80,
            end: 85,
            line: 4,
            column: 21,
        },
    ));
    let field_unit_ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("String"),
        Span {
            start: 95,
            end: 101,
            line: 4,
            column: 36,
        },
    ));
    let type_decl = StatementNode::TypeDecl(Box::new(TypeDeclNode {
        name: IdentifierNode::from_str_name("VitalReading"),
        fields: vec![
            TypeField {
                name: IdentifierNode::from_str_name("value").name,
                type_annotation: field_value_ann,
            },
            TypeField {
                name: IdentifierNode::from_str_name("unit").name,
                type_annotation: field_unit_ann,
            },
        ]
        .into(),
        span: type_span,
    }));

    // let v: VitalReading = { value: 98.6, unit: "F" };
    let let_span = Span {
        start: 130,
        end: 200,
        line: 5,
        column: 1,
    };
    let ann = ExpressionNode::Identifier(Spanned::new(
        IdentifierNode::from_str_name("VitalReading"),
        Span {
            start: 140,
            end: 152,
            line: 5,
            column: 10,
        },
    ));
    let struct_expr = ExpressionNode::Struct(Spanned::new(
        Box::new(StructLiteralNode {
            type_name: IdentifierNode::from_str_name("VitalReading").name,
            fields: vec![
                StructField {
                    name: IdentifierNode::from_str_name("value").name,
                    value: ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Float(98.6),
                        Span {
                            start: 165,
                            end: 170,
                            line: 5,
                            column: 30,
                        },
                    )),
                },
                StructField {
                    name: IdentifierNode::from_str_name("unit").name,
                    value: ExpressionNode::Literal(Spanned::new(
                        LiteralNode::String("F".to_string()),
                        Span {
                            start: 180,
                            end: 183,
                            line: 5,
                            column: 40,
                        },
                    )),
                },
            ]
            .into(),
        }),
        let_span,
    ));
    let let_stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("v"),
        type_annotation: Some(ann),
        value: Some(struct_expr),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        // First register the type
        tc.check_stmt(&type_decl).expect("type decl ok");
        // Then check the let
        tc.check_stmt(&let_stmt).expect("struct-annotated let ok");
        tc.type_table()
            .get(&(let_span.start, let_span.end))
            .cloned()
    };

    // The env should have the binding and the type table should record the let span
    let vital_ty = env.get("VitalReading").cloned().expect("type in env");
    assert_eq!(env.get("v"), Some(&vital_ty));
    assert_eq!(ty_from_table.as_ref(), Some(&vital_ty));
}

#[test]
fn side_table_records_initializer_expression_span_type() {
    // Build: let z = 1 .. 2;  (Range(Int))
    let let_span = Span {
        start: 210,
        end: 230,
        line: 6,
        column: 1,
    };
    let left_span = Span {
        start: 215,
        end: 216,
        line: 6,
        column: 6,
    };
    let right_span = Span {
        start: 221,
        end: 222,
        line: 6,
        column: 12,
    };
    let bin_span = Span {
        start: 215,
        end: 222,
        line: 6,
        column: 6,
    };
    let left = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), left_span));
    let right = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(2), right_span));
    let range_expr = ExpressionNode::Binary(Spanned::new(
        Box::new(BinaryExpressionNode {
            left: left.clone(),
            operator: BinaryOperator::Range,
            right: right.clone(),
        }),
        bin_span,
    ));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("z"),
        type_annotation: None,
        value: Some(range_expr.clone()),
        span: let_span,
    }));

    let mut env = TypeEnv::with_prelude();
    let expr_ty = {
        let mut tc = TypeChecker::new(&mut env);
        tc.check_stmt(&stmt).expect("range inference ok");
        tc.type_table()
            .get(&(bin_span.start, bin_span.end))
            .cloned()
    };

    // Let binding
    assert_eq!(
        env.get("z"),
        Some(&MediType::Range(Box::new(MediType::Int)))
    );

    // Side table recorded the range expr type at its span
    assert_eq!(expr_ty, Some(MediType::Range(Box::new(MediType::Int))));
}

#[test]
fn annotated_let_with_unknown_type_binds_unknown() {
    // Build: let y: Foo;
    let span = Span::default();
    let ann = ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("Foo"), span));
    let stmt = StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("y"),
        type_annotation: Some(ann),
        value: None,
        span,
    }));

    let mut env = TypeEnv::with_prelude();
    let ty_from_table = {
        let mut tc = TypeChecker::new(&mut env);
        // Checker treats unknown annotations as Unknown, not an error
        tc.check_stmt(&stmt)
            .expect("unknown type should bind as Unknown");
        tc.type_table().get(&(span.start, span.end)).cloned()
    };
    assert_eq!(env.get("y"), Some(&MediType::Unknown));
    assert_eq!(ty_from_table, Some(MediType::Unknown));
}
