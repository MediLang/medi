//! Parser acceptance tests mapped from `compiler/medic_parser/docs/parser_acceptance_checklist.md`.
//! We will progressively fill these in to cover grammar and healthcare constructs.

use medic_ast::ast::{BinaryOperator, ExpressionNode, StatementNode};
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::*;

fn to_slice(src: &str) -> TokenSlice<'_> {
    let toks: Vec<Token> = Lexer::new(src).collect();
    TokenSlice::new(Box::leak(Box::new(toks)))
}

#[test]
fn precedence_null_coalesce_right_associative() {
    let src = "a ?? b ?? c";
    let ts = to_slice(src);
    let (_rest, expr) = parse_expression(ts).expect("should parse expression");
    // Expect: a ?? (b ?? c)
    let ExpressionNode::Binary(top) = expr else {
        panic!("expected top binary")
    };
    assert_eq!(top.node.operator, BinaryOperator::NullCoalesce);
    // left is 'a'
    assert!(matches!(&top.node.left, ExpressionNode::Identifier(id) if id.node.name == "a"));
    // right is another ??
    let right = match &top.node.right {
        ExpressionNode::Binary(b) => b,
        _ => panic!("expected right to be ??"),
    };
    assert_eq!(right.node.operator, BinaryOperator::NullCoalesce);
    assert!(matches!(&right.node.left, ExpressionNode::Identifier(id) if id.node.name == "b"));
    assert!(matches!(&right.node.right, ExpressionNode::Identifier(id) if id.node.name == "c"));
}

#[test]
fn precedence_elvis_right_associative() {
    let src = "a ?: b ?: c";
    let ts = to_slice(src);
    let (_rest, expr) = parse_expression(ts).expect("should parse expression");
    // Expect: a ?: (b ?: c)
    let ExpressionNode::Binary(top) = expr else {
        panic!("expected top binary")
    };
    assert_eq!(top.node.operator, BinaryOperator::Elvis);
    assert!(matches!(&top.node.left, ExpressionNode::Identifier(id) if id.node.name == "a"));
    let right = match &top.node.right {
        ExpressionNode::Binary(b) => b,
        _ => panic!("expected right to be elvis"),
    };
    assert_eq!(right.node.operator, BinaryOperator::Elvis);
    assert!(matches!(&right.node.left, ExpressionNode::Identifier(id) if id.node.name == "b"));
    assert!(matches!(&right.node.right, ExpressionNode::Identifier(id) if id.node.name == "c"));
}

#[test]
fn precedence_unit_conversion_right_associative() {
    // Using ASCII arrow tokenization might be represented in lexer as a special token.
    // The source below assumes the proper token for unit-conversion is parsed from '->' style or unicode '→'.
    // We use the spec's unicode arrow.
    let src = "value → g → kg";
    let ts = to_slice(src);
    let (_rest, expr) = parse_expression(ts).expect("should parse expression");
    // Expect: value → (g → kg)
    let ExpressionNode::Binary(top) = expr else {
        panic!("expected top binary")
    };
    assert_eq!(top.node.operator, BinaryOperator::UnitConversion);
    assert!(matches!(&top.node.left, ExpressionNode::Identifier(id) if id.node.name == "value"));
    let right = match &top.node.right {
        ExpressionNode::Binary(b) => b,
        _ => panic!("expected right to be unit-conversion"),
    };
    assert_eq!(right.node.operator, BinaryOperator::UnitConversion);
    assert!(matches!(&right.node.left, ExpressionNode::Identifier(id) if id.node.name == "g"));
    assert!(matches!(&right.node.right, ExpressionNode::Identifier(id) if id.node.name == "kg"));
}

// t22: function declaration variants
#[test]
fn function_declaration_no_params_no_return_empty_body() {
    let src = r#"fn ping() {}"#;
    let ts = to_slice(src);
    let (_rest, stmt) = parse_function_declaration(ts).expect("should parse fn declaration");
    let StatementNode::Function(func) = stmt else {
        panic!("expected function node")
    };
    assert_eq!(func.name.name, "ping");
    assert_eq!(func.params.len(), 0);
    assert!(func.return_type.is_none());
    assert!(func.body.statements.is_empty());
}

#[test]
fn function_declaration_no_types_and_return_stmt() {
    let src = r#"fn greet(name) { return; }"#;
    let ts = to_slice(src);
    let (_rest, stmt) = parse_function_declaration(ts).expect("should parse fn declaration");
    let StatementNode::Function(func) = stmt else {
        panic!("expected function node")
    };
    assert_eq!(func.name.name, "greet");
    assert_eq!(func.params.len(), 1);
    assert!(func.params[0].type_annotation.is_none());
    assert!(func.return_type.is_none());
    // Body should contain a Return statement
    assert!(func
        .body
        .statements
        .iter()
        .any(|s| matches!(s, StatementNode::Return(_))));
}

#[test]
fn function_declaration_nested_blocks_and_returns() {
    let src = r#"fn outer() {
        { let x = 1; }
        return;
    }"#;
    let ts = to_slice(src);
    let (_rest, stmt) = parse_function_declaration(ts).expect("should parse fn declaration");
    let StatementNode::Function(func) = stmt else {
        panic!("expected function node")
    };
    assert_eq!(func.name.name, "outer");
    assert_eq!(func.params.len(), 0);
    assert!(func.return_type.is_none());
    // Expect at least one nested block and a return
    assert!(func
        .body
        .statements
        .iter()
        .any(|s| matches!(s, StatementNode::Block(_))));
    assert!(func
        .body
        .statements
        .iter()
        .any(|s| matches!(s, StatementNode::Return(_))));
}

#[test]
fn precedence_matrix_all_operators() {
    // Covers multiple precedence tiers; we assert a few key structural points.
    let src = "a || b && c + d * e";
    let ts = to_slice(src);
    let (_rest, expr) = parse_expression(ts).expect("should parse expression");

    // Top: OR
    let ExpressionNode::Binary(or) = expr else {
        panic!("expected OR at top")
    };
    assert_eq!(or.node.operator, BinaryOperator::Or);
    // Left of OR = identifier 'a'
    assert!(matches!(&or.node.left, ExpressionNode::Identifier(id) if id.node.name == "a"));
    // Right subtree starts with AND
    let and = match &or.node.right {
        ExpressionNode::Binary(and) => and,
        _ => panic!("expected AND on right of OR"),
    };
    assert_eq!(and.node.operator, BinaryOperator::And);
    // Right of AND: + with * inside
    let add = match &and.node.right {
        ExpressionNode::Binary(add) => add,
        _ => panic!("expected + inside AND right"),
    };
    assert_eq!(add.node.operator, BinaryOperator::Add);
    let mul = match &add.node.right {
        ExpressionNode::Binary(mul) => mul,
        _ => panic!("expected * inside + right"),
    };
    assert_eq!(mul.node.operator, BinaryOperator::Mul);
}

#[test]
#[ignore]
fn nested_blocks_conditionals_loops() {
    // TODO: Deeply nested constructs with if/else, while/for, match, blocks
}

#[test]
fn function_declaration_params_return_block() {
    let src = r#"fn calculate_bmi(weight_kg: float, height_m: float) -> float {
        let bmi = weight_kg / (height_m * height_m);
        return bmi;
    }"#;
    let ts = to_slice(src);
    let (_rest, stmt) = parse_function_declaration(ts).expect("should parse fn declaration");
    let StatementNode::Function(func) = stmt else {
        panic!("expected function node")
    };
    assert_eq!(func.name.name, "calculate_bmi");
    assert_eq!(func.params.len(), 2);
    assert!(func.return_type.is_some());
    assert!(!func.body.statements.is_empty());
}

#[test]
#[ignore]
fn declarations_let_const_types() {
    // TODO: let/const, type/struct/enum/trait/impl (as supported in v0.1)
}

#[test]
fn healthcare_fhir_query_regulate_blocks() {
    // regulate HIPAA { ... }
    let src = r#"regulate HIPAA {
        let x = 1;
    }"#;
    let ts = to_slice(src);
    let (_rest, stmt) = parse_statement(ts).expect("should parse regulate statement");
    let StatementNode::Regulate(reg) = stmt else {
        panic!("expected regulate node")
    };
    assert_eq!(reg.standard.name, "HIPAA");
    assert_eq!(reg.body.statements.len(), 1);
}

#[test]
#[ignore]
fn medical_literals_pid_icd10_and_datetime() {
    // TODO: pid("PT123"), icd10("A00.0"), datetime literals
}

#[test]
#[ignore]
fn error_diagnostics_examples() {
    // TODO: Provide a few invalid snippets and assert error categories/spans
}
