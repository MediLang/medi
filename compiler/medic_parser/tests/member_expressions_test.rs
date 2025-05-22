use medic_ast::ast::ExpressionNode;
use medic_lexer::lexer::Lexer;
use medic_parser::parser::{parse_expression, TokenSlice};

fn tokenize(input: &str) -> Vec<medic_lexer::token::Token> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    tokens
}

fn get_member_expression(expr: &ExpressionNode) -> (&ExpressionNode, &str) {
    if let ExpressionNode::Member(member) = expr {
        (&member.object, &member.property.name)
    } else {
        panic!("Expected member expression, got {:?}", expr);
    }
}

#[test]
/// Tests that nested member expressions are parsed correctly
///
/// Ensures that expressions like `patient.medical.records` are parsed
/// with all segments preserved in the correct order.
fn test_nested_member_expressions() {
    // First, let's test the tokenizer output
    let tokens = tokenize("patient.medical.records");
    println!("Tokens for 'patient.medical.records': {:?}", tokens);

    // Test simple nested member access
    let result = parse_expression(TokenSlice::new(&tokens));
    println!("Parse result: {:?}", result);

    // Unwrap the result to get the parsed expression
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {:?}", expr);

    // The expression should be parsed as ((patient.medical).records)
    let (inner_expr, prop_name) = get_member_expression(&expr);
    assert_eq!(prop_name, "records");

    let (base_expr, mid_prop) = get_member_expression(inner_expr);
    assert_eq!(mid_prop, "medical");

    if let ExpressionNode::Identifier(ident) = base_expr {
        assert_eq!(ident.name, "patient");
    } else {
        panic!(
            "Expected identifier 'patient' at the start of member chain, got {:?}",
            base_expr
        );
    }

    // Test with more levels of nesting using 'patient' as an identifier
    let tokens = tokenize("hospital.patient_id.records.blood_pressure");
    println!(
        "\nTokens for 'hospital.patient_id.records.blood_pressure': {:?}",
        tokens
    );

    let result = parse_expression(TokenSlice::new(&tokens));
    println!("Parse result: {:?}", result);

    // Unwrap the result to see the actual error
    let (_remaining, expr) = result.unwrap();
    println!("Parsed expression: {:?}", expr);

    // The expression should be parsed as (((hospital.patient_id).records).blood_pressure)
    let (outer_expr, outer_prop) = get_member_expression(&expr);
    assert_eq!(outer_prop, "blood_pressure");

    let (mid_expr, mid_prop) = get_member_expression(outer_expr);
    assert_eq!(mid_prop, "records");

    let (inner_expr, inner_prop) = get_member_expression(mid_expr);
    assert_eq!(inner_prop, "patient_id");

    if let ExpressionNode::Identifier(ident) = inner_expr {
        assert_eq!(ident.name, "hospital");
    } else {
        panic!(
            "Expected identifier 'hospital' at the start of member chain, got {:?}",
            inner_expr
        );
    }

    // Test with 'Patient' as a keyword (case-sensitive)
    let tokens = tokenize("hospital.Patient.records.blood_pressure");
    println!(
        "\nTokens for 'hospital.Patient.records.blood_pressure': {:?}",
        tokens
    );

    let result = parse_expression(TokenSlice::new(&tokens));
    println!("Parse result: {:?}", result);

    // Unwrap the result to see the actual error
    let (_remaining, expr) = result.unwrap();
    println!("Parsed expression: {:?}", expr);

    // The expression should be parsed as (((hospital.Patient).records).blood_pressure)
    let (outer_expr, outer_prop) = get_member_expression(&expr);
    assert_eq!(outer_prop, "blood_pressure");

    let (mid_expr, mid_prop) = get_member_expression(outer_expr);
    assert_eq!(mid_prop, "records");

    let (inner_expr, inner_prop) = get_member_expression(mid_expr);
    // 'Patient' should be treated as a regular identifier since it's not the keyword 'patient'
    assert_eq!(inner_prop, "Patient");

    if let ExpressionNode::Identifier(ident) = inner_expr {
        assert_eq!(ident.name, "hospital");
    } else {
        panic!(
            "Expected identifier 'hospital' at the start of member chain, got {:?}",
            inner_expr
        );
    }
}
