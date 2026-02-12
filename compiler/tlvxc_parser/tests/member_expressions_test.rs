use tlvxc_ast::ast::{ExpressionNode, Spanned};
use tlvxc_lexer::token::{Token, TokenType};
use tlvxc_lexer::Location;
use tlvxc_parser::parser::{parse_expression, TokenSlice};

// Import test utilities
mod test_utils;
use test_utils::*;

// All token creation functions are now in test_utils.rs

#[test]
/// Tests that nested member expressions are parsed correctly
///
/// Ensures that expressions like `patient.medical.records` are parsed
/// with all segments preserved in the correct order.
fn test_nested_member_expressions() {
    // Create test tokens for obj.prop1.prop2
    let tokens = vec![
        create_identifier_token("obj"),
        create_dot_token(),
        create_identifier_token("prop1"),
        create_dot_token(),
        create_identifier_token("prop2"),
    ];

    println!("Test tokens for obj.prop1.prop2: {tokens:?}");
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {expr:#?}");

    // The expression should be parsed as ((obj.prop1).prop2)
    // First, verify the outer structure
    match &expr {
        ExpressionNode::Member(Spanned {
            node: outer_member, ..
        }) => {
            // Access the MemberExpressionNode inside the Spanned
            assert_eq!(outer_member.property.name, "prop2");

            // Then verify the inner structure
            match &outer_member.object {
                ExpressionNode::Member(Spanned { node: inner, .. }) => {
                    assert_eq!(inner.property.name, "prop1");

                    // Finally, verify the base identifier
                    match &inner.object {
                        ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                            assert_eq!(ident.name, "obj");
                        }
                        _ => panic!("Expected identifier 'obj' at the base of member chain"),
                    }
                }
                _ => panic!("Expected inner member expression 'obj.prop1'"),
            }
        }
        _ => panic!("Expected member expression, got {expr:?}"),
    }

    // Test with more levels of nesting using a different identifier
    let tokens = vec![
        create_identifier_token("hospital"),
        create_dot_token(),
        create_identifier_token("patient_id"),
        create_dot_token(),
        create_identifier_token("records"),
        create_dot_token(),
        create_identifier_token("blood_pressure"),
    ];

    println!("Test tokens for hospital.patient_id.records.blood_pressure: {tokens:?}");
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {expr:#?}");

    // Verify the nested structure
    match &expr {
        ExpressionNode::Member(Spanned { node: outer, .. }) => {
            assert_eq!(outer.property.name, "blood_pressure");

            match &outer.object {
                ExpressionNode::Member(Spanned { node: inner, .. }) => {
                    assert_eq!(inner.property.name, "records");

                    match &inner.object {
                        ExpressionNode::Member(Spanned {
                            node: innermost, ..
                        }) => {
                            assert_eq!(innermost.property.name, "patient_id");

                            match &innermost.object {
                                ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                                    assert_eq!(ident.name, "hospital");
                                }
                                _ => panic!(
                                    "Expected identifier 'hospital' at the base of member chain"
                                ),
                            }
                        }
                        _ => panic!("Expected inner member expression 'hospital.patient_id'"),
                    }
                }
                _ => panic!("Expected middle member expression 'hospital.patient_id.records'"),
            }
        }
        _ => panic!("Expected member expression, got {expr:?}"),
    }

    // Test with 'Patient' as a keyword (case-sensitive)
    let tokens = vec![
        create_identifier_token("hospital"),
        create_dot_token(),
        Token::new(
            TokenType::Patient,
            "Patient",
            Location {
                line: 1,
                column: 1,
                offset: 0,
            },
        ),
        create_dot_token(),
        create_identifier_token("records"),
        create_dot_token(),
        create_identifier_token("blood_pressure"),
    ];

    println!("Test tokens for hospital.Patient.records.blood_pressure: {tokens:?}");
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {expr:?}");

    // The expression should be parsed as (((hospital.Patient).records).blood_pressure)
    match &expr {
        ExpressionNode::Member(Spanned { node: outer, .. }) => {
            assert_eq!(outer.property.name, "blood_pressure");

            match &outer.object {
                ExpressionNode::Member(Spanned { node: mid, .. }) => {
                    assert_eq!(mid.property.name, "records");

                    match &mid.object {
                        ExpressionNode::Member(Spanned { node: inner, .. }) => {
                            // Note: 'Patient' keyword becomes 'patient' identifier
                            assert_eq!(inner.property.name, "patient");

                            match &inner.object {
                                ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                                    assert_eq!(ident.name, "hospital");
                                }
                                _ => panic!(
                                    "Expected identifier 'hospital' at the base of member chain"
                                ),
                            }
                        }
                        _ => panic!("Expected inner member expression 'hospital.Patient'"),
                    }
                }
                _ => panic!("Expected middle member expression 'hospital.Patient.records'"),
            }
        }
        _ => panic!("Expected member expression, got {expr:?}"),
    }
}
