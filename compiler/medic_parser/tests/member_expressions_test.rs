use medic_ast::ast::ExpressionNode;
use medic_lexer::token::{Location, Token, TokenType};
use medic_parser::parser::{parse_expression, TokenSlice};

// Helper function to create an identifier token
fn create_identifier_token(name: &str) -> Token {
    Token::new(
        TokenType::Identifier(name.to_string()),
        name.to_string(),
        Location {
            line: 1,
            column: 1,
            offset: 0,
        },
    )
}

// Helper function to create a dot token
fn create_dot_token() -> Token {
    Token::new(
        TokenType::Dot,
        ".".to_string(),
        Location {
            line: 1,
            column: 1,
            offset: 0,
        },
    )
}

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

    println!("Test tokens for obj.prop1.prop2: {:?}", tokens);
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {:#?}", expr);

    // The expression should be parsed as ((obj.prop1).prop2)
    // First, verify the outer structure
    let outer = if let ExpressionNode::Member(outer_box) = &expr {
        outer_box
    } else {
        panic!("Expected member expression, got {:?}", expr);
    };

    // Access the MemberExpressionNode inside the Box
    let outer_member = outer.as_ref();
    assert_eq!(outer_member.property.name, "prop2");

    // Then verify the inner structure
    let inner = match &outer_member.object {
        ExpressionNode::Member(inner_box) => inner_box.as_ref(),
        _ => panic!("Expected inner member expression 'obj.prop1'"),
    };

    assert_eq!(inner.property.name, "prop1");

    // Finally, verify the base identifier
    match &inner.object {
        ExpressionNode::Identifier(ident) => assert_eq!(ident.name, "obj"),
        _ => panic!("Expected identifier 'obj' at the base of member chain"),
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

    println!(
        "Test tokens for hospital.patient_id.records.blood_pressure: {:?}",
        tokens
    );
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {:#?}", expr);

    // Verify the nested structure
    if let ExpressionNode::Member(outer_box) = &expr {
        let outer = outer_box.as_ref();
        assert_eq!(outer.property.name, "blood_pressure");

        if let ExpressionNode::Member(inner_box) = &outer.object {
            let inner = inner_box.as_ref();
            assert_eq!(inner.property.name, "records");

            if let ExpressionNode::Member(innermost_box) = &inner.object {
                let innermost = innermost_box.as_ref();
                assert_eq!(innermost.property.name, "patient_id");

                if let ExpressionNode::Identifier(ident) = &innermost.object {
                    assert_eq!(ident.name, "hospital");
                } else {
                    panic!("Expected identifier 'hospital' at the base of member chain");
                }
            } else {
                panic!("Expected inner member expression 'hospital.patient_id'");
            }
        } else {
            panic!("Expected middle member expression 'hospital.patient_id.records'");
        }
    } else {
        panic!("Expected member expression, got {:?}", expr);
    }

    // Test with 'Patient' as a keyword (case-sensitive)
    let tokens = vec![
        create_identifier_token("hospital"),
        create_dot_token(),
        Token {
            token_type: TokenType::Patient,
            lexeme: "Patient".to_string(),
            location: Location {
                line: 1,
                column: 1,
                offset: 0,
            },
        },
        create_dot_token(),
        create_identifier_token("records"),
        create_dot_token(),
        create_identifier_token("blood_pressure"),
    ];

    println!(
        "Test tokens for hospital.Patient.records.blood_pressure: {:?}",
        tokens
    );
    let result = parse_expression(TokenSlice::new(&tokens));
    let (_, expr) = result.unwrap();
    println!("Parsed expression: {:?}", expr);

    // The expression should be parsed as (((hospital.Patient).records).blood_pressure)
    if let ExpressionNode::Member(outer) = &expr {
        assert_eq!(outer.property.name, "blood_pressure");
        if let ExpressionNode::Member(mid) = &outer.object {
            assert_eq!(mid.property.name, "records");
            if let ExpressionNode::Member(inner) = &mid.object {
                // Note: 'Patient' keyword becomes 'patient' identifier
                assert_eq!(inner.property.name, "patient");
                if let ExpressionNode::Identifier(ident) = &inner.object {
                    assert_eq!(ident.name, "hospital");
                } else {
                    panic!("Expected identifier 'hospital' at the base of member chain");
                }
            } else {
                panic!("Expected inner member expression 'hospital.Patient'");
            }
        } else {
            panic!("Expected middle member expression 'hospital.Patient.records'");
        }
    } else {
        panic!("Expected member expression, got {:?}", expr);
    }
}
