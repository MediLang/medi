use medic_ast::ast::*;
use medic_parser::parser::*;

#[test]
/// ```
fn test_unit_conversion_precedence() {
    // 5 mg → g * 3 should be parsed as (5 mg → g) * 3
    let tokens = tokenize("5 mg → g * 3").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(bin) = &expr {
        assert_eq!(bin.operator, BinaryOperator::Mul);
        
        if let ExpressionNode::Binary(left_bin) = &*bin.left {
            assert_eq!(left_bin.operator, BinaryOperator::UnitConversion);
            assert_eq!(left_bin.right.to_string(), "g");
        } else {
            panic!("Expected binary expression on left");
        }
        
        assert_eq!(bin.right.to_string(), "3");
    } else {
        panic!("Expected binary expression");
    }
}

#[test]
/// Tests that the expression "2 of 3 doses" is parsed with correct operator precedence,
/// ensuring "of" binds before multiplication and the right operand is parsed as "3 doses".
///
/// # Examples
///
/// ```
/// test_of_operator();
/// ```
fn test_of_operator() {
    // 2 of 3 doses should be parsed as (2 of 3) doses
    let tokens = tokenize("2 of 3 doses").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(bin) = &expr {
        assert_eq!(bin.operator, BinaryOperator::Of);
        assert_eq!(bin.left.to_string(), "2");
        
        if let ExpressionNode::Binary(right_bin) = &*bin.right {
            assert_eq!(right_bin.operator, BinaryOperator::Mul);
            assert_eq!(right_bin.left.to_string(), "3");
            assert_eq!(right_bin.right.to_string(), "doses");
        } else {
            panic!("Expected binary expression on right");
        }
    } else {
        panic!("Expected binary expression");
    }
}

#[test]
/// ```
fn test_per_operator() {
    // 5 mg per day should be parsed as (5 mg) per day
    let tokens = tokenize("5 mg per day").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(bin) = &expr {
        assert_eq!(bin.operator, BinaryOperator::Per);
        
        if let ExpressionNode::Binary(left_bin) = &*bin.left {
            assert_eq!(left_bin.operator, BinaryOperator::Mul);
            assert_eq!(left_bin.left.to_string(), "5");
            assert_eq!(left_bin.right.to_string(), "mg");
        } else {
            panic!("Expected binary expression on left");
        }
        
        assert_eq!(bin.right.to_string(), "day");
    } else {
        panic!("Expected binary expression");
    }
}

#[test]
/// Tests that nested member expressions are parsed correctly
///
/// Ensures that expressions like `patient.medical.records` are parsed
/// with all segments preserved in the correct order.
fn test_nested_member_expressions() {
    // Test simple nested member access
    let tokens = tokenize("patient.medical.records").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    // The expression should be parsed as ((patient.medical).records)
    if let ExpressionNode::Member(outer) = &expr {
        assert_eq!(outer.property.name, "records");
        if let ExpressionNode::Member(inner) = &*outer.object {
            assert_eq!(inner.property.name, "medical");
            if let ExpressionNode::Identifier(ident) = &*inner.object {
                assert_eq!(ident.name, "patient");
            } else {
                panic!("Expected identifier 'patient' at the start of member chain");
            }
        } else {
            panic!("Expected inner member expression 'patient.medical'");
        }
    } else {
        panic!("Expected member expression");
    }
    
    // Test with more levels of nesting
    let tokens = tokenize("hospital.patient.records.blood_pressure").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    // The expression should be parsed as (((hospital.patient).records).blood_pressure)
    if let ExpressionNode::Member(outer) = &expr {
        assert_eq!(outer.property.name, "blood_pressure");
        if let ExpressionNode::Member(mid) = &*outer.object {
            assert_eq!(mid.property.name, "records");
            if let ExpressionNode::Member(inner) = &*mid.object {
                assert_eq!(inner.property.name, "patient");
                if let ExpressionNode::Identifier(ident) = &*inner.object {
                    assert_eq!(ident.name, "hospital");
                } else {
                    panic!("Expected identifier 'hospital' at the start of member chain");
                }
            } else {
                panic!("Expected inner member expression 'hospital.patient'");
            }
        } else {
            panic!("Expected middle member expression 'hospital.patient.records'");
        }
    } else {
        panic!("Expected member expression");
    }
}

#[test]
/// ```
fn test_mixed_medical_operators() {
    // 2 of 3 doses per day should be parsed as (2 of (3 doses)) per day
    let tokens = tokenize("2 of 3 doses per day").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(outer) = &expr {
        assert_eq!(outer.operator, BinaryOperator::Per);
        
        // Left side should be (2 of (3 * doses))
        if let ExpressionNode::Binary(of_expr) = &*outer.left {
            assert_eq!(of_expr.operator, BinaryOperator::Of);
            assert_eq!(of_expr.left.to_string(), "2");
            
            if let ExpressionNode::Binary(mul_expr) = &*of_expr.right {
                assert_eq!(mul_expr.operator, BinaryOperator::Mul);
                assert_eq!(mul_expr.left.to_string(), "3");
                assert_eq!(mul_expr.right.to_string(), "doses");
            } else {
                panic!("Expected multiplication in 'of' right operand");
            }
        } else {
            panic!("Expected 'of' expression on left of 'per'");
        }
        
        assert_eq!(outer.right.to_string(), "day");
    } else {
        panic!("Expected binary expression");
    }
}
