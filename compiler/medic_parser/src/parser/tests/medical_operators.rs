use medic_ast::ast::*;
use medic_parser::parser::*;

#[test]
/// ```
fn test_unit_conversion_precedence() {
    // 5 mg → g * 3 should be parsed as (5 mg → g) * 3
    let tokens = tokenize("5 mg → g * 3").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(bin) = &expr {
        assert_eq!(bin.op, BinaryOperator::Mul);
        
        if let ExpressionNode::Binary(left_bin) = &*bin.left {
            assert_eq!(left_bin.op, BinaryOperator::UnitConversion);
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
        assert_eq!(bin.op, BinaryOperator::Of);
        assert_eq!(bin.left.to_string(), "2");
        
        if let ExpressionNode::Binary(right_bin) = &*bin.right {
            assert_eq!(right_bin.op, BinaryOperator::Mul);
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
        assert_eq!(bin.op, BinaryOperator::Per);
        
        if let ExpressionNode::Binary(left_bin) = &*bin.left {
            assert_eq!(left_bin.op, BinaryOperator::Mul);
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
/// ```
fn test_mixed_medical_operators() {
    // 2 of 3 doses per day should be parsed as (2 of (3 doses)) per day
    let tokens = tokenize("2 of 3 doses per day").unwrap();
    let (_, expr) = parse_expression(TokenSlice::new(&tokens)).unwrap();
    
    if let ExpressionNode::Binary(outer) = &expr {
        assert_eq!(outer.op, BinaryOperator::Per);
        
        // Left side should be (2 of (3 * doses))
        if let ExpressionNode::Binary(of_expr) = &*outer.left {
            assert_eq!(of_expr.op, BinaryOperator::Of);
            assert_eq!(of_expr.left.to_string(), "2");
            
            if let ExpressionNode::Binary(mul_expr) = &*of_expr.right {
                assert_eq!(mul_expr.op, BinaryOperator::Mul);
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
