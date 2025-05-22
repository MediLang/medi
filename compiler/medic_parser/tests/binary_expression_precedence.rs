use medic_lexer::{tokenize, TokenSlice};
use medic_parser::parser::expressions::parse_expression;

#[test]
fn test_operator_precedence() {
    // Test that multiplication has higher precedence than addition
    let tokens = tokenize("2 + 3 * 4").unwrap();
    let input = TokenSlice::new(&tokens);
    let (_, expr) = parse_expression(input).unwrap();

    // The expression should be parsed as 2 + (3 * 4), not (2 + 3) * 4
    assert_eq!(
        format!("{:?}", expr),
        "Binary { left: Literal(Number(2.0)), operator: Add, right: Binary { left: Literal(Number(3.0)), operator: Multiply, right: Literal(Number(4.0)) } }"
    );

    // Test left-associativity of addition
    let tokens = tokenize("1 - 2 + 3").unwrap();
    let input = TokenSlice::new(&tokens);
    let (_, expr) = parse_expression(input).unwrap();

    // The expression should be parsed as (1 - 2) + 3, not 1 - (2 + 3)
    assert_eq!(
        format!("{:?}", expr),
        "Binary { left: Binary { left: Literal(Number(1.0)), operator: Subtract, right: Literal(Number(2.0)) }, operator: Add, right: Literal(Number(3.0)) }"
    );

    // Test right-associativity of exponentiation (if supported)
    let tokens = tokenize("2 ^ 3 ^ 2").unwrap();
    let input = TokenSlice::new(&tokens);
    if let Ok((_, expr)) = parse_expression(input) {
        // The expression should be parsed as 2 ^ (3 ^ 2), not (2 ^ 3) ^ 2
        assert_eq!(
            format!("{:?}", expr),
            "Binary { left: Literal(Number(2.0)), operator: Exponent, right: Binary { left: Literal(Number(3.0)), operator: Exponent, right: Literal(Number(2.0)) } }"
        );
    }
}
