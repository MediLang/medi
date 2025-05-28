use medic_lexer::{chunked_lexer::ChunkedLexer, token::TokenType};

#[test]
fn test_numeric_literals() {
    // Test valid numeric literals
    let valid_cases = [
        ("42", TokenType::Integer(42)),
        ("-123", TokenType::Integer(-123)),
        ("3.14159", TokenType::Float(std::f64::consts::PI)),
        ("1.0e10", TokenType::Float(1.0e10)),
        ("1.0e-10", TokenType::Float(1.0e-10)),
    ];

    for (input, expected) in valid_cases.iter() {
        let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
        let tokens: Vec<_> = lexer.collect();

        assert_eq!(
            tokens.len(),
            1,
            "Expected exactly one token for input: {}",
            input
        );
        assert_eq!(
            tokens[0].token_type, *expected,
            "Mismatch for input: {}",
            input
        );
    }
}

#[test]
fn test_invalid_numeric_literals() {
    // Test invalid numeric literals
    let test_cases = [
        ("123abc", "123abc"),
        ("1.2.3", "1.2.3"),
        ("0x1.2p3", "0x1"), // Hex float syntax not supported yet
        ("1_000", "1_000"),
        ("123e", "123e"),
        ("1.2e", "1.2e"),
        ("1.2e+", "1.2e+"),
    ];

    for (input, expected_error_lexeme) in test_cases.iter() {
        let cursor = std::io::Cursor::new(input.as_bytes());
        let lexer = ChunkedLexer::from_reader(cursor, Default::default());
        let tokens: Vec<_> = lexer.collect();

        assert!(
            !tokens.is_empty(),
            "Expected at least one token for input: {}",
            input
        );

        // Check if we have an error token or if the lexer split the input into multiple tokens
        let has_error = tokens
            .iter()
            .any(|t| matches!(&t.token_type, TokenType::Error(_)));
        let is_split_into_multiple = tokens.len() > 1;

        assert!(
            has_error || is_split_into_multiple,
            "Expected error token or multiple tokens for input: {}",
            input
        );

        // If we have an error token, verify it contains the expected lexeme
        if has_error {
            let error_token = tokens
                .iter()
                .find(|t| matches!(&t.token_type, TokenType::Error(_)))
                .unwrap();
            if let TokenType::Error(msg) = &error_token.token_type {
                let msg_str = msg.to_string();
                assert!(
                    msg_str.contains(*expected_error_lexeme),
                    "Error message '{}' does not contain expected lexeme '{}' for input: {}",
                    msg_str,
                    expected_error_lexeme,
                    input
                );
            }
        }
    }
}

#[test]
fn test_numeric_literals_in_expressions() {
    let source = r#"
        let x = 42;
        let y = 3.14;
        let z = x + y * 2.0;
        let invalid = 123abc;
    "#;

    let lexer = ChunkedLexer::from_reader(source.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Verify we have the expected number of tokens (including the error token)
    assert!(
        tokens.len() > 10,
        "Expected multiple tokens, got: {}",
        tokens.len()
    );

    // Check that we have an error token for the invalid numeric literal
    assert!(
        tokens
            .iter()
            .any(|t| matches!(&t.token_type, TokenType::Error(_))),
        "Expected at least one error token for invalid numeric literal"
    );
}

#[test]
fn test_error_recovery() {
    let source = r#"
        let x = 123abc;
        let y = 456;  // This should still be parsed correctly
        let z = 789;
    "#;

    let lexer = ChunkedLexer::from_reader(source.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Verify we have the expected number of tokens
    assert!(
        tokens.len() >= 10,
        "Expected multiple tokens, got: {}",
        tokens.len()
    );

    // Check that we have an error token for the invalid numeric literal
    assert!(
        tokens
            .iter()
            .any(|t| matches!(&t.token_type, TokenType::Error(_))),
        "Expected at least one error token for invalid numeric literal"
    );

    // Check that valid code after the error is still tokenized
    assert!(
        tokens
            .iter()
            .any(|t| matches!(&t.token_type, TokenType::Integer(456))),
        "Expected to find valid integer literal after error"
    );
    assert!(
        tokens
            .iter()
            .any(|t| matches!(&t.token_type, TokenType::Integer(789))),
        "Expected to find valid integer literal after error"
    );
}

// Basic test cases for the lexer
#[cfg(test)]
mod basic_tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_specific_integers() {
        let test_cases = [
            ("0", 0),
            ("42", 42),
            ("-123", -123),
            ("999999999999999", 999999999999999),
        ];

        for (input, expected) in test_cases.iter() {
            let cursor = Cursor::new(input.as_bytes());
            let lexer = ChunkedLexer::from_reader(cursor, Default::default());
            let tokens: Vec<_> = lexer.collect();

            assert_eq!(
                tokens.len(),
                1,
                "Expected exactly one token for input: {}",
                input
            );
            assert_eq!(
                tokens[0].token_type,
                TokenType::Integer(*expected),
                "Mismatch for input: {}",
                input
            );
        }
    }

    #[test]
    fn test_specific_floats() {
        let test_cases = [
            ("0.0", 0.0),
            ("3.14159", std::f64::consts::PI),
            ("-123.456", -123.456),
            ("1.0e10", 1.0e10),
            ("1.0e-10", 1.0e-10),
        ];

        for (input, expected) in test_cases.iter() {
            let cursor = Cursor::new(input.as_bytes());
            let lexer = ChunkedLexer::from_reader(cursor, Default::default());
            let tokens: Vec<_> = lexer.collect();

            assert_eq!(
                tokens.len(),
                1,
                "Expected exactly one token for input: {}",
                input
            );
            if let TokenType::Float(actual) = tokens[0].token_type {
                let diff = (actual - expected).abs();
                let tolerance = 1e-10 * expected.abs().max(1.0);
                assert!(
                    diff <= tolerance,
                    "Float parsing mismatch for input '{}': expected {}, got {}",
                    input,
                    expected,
                    actual
                );
            } else {
                panic!("Expected a float token for input: {}", input);
            }
        }
    }
}
