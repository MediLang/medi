// Use the fully qualified path to avoid ambiguity with the prelude assert_eq
use pretty_assertions::assert_eq as pretty_assert_eq;

use medic_lexer::chunked_lexer::{ChunkedLexer, ChunkedLexerConfig};
use medic_lexer::token::TokenType;

#[allow(unused_imports)]
use std::env;
use std::io::Cursor;

#[allow(dead_code)]
fn init_test_logger() {
    let _ = env_logger::builder()
        .is_test(true)
        .filter_level(log::LevelFilter::Debug)
        .try_init();
}

#[cfg(feature = "pipeline_op")]
#[test]
fn test_pipeline_operator_tokenized_when_enabled() {
    // With feature enabled, '|>' should be a single token
    let input = "|>";
    let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Expect exactly one token PipeGreater
    pretty_assert_eq!(
        tokens.len(),
        1,
        "Expected 1 token for '|>' with feature enabled"
    );
    pretty_assert_eq!(tokens[0].lexeme.as_str(), "|>");
    pretty_assert_eq!(tokens[0].token_type, TokenType::PipeGreater);
}

#[cfg(feature = "pipeline_op")]
#[test]
fn test_pipeline_operator_cross_chunk_merge() {
    // Ensure merging across chunk boundary works: '|' at end of chunk, '>' at start of next
    let input = "|>";
    let config = ChunkedLexerConfig { chunk_size: 1 }; // force 1-byte chunks
    let lexer = ChunkedLexer::from_reader(input.as_bytes(), config);
    let tokens: Vec<_> = lexer.collect();

    pretty_assert_eq!(
        tokens.len(),
        1,
        "Expected 1 merged token across chunks for '|>'"
    );
    pretty_assert_eq!(tokens[0].lexeme.as_str(), "|>");
    pretty_assert_eq!(tokens[0].token_type, TokenType::PipeGreater);
}

#[test]
fn test_long_tokens_and_performance_smoke() {
    // Very long identifier and string should tokenize without errors or stack overflow
    let long_ident = "a".repeat(2_000);
    let long_string_inner = "b".repeat(3_000);
    let long_string = format!("\"{long_string_inner}\"");
    let input = format!("{long_ident}\n{long_string}");

    let lexer = ChunkedLexer::from_reader(std::io::Cursor::new(input), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Expect two tokens (identifier and string)
    pretty_assert_eq!(tokens.len(), 2);
    assert!(matches!(tokens[0].token_type, TokenType::Identifier(_)));
    assert!(matches!(tokens[1].token_type, TokenType::String(_)));
}

#[test]
fn test_lex_prd_phase1_examples_no_errors() {
    // Lex the PRD Phase 1 fenced code blocks (skip prose) to ensure no lexer errors are emitted
    let prd_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join(".taskmaster")
        .join("docs")
        .join("PRD-Phase-1.txt");

    let content = std::fs::read_to_string(&prd_path)
        .unwrap_or_else(|_| panic!("Failed to read PRD file at {}", prd_path.display()));

    // Extract fenced code blocks ``` ... ```
    let mut in_block = false;
    let mut code = String::new();
    let mut blocks_found = 0usize;
    for line in content.lines() {
        if line.trim_start().starts_with("```") {
            in_block = !in_block;
            if !in_block {
                // Block ended
                blocks_found += 1;
                code.push('\n');
            }
            continue;
        }
        if in_block {
            code.push_str(line);
            code.push('\n');
        }
    }

    assert!(
        blocks_found > 0,
        "No fenced code blocks found in PRD Phase 1 file"
    );

    let lexer = ChunkedLexer::from_reader(std::io::Cursor::new(code), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Ensure we got some tokens and none are errors
    assert!(
        !tokens.is_empty(),
        "PRD code block lexing produced no tokens"
    );
    for t in &tokens {
        assert!(
            !matches!(t.token_type, TokenType::Error(_)),
            "Lexer error token encountered at {}:{} offset {}: {:?}",
            t.location.line,
            t.location.column,
            t.location.offset,
            t.lexeme.as_str()
        );
    }
}

#[test]
fn test_crlf_and_cr_normalization_positions() {
    // Mix of CRLF and CR only. Columns should not advance on '\r'.
    let input = "let\r\nx = 1\ry\n";
    // Tokens by lexeme (skipping whitespace): let, x, =, 1, y
    let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    let lexemes: Vec<&str> = tokens.iter().map(|t| t.lexeme.as_str()).collect();
    pretty_assert_eq!(lexemes, vec!["let", "x", "=", "1", "y"]);

    // Ensure line increments only on \n
    // 'let' at line 1
    pretty_assert_eq!(tokens[0].location.line, 1);
    // After CRLF, 'x' starts at line 2, column 1
    pretty_assert_eq!(tokens[1].location.line, 2);
    pretty_assert_eq!(tokens[1].location.column, 1);
    // '=' follows 'x ' on same line
    pretty_assert_eq!(tokens[2].location.line, 2);
    // After CR only, no new line until \n; 'y' remains on line 2
    pretty_assert_eq!(tokens[4].location.line, 2);
}

#[test]
fn test_function_like_medical_literals() {
    let input = r#"pid("PT-123") icd10("A00.1")"#;
    let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    pretty_assert_eq!(tokens.len(), 2);

    match &tokens[0].token_type {
        TokenType::PatientId(s) => pretty_assert_eq!(s.as_str(), "PT-123"),
        other => panic!("Expected PatientId, got: {other:?}"),
    }
    pretty_assert_eq!(tokens[0].lexeme.as_str(), "pid(\"PT-123\")");

    match &tokens[1].token_type {
        TokenType::ICD10(s) => pretty_assert_eq!(s.as_str(), "A00.1"),
        other => panic!("Expected ICD10, got: {other:?}"),
    }
    pretty_assert_eq!(tokens[1].lexeme.as_str(), "icd10(\"A00.1\")");
}

#[cfg(not(feature = "pipeline_op"))]
#[test]
fn test_pipeline_operator_not_tokenized() {
    // `|>` must NOT be tokenized as a single pipeline operator in v0.1.
    // Expect two tokens: '|' (BitOr) and '>' (Greater).
    let input = "|>";
    let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    // Verify exactly two tokens were produced
    pretty_assert_eq!(tokens.len(), 2, "Expected 2 tokens for '|>'");

    // Verify lexemes
    pretty_assert_eq!(tokens[0].lexeme.as_str(), "|");
    pretty_assert_eq!(tokens[1].lexeme.as_str(), ">");

    // Verify token types
    pretty_assert_eq!(tokens[0].token_type, TokenType::BitOr);
    pretty_assert_eq!(tokens[1].token_type, TokenType::Greater);
}

#[test]
fn test_numeric_literals() {
    // Test valid numeric literals
    let valid_cases = [
        ("42", TokenType::Integer(42)),
        ("-123", TokenType::NegativeInteger(-123)),
        ("3.141592653589793", TokenType::Float(std::f64::consts::PI)),
        ("1.0e10", TokenType::Float(1.0e10)),
        ("1.0e-10", TokenType::Float(1.0e-10)),
    ];

    for (input, expected) in valid_cases.iter() {
        println!("\nTesting input: '{input}'");
        let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
        let tokens: Vec<_> = lexer.collect();

        // Print all tokens for debugging
        let count = tokens.len();
        println!("Tokens ({count}):");
        for (i, token) in tokens.iter().enumerate() {
            println!("  {i}: {token:?}");
        }

        assert_eq!(
            count, 1,
            "Expected exactly one token for input: {input}, but got {count} tokens"
        );

        match (&tokens[0].token_type, expected) {
            (TokenType::Float(actual), TokenType::Float(expected)) => {
                let diff = (actual - expected).abs();
                let tolerance = 1e-10 * expected.abs().max(1.0);
                assert!(
                    diff <= tolerance,
                    "Float parsing mismatch for input '{input}': expected {expected}, got {actual}"
                );
            }
            _ => assert_eq!(
                tokens[0].token_type, *expected,
                "Mismatch for input: {input}"
            ),
        }
    }
}

// Position tracking tests (single-line, multi-line, UTF-8)
#[cfg(test)]
mod position_tests {
    use super::*;

    #[test]
    fn test_positions_single_line_ascii() {
        let input = "let x = 42;";
        let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
        let tokens: Vec<_> = lexer.collect();

        // Expect: let, Identifier(x), =, 42, ;
        let count = tokens.len();
        assert!(count >= 5, "got tokens: {tokens:?}");

        // Helper to check a token's pos quickly
        let check = |idx: usize, expected_lex: &str, line: usize, column: usize, offset: usize| {
            let t = &tokens[idx];
            pretty_assert_eq!(t.lexeme.as_str(), expected_lex, "lexeme mismatch at {idx}");
            pretty_assert_eq!(t.location.line, line, "line mismatch for '{expected_lex}'");
            pretty_assert_eq!(
                t.location.column,
                column,
                "column mismatch for '{expected_lex}'"
            );
            pretty_assert_eq!(
                t.location.offset,
                offset,
                "offset mismatch for '{expected_lex}'"
            );
        };

        check(0, "let", 1, 1, 0);
        check(1, "x", 1, 5, 4);
        check(2, "=", 1, 7, 6);
        check(3, "42", 1, 9, 8);
        check(4, ";", 1, 11, 10);
    }

    #[test]
    fn test_positions_multiline_with_utf8() {
        // Greek letters are 2-byte UTF-8; ensure columns reset per line and offsets count bytes
        let input = "β = 1\nx = 2\nγδ = 3";
        let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
        let tokens: Vec<_> = lexer.collect();

        // Expect sequence by lexeme: β, =, 1, x, =, 2, γδ, =, 3
        let seq: Vec<&str> = tokens.iter().map(|t| t.lexeme.as_str()).collect();
        pretty_assert_eq!(seq, vec!["β", "=", "1", "x", "=", "2", "γδ", "=", "3"]);

        // β at start (line 1, col 1, offset 0)
        pretty_assert_eq!(tokens[0].location.line, 1);
        pretty_assert_eq!(tokens[0].location.column, 1);
        pretty_assert_eq!(tokens[0].location.offset, 0);

        // '=' on line 1, after 'β ' (β is 2 bytes, space 1) -> column 3, offset 3
        pretty_assert_eq!(tokens[1].location.line, 1);
        pretty_assert_eq!(tokens[1].location.column, 3);
        pretty_assert_eq!(tokens[1].location.offset, 3);

        // '1' on line 1, column 5, offset 5
        pretty_assert_eq!(tokens[2].location.line, 1);
        pretty_assert_eq!(tokens[2].location.column, 5);
        pretty_assert_eq!(tokens[2].location.offset, 5);

        // 'x' starts line 2 (offset after "β = 1\n"). Bytes for first line: β(2)+' '(1)+'='(1)+' '(1)+'1'(1)=6, plus '\n'(1)=7
        pretty_assert_eq!(tokens[3].location.line, 2);
        pretty_assert_eq!(tokens[3].location.column, 1);
        pretty_assert_eq!(tokens[3].location.offset, 7);

        // '=' on line 2, after "x " -> column 3, offset 9
        pretty_assert_eq!(tokens[4].location.line, 2);
        pretty_assert_eq!(tokens[4].location.column, 3);
        pretty_assert_eq!(tokens[4].location.offset, 9);

        // '2' on line 2, column 5, offset 11
        pretty_assert_eq!(tokens[5].location.line, 2);
        pretty_assert_eq!(tokens[5].location.column, 5);
        pretty_assert_eq!(tokens[5].location.offset, 11);

        // 'γδ' starts line 3. Offsets so far: line1 (6) + '\n'(1) + line2 ("x = 2" -> 5) + '\n'(1) = 13
        pretty_assert_eq!(tokens[6].location.line, 3);
        pretty_assert_eq!(tokens[6].location.column, 1);
        pretty_assert_eq!(tokens[6].location.offset, 13);

        // '=' on line 3 after "γδ " -> column 4, offset 18 (γ(2)+δ(2)+space(1)=5; line start offset 13)
        pretty_assert_eq!(tokens[7].location.line, 3);
        pretty_assert_eq!(tokens[7].location.column, 4);
        pretty_assert_eq!(tokens[7].location.offset, 18);

        // '3' on line 3, column 6, offset 20
        pretty_assert_eq!(tokens[8].location.line, 3);
        pretty_assert_eq!(tokens[8].location.column, 6);
        pretty_assert_eq!(tokens[8].location.offset, 20);
    }

    #[test]
    fn test_utf8_in_line_column_tracking() {
        // Ensure columns account for multi-byte char before an operator on the same line
        let input = "µg == 5"; // 'µ' is 2 bytes
        let lexer = ChunkedLexer::from_reader(input.as_bytes(), Default::default());
        let tokens: Vec<_> = lexer.collect();

        // Expect: Identifier("µg"), '==', '5'
        let count = tokens.len();
        assert!(count >= 3, "got tokens: {tokens:?}");
        pretty_assert_eq!(tokens[0].lexeme.as_str(), "µg");
        pretty_assert_eq!(tokens[1].lexeme.as_str(), "==");
        pretty_assert_eq!(tokens[2].lexeme.as_str(), "5");

        // 'µg' at col 1, offset 0
        pretty_assert_eq!(tokens[0].location.column, 1);
        pretty_assert_eq!(tokens[0].location.offset, 0);

        // '==' should begin at column 4 (µ, g, space) and byte offset 4 (2 + 1 + 1)
        pretty_assert_eq!(tokens[1].location.column, 4);
        pretty_assert_eq!(tokens[1].location.offset, 4);

        // '5' at column 7 and byte offset 7
        pretty_assert_eq!(tokens[2].location.column, 7);
        pretty_assert_eq!(tokens[2].location.offset, 7);
    }
}

#[test]
fn test_invalid_numeric_literals() {
    // Enable debug logging for the test
    std::env::set_var("RUST_LOG", "debug");
    let _ = env_logger::builder().is_test(true).try_init();

    // Test invalid numeric literals
    let test_cases = [
        ("123abc", "123abc"),
        ("1.2.3", "1.2."),  // Error detected at second decimal point
        ("0x1.2p3", "0x1"), // Hex float syntax not supported yet
        ("1_000", "1_000"),
        ("123e", "123e"),
        ("1.2e", "1.2e"),
        ("1.2e+", "1.2e+"),
    ];

    for (input, expected_error_lexeme) in test_cases.iter() {
        println!("\n=== Testing input: '{input}' ===");
        let cursor = std::io::Cursor::new(input.as_bytes());

        // Create lexer with debug configuration
        let config = ChunkedLexerConfig {
            chunk_size: 8, // Small chunk size to test chunk boundary handling
        };

        let lexer = ChunkedLexer::from_reader(cursor, config);
        let tokens_result: Result<Vec<_>, _> = lexer.tokenize();

        // Print the result for debugging
        match tokens_result {
            Ok(tokens) => {
                println!("Successfully tokenized input '{input}'");
                let count = tokens.len();
                println!("Tokens ({count}):");
                for (i, token) in tokens.iter().enumerate() {
                    let kind = &token.token_type;
                    let lexeme = &token.lexeme;
                    let loc = &token.location;
                    println!("  {i}: {kind:?} - '{lexeme}' at {loc:?}");
                }

                // Check that we have at least one error token
                assert!(
                    tokens
                        .iter()
                        .any(|t| matches!(&t.token_type, TokenType::Error(_))),
                    "Expected at least one error token for input: {input}"
                );

                // Check that the error message contains the expected lexeme
                if let Some(error_token) = tokens
                    .iter()
                    .find(|t| matches!(&t.token_type, TokenType::Error(_)))
                {
                    if let TokenType::Error(msg) = &error_token.token_type {
                        let msg_str = msg.to_string();
                        assert!(
                            msg_str.contains(*expected_error_lexeme),
                            "Error message '{msg_str}' does not contain expected lexeme '{expected_error_lexeme}' for input: {input}"
                        );
                    }
                }
            }
            Err(e) => {
                // Check if the error message contains the expected lexeme
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains(*expected_error_lexeme),
                    "Error message '{error_msg}' does not contain expected lexeme '{expected_error_lexeme}' for input: {input}"
                );
                println!("Test passed with error: {e}");
            }
        }
    }
}

#[test]
fn test_numeric_literals_in_expressions() {
    std::env::set_var("RUST_LOG", "debug");
    env_logger::builder().is_test(true).try_init().ok();

    let source = r#"
        let x = 42;
        let y = 3.14;
        let z = x + y * 2.0;
        let invalid = 123abc;
    "#;

    println!("=== Starting test_numeric_literals_in_expressions ===");
    println!("Source code: {source:?}");

    let config = ChunkedLexerConfig { chunk_size: 1024 };

    let lexer = ChunkedLexer::from_reader(source.as_bytes(), config);

    // Collect all tokens and log them
    let tokens: Vec<_> = lexer
        .inspect(|token| println!("Token: {token:?}"))
        .collect();

    let count = tokens.len();
    println!("\n=== Total tokens collected: {count} ===");
    for (i, token) in tokens.iter().enumerate() {
        println!("Token[{i}]: {token:?}");
    }

    // Verify we have the expected number of tokens (including the error token)
    assert!(
        !tokens.is_empty(),
        "No tokens were generated. Expected at least some tokens."
    );

    if count <= 10 {
        println!(
            "Warning: Only {count} tokens generated, expected more. This might indicate an issue."
        );
    }

    // Check that we have an error token for the invalid numeric literal
    let has_error = tokens
        .iter()
        .any(|t| matches!(&t.token_type, TokenType::Error(_)));
    if !has_error {
        println!("No error token found. All tokens:");
        for (i, token) in tokens.iter().enumerate() {
            let kind = &token.token_type;
            let lexeme = &token.lexeme;
            println!("  {i}: {kind:?} - '{lexeme}'");
        }
    }

    assert!(
        has_error,
        "Expected at least one error token for invalid numeric literal '123abc'"
    );
}

#[test]
fn test_error_recovery() {
    // Enable debug logging for the test
    env::set_var("RUST_LOG", "debug");
    init_test_logger();

    let source = r#"
        let x = 123abc;
        let y = 456;  // This should still be parsed correctly
        let z = 789;
    "#;

    println!("=== Starting test_error_recovery ===");
    println!("Source code:");
    println!("{source}");

    let lexer = ChunkedLexer::from_reader(source.as_bytes(), Default::default());
    let tokens: Vec<_> = lexer.collect();

    let count = tokens.len();
    println!("\n=== Tokens found ({count} total) ===");
    for (i, token) in tokens.iter().enumerate() {
        println!("Token[{i}]: {token:?}");
    }

    // Verify we have the expected number of tokens
    let count = tokens.len();
    assert!(count >= 10, "Expected at least 10 tokens, got: {count}");

    // Check that we have an error token for the invalid numeric literal
    let has_error = tokens
        .iter()
        .any(|t| matches!(&t.token_type, TokenType::Error(_)));
    assert!(
        has_error,
        "Expected at least one error token for invalid numeric literal. Tokens: {tokens:?}"
    );

    if has_error {
        println!("\nFound error token as expected");
    }

    // Check that valid code after the error is still tokenized
    let has_456 = tokens
        .iter()
        .any(|t| matches!(&t.token_type, TokenType::Integer(456)));
    assert!(
        has_456,
        "Expected to find valid integer literal 456 after error. Tokens: {tokens:?}"
    );

    let has_789 = tokens
        .iter()
        .any(|t| matches!(&t.token_type, TokenType::Integer(789)));
    assert!(
        has_789,
        "Expected to find valid integer literal 789 after error. Tokens: {tokens:?}"
    );

    if has_456 && has_789 {
        println!("\nFound both expected integer literals after error");
    }
}

#[test]
fn test_specific_floats() {
    let test_cases = [
        ("0.0", 0.0),
        ("3.141592653589793", std::f64::consts::PI),
        ("-123.456", -123.456),
        ("1.0e10", 1.0e10),
        ("1.0e-10", 1.0e-10),
    ];

    for (input, expected) in test_cases.iter() {
        let cursor = Cursor::new(input.as_bytes());
        let lexer = ChunkedLexer::from_reader(cursor, Default::default());
        let tokens: Vec<_> = lexer.collect();

        pretty_assert_eq!(
            tokens.len(),
            1,
            "Expected exactly one token for input: {input}"
        );
        if let TokenType::Float(actual) = tokens[0].token_type {
            let diff = (actual - expected).abs();
            let tolerance = 1e-10 * expected.abs().max(1.0);
            assert!(
                diff <= tolerance,
                "Float parsing mismatch for input '{input}': expected {expected}, got {actual}"
            );
        } else {
            panic!("Expected a float token for input: {input}");
        }
    }
}
