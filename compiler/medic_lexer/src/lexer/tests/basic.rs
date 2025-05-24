use super::super::*;
use crate::token::TokenType;

#[test]
fn test_lexer_basic() {
    let input = "let x = 42;";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    assert_eq!(tokens.len(), 5);
    assert_eq!(tokens[0].token_type, TokenType::Let);
    assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
    assert_eq!(tokens[2].token_type, TokenType::Equal);
    assert_eq!(tokens[3].token_type, TokenType::Integer(42));
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
}

#[test]
fn test_lexer_float_numbers() {
    let input = "let x = 3.14; let y = .5; let z = 1e10; let w = 2.5e-3; let a = 42.;";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    // Print all tokens for debugging
    println!("All tokens:");
    for (i, token) in tokens.iter().enumerate() {
        println!("{}: {:?}", i, token);
    }

    // Expected tokens: [let, x, =, 3.14, ;, let, y, =, 0.5, ;, let, z, =, 1e10, ;, let, w, =, 2.5e-3, ;, let, a, =, 42, ., ;]
    assert_eq!(tokens.len(), 26, "Unexpected number of tokens");

    // Check the first float (3.14)
    match tokens[3].token_type {
        TokenType::Float(value) => {
            // Using exact value from input string
            #[allow(clippy::approx_constant)]
            let expected = 3.14;
            assert!(
                (value - expected).abs() < f64::EPSILON,
                "Expected Float {} at position 3, got {}",
                expected,
                value
            )
        }
        _ => panic!(
            "Expected Float at position 3, got {:?}",
            tokens[3].token_type
        ),
    }

    // Check the float with leading decimal point (.5)
    assert_eq!(
        tokens[8].token_type,
        TokenType::Float(0.5),
        "Expected Float(0.5) at position 8"
    );

    // Check the scientific notation (1e10)
    match tokens[13].token_type {
        TokenType::Float(value) => assert!(
            (value - 10000000000.0).abs() < f64::EPSILON,
            "Expected value close to 10000000000.0, got {}",
            value
        ),
        _ => panic!(
            "Expected Float token at position 13, got {:?}",
            tokens[13].token_type
        ),
    }

    // Check the scientific notation with negative exponent (2.5e-3)
    match tokens[18].token_type {
        TokenType::Float(value) => assert!(
            (value - 0.0025).abs() < f64::EPSILON,
            "Expected value close to 0.0025, got {}",
            value
        ),
        _ => panic!(
            "Expected Float token at position 18, got {:?}",
            tokens[18].token_type
        ),
    }

    // Check the integer with trailing dot (42.)
    match tokens[23].token_type {
        TokenType::Integer(42) => {}
        _ => panic!(
            "Expected Integer(42) at position 23, got {:?}",
            tokens[23].token_type
        ),
    }
    match tokens[24].token_type {
        TokenType::Dot => {}
        _ => panic!(
            "Expected Dot at position 24, got {:?}",
            tokens[24].token_type
        ),
    }
}

#[test]
fn test_lexer_range_operator() {
    let input = "1..10";
    let mut lexer = Lexer::new(input);

    println!("Input: {}", input);

    // Collect tokens and print detailed information
    let mut tokens = Vec::new();

    // Print lexer state before processing
    println!("\n=== Lexer State Before Processing ===");
    println!("Source: {}", input);

    // Process tokens one by one to see what's happening
    loop {
        match lexer.next() {
            Some(token) => {
                println!(
                    "\nProcessing token: {:?} (lexeme: '{}')",
                    token.token_type, token.lexeme
                );
                println!("  Location: {:?}", token.location);
                tokens.push(token);

                // Stop after a reasonable number of tokens to prevent infinite loops
                if tokens.len() > 10 {
                    println!("\nWarning: More than 10 tokens generated, possible infinite loop");
                    break;
                }
            }
            None => {
                println!("\nNo more tokens. Total tokens: {}", tokens.len());
                break;
            }
        }
    }

    // Print all tokens for debugging
    println!("\n=== All Tokens ({}): ===", tokens.len());
    for (i, token) in tokens.iter().enumerate() {
        println!(
            "  {}: {:?} (lexeme: '{}')",
            i, token.token_type, token.lexeme
        );
    }

    // Check if we got any tokens at all
    if tokens.is_empty() {
        panic!("No tokens were generated from the input");
    }

    // Print the first token's debug info
    if let Some(first_token) = tokens.first() {
        println!(
            "\nFirst token: {:?} (lexeme: '{}')",
            first_token.token_type, first_token.lexeme
        );
    }

    // Check the number of tokens (should be 3: 1, .., 10)
    if tokens.len() != 3 {
        println!("\nExpected 3 tokens but got {}:", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!(
                "  {}: {:?} (lexeme: '{}')",
                i, token.token_type, token.lexeme
            );
        }
        panic!("Unexpected number of tokens");
    }

    // Check each token
    assert_eq!(
        tokens[0].token_type,
        TokenType::Integer(1),
        "Expected Integer(1) at position 0"
    );
    assert_eq!(
        tokens[1].token_type,
        TokenType::Range,
        "Expected Range token at position 1"
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::Integer(10),
        "Expected Integer(10) at position 2"
    );

    // Verify the lexemes
    assert_eq!(tokens[0].lexeme, "1", "Expected lexeme '1' at position 0");
    assert_eq!(tokens[1].lexeme, "..", "Expected lexeme '..' at position 1");
    assert_eq!(tokens[2].lexeme, "10", "Expected lexeme '10' at position 2");

    // Also test with spaces around the range operator
    let input_with_spaces = "1 .. 10";
    let lexer = Lexer::new(input_with_spaces);
    let tokens: Vec<Token> = lexer.collect();

    println!("\nInput with spaces: {}", input_with_spaces);
    println!("All tokens ({}):", tokens.len());
    for (i, token) in tokens.iter().enumerate() {
        println!(
            "  {}: {:?} (lexeme: '{}')",
            i, token.token_type, token.lexeme
        );
    }

    // Should still be 3 tokens (whitespace is skipped)
    assert_eq!(tokens.len(), 3, "Unexpected number of tokens with spaces");
    assert_eq!(
        tokens[0].token_type,
        TokenType::Integer(1),
        "Expected Integer(1) at position 0 with spaces"
    );
    assert_eq!(
        tokens[1].token_type,
        TokenType::Range,
        "Expected Range token at position 1 with spaces"
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::Integer(10),
        "Expected Integer(10) at position 2 with spaces"
    );
}

#[test]
fn test_lexer_keywords() {
    let input =
        "fn let const type struct enum trait impl pub priv return while for in match if else";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    assert_eq!(tokens.len(), 17); // 17 tokens (16 keywords + 1 whitespace)
    assert_eq!(tokens[0].token_type, TokenType::Fn);
    assert_eq!(tokens[1].token_type, TokenType::Let);
    assert_eq!(tokens[2].token_type, TokenType::Const);
    assert_eq!(tokens[3].token_type, TokenType::Type);
    assert_eq!(tokens[4].token_type, TokenType::Struct);
    assert_eq!(tokens[5].token_type, TokenType::Enum);
    assert_eq!(tokens[6].token_type, TokenType::Trait);
    assert_eq!(tokens[7].token_type, TokenType::Impl);
    assert_eq!(tokens[8].token_type, TokenType::Pub);
    assert_eq!(tokens[9].token_type, TokenType::Priv);
    assert_eq!(tokens[10].token_type, TokenType::Return);
    assert_eq!(tokens[11].token_type, TokenType::While);
    assert_eq!(tokens[12].token_type, TokenType::For);
    assert_eq!(tokens[13].token_type, TokenType::In);
    assert_eq!(tokens[14].token_type, TokenType::Match);
    assert_eq!(tokens[15].token_type, TokenType::If);
    assert_eq!(tokens[16].token_type, TokenType::Else);
}
