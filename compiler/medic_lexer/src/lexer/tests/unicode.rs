use super::super::*;
use crate::token::TokenType;

#[test]
fn test_lexer_unicode_identifiers() {
    // Test with a Unicode identifier and string
    let input = "let 名字 = \"こんにちは\"";
    println!("Input string: {}", input);
    println!("Input bytes: {:?}", input.as_bytes());
    println!(
        "Input length: {} chars, {} bytes",
        input.chars().count(),
        input.len()
    );
    println!("Input: {}", input);
    println!("Input bytes: {:?}", input.as_bytes());

    // Collect tokens using the iterator
    let tokens: Vec<_> = Lexer::new(input).collect();
    
    // Print the tokens for debugging
    for token in &tokens {
        println!("Token: {:?} (lexeme: '{}')", token.token_type, token.lexeme);
    }

    // Print detailed information about each token
    println!("\nCollected tokens ({}):", tokens.len());
    for (i, token) in tokens.iter().enumerate() {
        println!(
            "  Token {}: {:?} (lexeme: '{}' at {}..{})",
            i,
            token.token_type,
            token.lexeme,
            token.location.offset,
            token.location.offset + token.lexeme.len()
        );
    }

    // Print the input string with byte positions
    println!("\nInput string with byte positions:");
    let mut current_line = String::new();
    for (i, b) in input.bytes().enumerate() {
        print!("{:02X} ", b);
        current_line.push(if b.is_ascii_graphic() || b == b' ' {
            b as char
        } else {
            '.'
        });

        if (i + 1) % 16 == 0 || i == input.len() - 1 {
            // Print ASCII representation
            println!("  | {}", current_line);
            current_line.clear();
        }
    }

    // Print all tokens for debugging
    println!("\nAll tokens ({}):", tokens.len());
    for (i, token) in tokens.iter().enumerate() {
        println!("  {}: {:?} ('{}')", i, token.token_type, token.lexeme);
    }

    // We expect exactly 4 tokens: 'let', identifier, '=', string (no semicolon in input)
    assert_eq!(
        tokens.len(),
        4,
        "Expected exactly 4 tokens, got {}\nTokens: {:#?}",
        tokens.len(),
        tokens
    );

    // Check each token
    assert_eq!(
        tokens[0].token_type,
        TokenType::Let,
        "First token should be 'let', got {:?}",
        tokens[0].token_type
    );

    let expected_ident = "名字";
    println!(
        "\nToken 1: {:?} ('{}')",
        tokens[1].token_type, tokens[1].lexeme
    );
    match &tokens[1].token_type {
        TokenType::Identifier(ident) => {
            assert_eq!(
                ident, expected_ident,
                "Expected identifier '{}', got '{}'",
                expected_ident, ident
            );
        }
        _ => panic!("Expected identifier token, got {:?}", tokens[1].token_type),
    }

    println!(
        "\nToken 2: {:?} ('{}')",
        tokens[2].token_type, tokens[2].lexeme
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::Equal,
        "Third token should be '=', got {:?}",
        tokens[2].token_type
    );

    let expected_str = "こんにちは";
    println!(
        "\nToken 3: {:?} ('{}')",
        tokens[3].token_type, tokens[3].lexeme
    );
    match &tokens[3].token_type {
        TokenType::String(s) => {
            assert_eq!(
                s, expected_str,
                "Expected string '{}', got '{}'",
                expected_str, s
            );
        }
        _ => panic!("Expected string token, got {:?}", tokens[3].token_type),
    }
}

#[test]
fn test_lexer_unicode_strings() {
    let input = r#"
        "Hello, 世界!"
        "Привет, мир!"
        "مرحبا بالعالم!"
        "नमस्ते दुनिया!"
    "#;

    let tokens: Vec<Token> = Lexer::new(input).collect();

    // We should have 4 string tokens (one per line)
    assert_eq!(tokens.len(), 4);
    assert_eq!(
        tokens[0].token_type,
        TokenType::String("Hello, 世界!".to_string())
    );
    assert_eq!(
        tokens[1].token_type,
        TokenType::String("Привет, мир!".to_string())
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::String("مرحبا بالعالم!".to_string())
    );
    assert_eq!(
        tokens[3].token_type,
        TokenType::String("नमस्ते दुनिया!".to_string())
    );
}

#[test]
fn test_lexer_unicode_comments() {
    let input = r#"
        // This is a comment with Unicode: 你好
        let x = 42; // Another comment: こんにちは
        /*
         * Multi-line comment
         * With Unicode: 你好世界
         */
    "#;

    let tokens: Vec<Token> = Lexer::new(input).collect();

    // Should only have tokens from the actual code, comments are skipped
    // Includes semicolon token
    assert_eq!(tokens.len(), 5);
    assert_eq!(tokens[0].token_type, TokenType::Let);
    assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
    assert_eq!(tokens[2].token_type, TokenType::Equal);
    assert_eq!(tokens[3].token_type, TokenType::Integer(42));
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
}
