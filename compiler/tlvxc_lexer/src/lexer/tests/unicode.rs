use super::super::*;
use crate::string_interner::InternedString;
use crate::token::TokenType;

#[test]
fn test_lexer_unicode_identifiers() {
    // Test with a Unicode identifier and string
    let input = "let 名字 = \"こんにちは\"";
    println!("Input string: {input}");
    let bytes = input.as_bytes();
    println!("Input bytes: {bytes:?}");
    let chars = input.chars().count();
    let len = input.len();
    println!("Input length: {chars} chars, {len} bytes");
    println!("Input: {input}");
    println!("Input bytes: {bytes:?}");

    // Collect tokens using the iterator
    let tokens: Vec<_> = Lexer::new(input).collect();

    // Print the tokens for debugging
    for token in &tokens {
        let kind = &token.token_type;
        let lex = &token.lexeme;
        println!("Token: {kind:?} (lexeme: '{lex}')");
    }

    // Print detailed information about each token
    let count = tokens.len();
    println!("\nCollected tokens ({count}):");
    for (i, token) in tokens.iter().enumerate() {
        let kind = &token.token_type;
        let lex = &token.lexeme;
        let start = token.location.offset;
        let end = start + token.lexeme.len();
        println!("  Token {i}: {kind:?} (lexeme: '{lex}' at {start}..{end})");
    }

    // Print the input string with byte positions
    println!("\nInput string with byte positions:");
    let mut current_line = String::new();
    for (i, b) in input.bytes().enumerate() {
        print!("{b:02X} ");
        current_line.push(if b.is_ascii_graphic() || b == b' ' {
            b as char
        } else {
            '.'
        });

        if (i + 1) % 16 == 0 || i == input.len() - 1 {
            // Print ASCII representation
            println!("  | {current_line}");
            current_line.clear();
        }
    }

    // Print all tokens for debugging
    println!("\nAll tokens ({count}):");
    for (i, token) in tokens.iter().enumerate() {
        let kind = &token.token_type;
        let lex = &token.lexeme;
        println!("  {i}: {kind:?} ('{lex}')");
    }

    // We expect exactly 4 tokens: 'let', identifier, '=', string (no semicolon in input)
    let count = tokens.len();
    assert_eq!(
        count, 4,
        "Expected exactly 4 tokens, got {count}\nTokens: {tokens:#?}"
    );

    // Check each token
    let got0 = &tokens[0].token_type;
    assert_eq!(
        tokens[0].token_type,
        TokenType::Let,
        "First token should be 'let', got {got0:?}"
    );

    let expected_ident = "名字";
    let kind1 = &tokens[1].token_type;
    let lex1 = &tokens[1].lexeme;
    println!("\nToken 1: {kind1:?} ('{lex1}')");
    match &tokens[1].token_type {
        TokenType::Identifier(ident) => {
            let got = ident.as_str();
            assert_eq!(
                got, expected_ident,
                "Expected identifier '{expected_ident}', got '{got}'"
            );
        }
        _ => {
            let got = &tokens[1].token_type;
            panic!("Expected identifier token, got {got:?}")
        }
    }

    let kind2 = &tokens[2].token_type;
    let lex2 = &tokens[2].lexeme;
    println!("\nToken 2: {kind2:?} ('{lex2}')");
    let got2 = &tokens[2].token_type;
    assert_eq!(
        tokens[2].token_type,
        TokenType::Equal,
        "Third token should be '=', got {got2:?}"
    );

    let expected_str = "こんにちは";
    let kind3 = &tokens[3].token_type;
    let lex3 = &tokens[3].lexeme;
    println!("\nToken 3: {kind3:?} ('{lex3}')");
    match &tokens[3].token_type {
        TokenType::String(s) => {
            let got = s.as_str();
            assert_eq!(
                got, expected_str,
                "Expected string '{expected_str}', got '{got}'"
            );
        }
        _ => {
            let got = &tokens[3].token_type;
            panic!("Expected string token, got {got:?}")
        }
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
        TokenType::String(InternedString::from("Hello, 世界!"))
    );
    assert_eq!(
        tokens[1].token_type,
        TokenType::String(InternedString::from("Привет, мир!"))
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::String(InternedString::from("مرحبا بالعالم!"))
    );
    assert_eq!(
        tokens[3].token_type,
        TokenType::String(InternedString::from("नमस्ते दुनिया!"))
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
    assert_eq!(
        tokens[1].token_type,
        TokenType::Identifier(InternedString::from("x"))
    );
    assert_eq!(tokens[2].token_type, TokenType::Equal);
    assert_eq!(tokens[3].token_type, TokenType::Integer(42));
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
}
