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
