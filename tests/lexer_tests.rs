// Unit tests for the Medi lexer (logos-based)
use medi::lexer::Token;
use logos::Logos;

#[test]
fn test_keywords_and_identifiers() {
    let input = "patient let observation foo123";
    let tokens: Vec<_> = Token::lexer(input).map(|r| r.unwrap_or(Token::Error)).collect();
    assert_eq!(tokens, vec![
        Token::Patient,
        Token::Let,
        Token::Observation,
        Token::Identifier,
    ]);
}

#[test]
fn test_literals_and_operators() {
    let input = "42 \"hello\" + - * / == != <= >= && ||";
    let tokens: Vec<_> = Token::lexer(input).map(|r| r.unwrap_or(Token::Error)).collect();
    assert_eq!(tokens, vec![
        Token::IntLiteral(42),
        Token::StringLiteral,
        Token::Plus,
        Token::Minus,
        Token::Star,
        Token::Slash,
        Token::EqEq,
        Token::Neq,
        Token::Le,
        Token::Ge,
        Token::AndAnd,
        Token::OrOr,
    ]);
}

#[test]
fn test_medical_terms_and_workflow() {
    let input = "fhir_query kaplan_meier regulate report";
    let tokens: Vec<_> = Token::lexer(input).map(|r| r.unwrap_or(Token::Error)).collect();
    assert_eq!(tokens, vec![
        Token::FhirQuery,
        Token::KaplanMeier,
        Token::Regulate,
        Token::Report,
    ]);
}
