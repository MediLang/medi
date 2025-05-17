use super::super::*;
use crate::token::TokenType;

#[test]
fn test_lexer_healthcare_keywords() {
    let input =
        "patient observation medication fhir_query query regulate scope federated safe real_time";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    assert_eq!(tokens.len(), 10);
    assert_eq!(tokens[0].token_type, TokenType::Patient);
    assert_eq!(tokens[1].token_type, TokenType::Observation);
    assert_eq!(tokens[2].token_type, TokenType::Medication);
    assert_eq!(tokens[3].token_type, TokenType::FhirQuery);
    assert_eq!(tokens[4].token_type, TokenType::Query);
    assert_eq!(tokens[5].token_type, TokenType::Regulate);
    assert_eq!(tokens[6].token_type, TokenType::Scope);
    assert_eq!(tokens[7].token_type, TokenType::Federated);
    assert_eq!(tokens[8].token_type, TokenType::Safe);
    assert_eq!(tokens[9].token_type, TokenType::RealTime);
}

#[test]
fn test_lexer_medical_codes() {
    let input = "ICD10:E11.65 LOINC:12345-6 SNOMED:1234567890 CPT:99213";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    assert_eq!(tokens.len(), 4);
    assert_eq!(
        tokens[0].token_type,
        TokenType::ICD10("ICD10:E11.65".to_string())
    );
    assert_eq!(
        tokens[1].token_type,
        TokenType::LOINC("LOINC:12345-6".to_string())
    );
    assert_eq!(
        tokens[2].token_type,
        TokenType::SNOMED("SNOMED:1234567890".to_string())
    );
    assert_eq!(
        tokens[3].token_type,
        TokenType::CPT("CPT:99213".to_string())
    );
}

#[test]
fn test_lexer_healthcare() {
    let input = "patient.fhir_query ICD10:A01.1";
    let lexer = Lexer::new(input);

    let tokens: Vec<Token> = lexer.collect();

    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].token_type, TokenType::Patient);
    assert_eq!(tokens[1].token_type, TokenType::Dot);
    assert_eq!(tokens[2].token_type, TokenType::FhirQuery);
    assert_eq!(
        tokens[3].token_type,
        TokenType::ICD10("ICD10:A01.1".to_string())
    );
}
