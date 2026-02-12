use tlvxc_lexer::streaming_lexer::{LexerConfig, StreamingLexer};
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program, TokenSlice};

#[test]
fn parse_bmi_functions_and_calls() {
    let src = r#"
fn calculate_bmi(weight_kg: float, height_m: float) -> float {
    return weight_kg / (height_m * height_m)
}

fn recommend_lifestyle_changes(patient_id: string) {
    print("Recommend lifestyle changes for ", patient_id)
}

let bmi = calculate_bmi(72.0, 1.80);
if bmi > 25.0 {
    recommend_lifestyle_changes("P1");
}
"#;

    let cfg = LexerConfig {
        max_buffer_size: 1024,
        include_whitespace: false,
        include_comments: false,
    };
    let lexer = StreamingLexer::with_config(src, cfg);
    let tokens: Vec<Token> = lexer.collect();
    assert!(!tokens.is_empty());

    let (remaining, ast) = parse_program(TokenSlice::new(&tokens)).expect("parser should succeed");
    assert!(
        remaining.is_empty(),
        "Leftover tokens: {:?}",
        remaining
            .0
            .iter()
            .map(|t| &t.token_type)
            .collect::<Vec<_>>()
    );
    assert!(!ast.statements.is_empty());
}
