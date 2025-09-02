use medic_env::env::TypeEnv;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program_with_diagnostics, TokenSlice};
use medic_typeck::type_checker::TypeChecker;
use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct PrivacySpanLabel {
    pub start: usize,
    pub end: usize,
    pub label: String,
}

#[derive(Debug, Serialize)]
pub struct PrivacyReport {
    pub privacy: Vec<PrivacySpanLabel>,
    pub errors: Vec<String>,
}

/// Analyze a source string and return a privacy report with
/// inferred privacy labels per expression span and a list of
/// privacy/HIPAA-related errors.
pub fn analyze_source(source: &str) -> PrivacyReport {
    // Tokenize and parse strictly; if parsing fails, surface one error via errors list.
    let tokens: Vec<Token> = Lexer::new(source).collect();
    let input = TokenSlice::new(&tokens);

    match parse_program_with_diagnostics(input) {
        Ok(program) => {
            let mut env = TypeEnv::with_prelude();
            let mut checker = TypeChecker::new(&mut env);

            // Run type checking across statements (returns immediate errors)
            let mut errs = checker.check_program(&program);
            // Include any collected errors (validation/privacy etc.)
            errs.extend(checker.take_errors());

            // Filter to privacy/HIPAA violations only
            let error_texts: Vec<String> = errs
                .into_iter()
                .filter(|e| {
                    matches!(
                        e,
                        medic_typeck::type_checker::TypeError::PrivacyViolation { .. }
                    )
                })
                .map(|e| e.to_string())
                .collect();

            // Extract privacy table
            let mut privacy = Vec::new();
            for ((start, end), label) in checker.privacy_table_map().iter() {
                privacy.push(PrivacySpanLabel {
                    start: *start,
                    end: *end,
                    label: format!("{label:?}"),
                });
            }

            PrivacyReport {
                privacy,
                errors: error_texts,
            }
        }
        Err(diag) => PrivacyReport {
            privacy: Vec::new(),
            errors: vec![format!(
                "parse error: {}",
                medic_parser::parser::render_snippet(&diag, source)
            )],
        },
    }
}
