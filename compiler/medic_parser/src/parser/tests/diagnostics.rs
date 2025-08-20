use super::*;
use crate::parser::{parse_program_with_diagnostics, Severity, TokenSlice};
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;

// Helper to convert a string to a TokenSlice and keep tokens alive for the test lifetime
fn str_to_token_slice(input: &str) -> (TokenSlice<'_>, Vec<Token>) {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let tokens_static = Box::new(tokens.clone());
    let tokens_ref = Box::leak(tokens_static);
    (TokenSlice(tokens_ref), tokens)
}

#[cfg(test)]
mod diagnostics_tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_unexpected_right_brace_diagnostic() {
        let (slice, tokens) = str_to_token_slice("}");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.message, "Unexpected token: RightBrace");

        // Span should point at the '}' token
        let tok = &tokens[0];
        assert_eq!(err.span.start, tok.location.offset);
        assert_eq!(err.span.end, tok.location.offset + tok.lexeme.len());
        assert_eq!(err.span.line, tok.location.line as u32);
        assert_eq!(err.span.column, tok.location.column as u32);
    }

    #[test]
    fn test_expected_identifier_after_let() {
        // "let 123;" should error at the integer where an identifier is expected
        let (slice, tokens) = str_to_token_slice("let 123;");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.message, "Expected an identifier");

        // The token where it fails should be the integer 123
        // Find first integer token in the stream
        let int_tok = tokens
            .iter()
            .find(|t| matches!(t.token_type, medic_lexer::token::TokenType::Integer(_)))
            .expect("integer token present");

        assert_eq!(err.span.start, int_tok.location.offset);
        assert_eq!(err.span.end, int_tok.location.offset + int_tok.lexeme.len());
        assert_eq!(err.span.line, int_tok.location.line as u32);
        assert_eq!(err.span.column, int_tok.location.column as u32);
    }

    #[test]
    fn test_unexpected_token_span_length_for_eqeq() {
        // Starting a program with '==' should produce an unexpected token error
        let (slice, tokens) = str_to_token_slice("==");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        // Not asserting message text here beyond it being non-empty, as formatting may evolve
        assert!(!err.message.is_empty());

        // Ensure span length matches the two-character token '=='
        let tok = &tokens[0];
        assert_eq!(tok.lexeme.as_str(), "==");
        assert_eq!(err.span.start, tok.location.offset);
        assert_eq!(err.span.end, tok.location.offset + tok.lexeme.len());
        assert_eq!(err.span.end - err.span.start, 2);
    }

    #[test]
    fn test_help_suggests_eqeq_when_equal_seen() {
        // A lone '=' at start likely indicates user meant '=='
        let (slice, _tokens) = str_to_token_slice("=");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert!(err.message.starts_with("Unexpected token"));
        assert_eq!(
            err.help.as_deref(),
            Some("If you meant to compare two values, use '=='")
        );
    }

    #[test]
    fn test_help_for_unmatched_right_bracket() {
        let (slice, _tokens) = str_to_token_slice("]");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert!(err.message.starts_with("Unexpected token"));
        assert_eq!(
            err.help.as_deref(),
            Some("Did you forget a matching '[' earlier?")
        );
    }

    #[test]
    fn test_help_for_healthcare_per_token_out_of_context() {
        // 'per' is a domain operator; starting with it should produce helpful guidance
        let (slice, _tokens) = str_to_token_slice("per");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert!(err.message.starts_with("Unexpected token"));
        assert_eq!(
            err.help.as_deref(),
            Some("'per' expresses rates (e.g., 'mg per kg'). Ensure units are valid")
        );
    }
}
