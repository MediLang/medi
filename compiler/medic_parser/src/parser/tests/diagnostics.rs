use super::*;
use crate::parser::{
    parse_block_with_recovery, parse_program_recovering, parse_program_with_diagnostics,
    render_snippet, Diagnostic, Severity, TokenSlice,
};
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
    fn test_help_for_missing_variable_name_after_let() {
        // 'let = 100' should suggest the user forgot the variable name
        let (slice, _tokens) = str_to_token_slice("let = 100");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.message, "Expected an identifier");
        assert_eq!(
            err.help.as_deref(),
            Some("Did you forget the variable name? Example: `let x = 100`")
        );
    }

    #[test]
    fn test_help_for_medical_code_used_as_identifier_name() {
        // A common domain mistake: using a code literal where a variable name should go.
        let (slice, _tokens) = str_to_token_slice("let ICD10:A00.1 = 1;");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert_eq!(err.message, "Expected an identifier");
        assert_eq!(
            err.help.as_deref(),
            Some("A medical code literal cannot be used as a name. Use an identifier on the left and put the code on the right (e.g., `let code = ICD10:E11.65`)")
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

    #[test]
    fn test_help_for_fhir_query_token_out_of_context() {
        // 'fhir_query' should be used as a function-style call.
        let (slice, _tokens) = str_to_token_slice("fhir_query");
        let err = parse_program_with_diagnostics(slice).expect_err("expected diagnostic error");

        assert_eq!(err.severity, Severity::Error);
        assert!(err.message.starts_with("Unexpected token"));
        assert_eq!(
            err.help.as_deref(),
            Some("FHIR/clinical queries are written like `fhir_query(\"Patient\", age > 65)` (i.e., a function-style call with a resource type and filters).")
        );
    }

    #[test]
    fn test_recovering_parser_handles_lexer_errors_and_continues() {
        // Two lexer errors '@' around valid statements should yield diagnostics and continue
        let (slice, _tokens) = str_to_token_slice("@ let a = 1; @ let b = 2;");

        let mut diags = Vec::new();
        let (_rest, program) =
            parse_program_recovering(slice, &mut diags).expect("recovering parse should succeed");

        // Both let statements should be parsed despite the error tokens
        assert_eq!(program.statements.len(), 2);
        // At least two diagnostics for the two error tokens
        assert!(diags.len() >= 2);
        assert!(diags
            .iter()
            .any(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_multiple_consecutive_lexer_errors_top_level() {
        // Multiple consecutive invalid tokens before a valid statement
        let (slice, tokens) = str_to_token_slice("@ @ @@ let x = 1;");

        // Count how many lexer error tokens the lexer produced to set a lower bound
        let err_count = tokens
            .iter()
            .filter(|t| matches!(t.token_type, medic_lexer::token::TokenType::Error(_)))
            .count();

        let mut diags = Vec::new();
        let (_rest, program) =
            parse_program_recovering(slice, &mut diags).expect("recovering parse should succeed");

        assert_eq!(program.statements.len(), 1);
        assert!(diags.len() >= err_count);
        assert!(diags
            .iter()
            .all(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_lexer_error_token_at_eof_top_level() {
        // A single invalid token at EOF should emit a diagnostic and finish cleanly
        let (slice, _tokens) = str_to_token_slice("@");

        let mut diags = Vec::new();
        let (_rest, program) =
            parse_program_recovering(slice, &mut diags).expect("recovering parse should succeed");

        assert_eq!(program.statements.len(), 0);
        assert!(diags
            .iter()
            .any(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_lexer_error_immediately_before_closing_brace_in_block() {
        // Use the recovering block parser to ensure graceful handling before '}'
        let (slice, _tokens) = str_to_token_slice("{ let a = 1; @ }");

        let mut diags = Vec::new();
        let (_rest, block) =
            parse_block_with_recovery(slice, &mut diags).expect("block parse should succeed");

        assert_eq!(block.statements.len(), 1);
        assert!(diags
            .iter()
            .any(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_multiple_consecutive_lexer_errors_inside_block() {
        let (slice, tokens) = str_to_token_slice("{ @ @ let b = 2; }");

        let err_count = tokens
            .iter()
            .filter(|t| matches!(t.token_type, medic_lexer::token::TokenType::Error(_)))
            .count();

        let mut diags = Vec::new();
        let (_rest, block) =
            parse_block_with_recovery(slice, &mut diags).expect("block parse should succeed");

        assert_eq!(block.statements.len(), 1);
        assert!(diags.len() >= err_count);
        assert!(diags
            .iter()
            .all(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_recovering_parser_skips_semicolons_after_lexer_error() {
        // Error token followed by stray semicolons should be skipped to next statement
        let (slice, _tokens) = str_to_token_slice("@ ;; let x = 3; ;");

        let mut diags = Vec::new();
        let (_rest, program) =
            parse_program_recovering(slice, &mut diags).expect("recovering parse should succeed");

        assert_eq!(program.statements.len(), 1);
        assert!(diags
            .iter()
            .any(|d| d.message.starts_with("Unrecognized token")));
    }

    #[test]
    fn test_render_snippet_single_char_span_caret() {
        let source = "=";
        let (slice, _tokens) = str_to_token_slice(source);
        let err = parse_program_with_diagnostics(slice).expect_err("expected error for '='");

        let snippet = render_snippet(&err, source);
        assert!(snippet.contains("error:"));
        assert!(snippet.contains(" --> line 1, col 1"));
        assert!(snippet.contains("1 | ="));
        // underline should include a caret for single-char span
        assert!(snippet.contains("^"));
    }

    #[test]
    fn test_render_snippet_multi_char_span_tildes() {
        let source = "==";
        let (slice, _tokens) = str_to_token_slice(source);
        let err = parse_program_with_diagnostics(slice).expect_err("expected error for '=='");

        let snippet = render_snippet(&err, source);
        assert!(snippet.contains("error:"));
        assert!(snippet.contains("1 | =="));
        // underline should include multiple tildes for multi-char span
        assert!(snippet.contains("~~"));
    }

    #[test]
    fn test_render_snippet_info_severity_label() {
        use medic_ast::visit::Span;
        // Construct an info diagnostic manually at a simple span
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 1,
        };
        let diag = Diagnostic::at_span(span, "Informational message").with_severity(Severity::Info);
        let snippet = render_snippet(&diag, "x");
        assert!(snippet.starts_with("info:"));
        assert!(snippet.contains("1 | x"));
    }

    #[test]
    fn test_render_snippet_span_at_line_start_explicit() {
        use medic_ast::visit::Span;
        let source = "xyz";
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 1,
        };
        let diag = Diagnostic::at_span(span, "Start of line");
        let snippet = render_snippet(&diag, source);
        assert!(snippet.contains("1 | xyz"));
        // caret should start immediately under 'x'
        assert!(snippet.contains("  | ^"));
    }

    #[test]
    fn test_render_snippet_span_at_line_end() {
        use medic_ast::visit::Span;
        let source = "abc";
        // highlight last character 'c'
        let span = Span {
            start: 2,
            end: 3,
            line: 1,
            column: 3,
        };
        let diag = Diagnostic::at_span(span, "End of line");
        let snippet = render_snippet(&diag, source);
        assert!(snippet.contains("1 | abc"));
        // After gutter '  | ' there should be two spaces then '^'
        assert!(snippet.contains("  |   ^"));
    }

    #[test]
    fn test_render_snippet_multiline_span_on_second_line() {
        let source = "let x = 1;\n==";
        let (slice, _tokens) = str_to_token_slice(source);
        let err =
            parse_program_with_diagnostics(slice).expect_err("expected error on second line '=='");

        let snippet = render_snippet(&err, source);
        assert!(snippet.contains(" --> line 2, col 1"));
        assert!(snippet.contains("2 | =="));
    }

    #[test]
    fn test_render_snippet_help_and_no_help() {
        use medic_ast::visit::Span;
        // No-help diagnostic
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 1,
        };
        let diag_no_help = Diagnostic::at_span(span, "No help here");
        let out_no_help = render_snippet(&diag_no_help, "x");
        assert!(!out_no_help.contains("help:"));

        // With-help diagnostic
        let diag_with_help = Diagnostic::at_span(span, "Has help").with_help("Try adding ';'");
        let out_with_help = render_snippet(&diag_with_help, "x");
        assert!(out_with_help.contains("help: Try adding ';'"));
    }

    #[test]
    fn test_recovered_parse_error_is_warning() {
        // Intentionally malformed inside a block to trigger recovery
        // e.g., missing expression after 'let'
        let source = "{ let ; }";
        let (slice, _tokens) = str_to_token_slice(source);
        let mut diags = Vec::new();
        let _ = parse_block_with_recovery(slice, &mut diags);
        // Expect at least one diagnostic and ensure at least one is a warning
        assert!(!diags.is_empty());
        assert!(diags
            .iter()
            .any(|d| matches!(d.severity, Severity::Warning)));
    }

    #[test]
    fn test_lexer_error_token_is_error() {
        // Use an invalid character sequence to force a lexer error token
        let source = "\u{0000}"; // NUL often becomes a lexer error
        let (slice, _tokens) = str_to_token_slice(source);
        let mut diags = Vec::new();
        let _ = parse_program_recovering(slice, &mut diags);
        // At least one diagnostic should be an Error from lexer error token handling
        assert!(diags.iter().any(|d| matches!(d.severity, Severity::Error)));
    }

    #[test]
    fn test_render_snippet_note_severity_label() {
        use medic_ast::visit::Span;
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 1,
        };
        let diag = Diagnostic::at_span(span, "FYI").with_severity(Severity::Note);
        let snippet = render_snippet(&diag, "x");
        assert!(snippet.starts_with("note:"));
    }

    #[test]
    fn test_render_snippet_info_severity_label_secondary() {
        use medic_ast::visit::Span;
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 1,
        };
        let diag = Diagnostic::at_span(span, "Style hint").with_severity(Severity::Info);
        let snippet = render_snippet(&diag, "x");
        assert!(snippet.starts_with("info:"));
    }

    #[test]
    fn test_recovery_emits_note_with_help() {
        // Trigger recovery and ensure we also emit a Note diagnostic with help
        let source = "{ let ; }";
        let (slice, _tokens) = str_to_token_slice(source);
        let mut diags = Vec::new();
        let _ = parse_block_with_recovery(slice, &mut diags);
        // There should be a Note with the expected message and help
        let note = diags
            .iter()
            .find(|d| matches!(d.severity, Severity::Note))
            .expect("expected a Note diagnostic");
        assert!(note.message.contains("Parser recovered and continued"));
        assert!(note.help.as_deref().unwrap_or("").contains("Informational"));
    }

    #[test]
    fn test_recovery_warning_contains_recovery_help_in_snippet() {
        // Ensure the Warning includes our recovery help text when rendered
        let source = "{ let ; }";
        let (slice, _tokens) = str_to_token_slice(source);
        let mut diags = Vec::new();
        let _ = parse_block_with_recovery(slice, &mut diags);
        let warn = diags
            .iter()
            .find(|d| matches!(d.severity, Severity::Warning))
            .expect("expected a Warning diagnostic");
        let snippet = render_snippet(warn, source);
        assert!(snippet.contains("skipped ahead to the next ';' or '}'"));
    }

    #[test]
    fn test_lexer_error_snippet_includes_domain_help() {
        // Force lexer error and ensure snippet shows help guidance
        let source = "\u{0000}";
        let (slice, _tokens) = str_to_token_slice(source);
        let mut diags = Vec::new();
        let _ = parse_program_recovering(slice, &mut diags);
        let err = diags
            .iter()
            .find(|d| matches!(d.severity, Severity::Error))
            .expect("expected an Error diagnostic");
        let snippet = render_snippet(err, source);
        assert!(snippet.contains("help:"));
    }
}
