use medic_ast::visit::Span;
use medic_lexer::token::{Token, TokenType};
use nom::error::{Error as NomError, ErrorKind};
use nom::Err as NomErr;

/// Severity levels for diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    /// Informational message (non-actionable, general guidance)
    Info,
    Note,
}

/// A clinician-friendly diagnostic describing a problem in source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
}

impl Diagnostic {
    /// Create a diagnostic at a specific token with a custom message
    pub fn at_token<S: Into<String>>(token: &Token, message: S) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span: span_from_token(token),
            help: default_help_for_token(&token.token_type),
        }
    }

    /// Create a diagnostic with an explicit span
    pub fn at_span<S: Into<String>>(span: Span, message: S) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            help: None,
        }
    }

    /// Create a diagnostic for a nom ErrorKind, optionally anchored at a token
    pub fn from_error_kind(token: Option<&Token>, kind: ErrorKind) -> Self {
        let (message, help) = message_for_error_kind(kind, token.map(|t| &t.token_type));
        let span = token.map(span_from_token).unwrap_or_else(|| Span {
            start: 0,
            end: 0,
            line: 1,   // File-level default when no token is available
            column: 1, // Point to the start of file to avoid 0/0 coordinates
        });
        Self {
            severity: Severity::Error,
            message,
            span,
            help,
        }
    }

    /// Set the severity level on this diagnostic.
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }

    /// Set or replace the help text on this diagnostic.
    pub fn with_help<T: Into<String>>(mut self, help: T) -> Self {
        self.help = Some(help.into());
        self
    }
}

/// Convert a nom error into a single clinician-friendly diagnostic
pub fn diagnostic_from_nom_error<'a>(
    err: &NomErr<NomError<crate::parser::TokenSlice<'a>>>,
) -> Diagnostic {
    match err {
        NomErr::Error(NomError { input, code }) | NomErr::Failure(NomError { input, code }) => {
            Diagnostic::from_error_kind(input.first(), *code)
        }
        NomErr::Incomplete(_) => Diagnostic {
            severity: Severity::Error,
            message: "Incomplete input".to_string(),
            span: Span {
                start: 0,
                end: 0,
                line: 1,   // File-level default
                column: 1, // Start of file
            },
            help: Some("The parser expected more input. Did the file end unexpectedly?".into()),
        },
    }
}

/// Provide a default help message for a token type
fn default_help_for_token(tt: &TokenType) -> Option<String> {
    use TokenType::*;
    match tt {
        // Brackets and delimiters
        RightBrace => Some("Did you forget a matching '{' earlier?".to_string()),
        RightParen => Some("Did you forget a matching '(' earlier?".to_string()),
        RightBracket => Some("Did you forget a matching '[' earlier?".to_string()),
        Comma => Some("Use ',' to separate items, e.g., parameters or list entries".to_string()),
        Colon => Some("Type annotations look like 'name: Type'".to_string()),
        Semicolon => Some("Statements usually end with ';'".to_string()),

        // Common operator confusions
        Equal => Some("'=' assigns. For comparison, use '=='".to_string()),
        EqualEqual => Some("Use '=' to assign and '==' to compare".to_string()),
        NotEqual => Some("'!=' means 'not equal'".to_string()),
        QuestionQuestion => Some("'??' provides a default when the left side is missing (null)".to_string()),
        QuestionColon => Some("'?:' chooses the left value unless it is missing (Elvis operator)".to_string()),
        Range | RangeInclusive | DotDot | DotDotEq | DotDotDot => Some("Ranges use '..' or '..=' between bounds".to_string()),

        // Lexer error token
        Error(_) => Some("Unrecognized text. This is not a valid Medi token. Remove it or replace it with a valid symbol/keyword (e.g., '=' vs '==', proper unit operators like 'per').".to_string()),

        // Domain-specific tokens
        Of => Some("'of' is used in clinical expressions (e.g., '2 of doses'). Ensure it's in the right place".to_string()),
        Per => Some("'per' expresses rates (e.g., 'mg per kg'). Ensure units are valid".to_string()),
        FhirQuery | Query => Some("Ensure query expressions are in the correct syntactic context".to_string()),
        Regulate | Scope | Federated => Some("This keyword must appear in a valid clinical policy or query context".to_string()),

        // Identifiers and literals
        Identifier(_) => Some("Identifiers should start with a letter and contain letters, digits, or '_'".to_string()),
        Integer(_) | Float(_) => Some("A number appeared where a name was expected".to_string()),

        _ => None,
    }
}

/// Map a nom ErrorKind and optional token type to a user-friendly message
fn message_for_error_kind(kind: ErrorKind, tok: Option<&TokenType>) -> (String, Option<String>) {
    use ErrorKind::*;
    let msg = match (kind, tok) {
        // Token mismatch
        (Tag, Some(tt)) => format!("Unexpected token: {}", pretty_token(tt)),
        (Tag, None) => "Unexpected token".to_string(),

        // Expected character classes
        (Alpha, _) => "Expected an identifier".to_string(),
        (Digit, _) => "Expected a number".to_string(),

        // Sequences and predicates
        (TakeTill1, Some(tt)) | (TakeUntil, Some(tt)) | (TakeWhile1, Some(tt)) => {
            format!("Unexpected sequence starting with {}", pretty_token(tt))
        }
        (TakeTill1, None) | (TakeUntil, None) | (TakeWhile1, None) => {
            "Unexpected sequence".to_string()
        }

        // Combinator-related messages (kept simple and clinician-friendly)
        (Alt, _) => "Tried different forms here, but none matched".to_string(),
        (Many0, _) | (Many1, _) => "Repeated items were malformed".to_string(),
        (Verify, _) => "Value did not meet expected form".to_string(),
        (MapRes, _) => "Invalid value for this position".to_string(),
        (Permutation, _) => "Items appear in an unexpected order".to_string(),
        (Eof, _) => "Unexpected end of file. Did you forget to complete this statement or block?"
            .to_string(),

        _ => "Syntax error".to_string(),
    };

    // Provide contextual help when possible, otherwise fall back to token defaults
    let help = match (kind, tok) {
        (Tag, Some(TokenType::Equal)) => {
            Some("If you meant to compare two values, use '=='".to_string())
        }
        (Tag, Some(TokenType::RightBrace)) => {
            Some("Did you forget a matching '{' earlier?".to_string())
        }
        (Tag, Some(TokenType::RightParen)) => {
            Some("Did you forget a matching '(' earlier?".to_string())
        }
        (Tag, Some(TokenType::RightBracket)) => {
            Some("Did you forget a matching '[' earlier?".to_string())
        }
        (Alpha, Some(TokenType::Integer(_))) | (Alpha, Some(TokenType::Float(_))) => {
            Some("A number appeared where a name was expected".to_string())
        }
        _ => tok.and_then(default_help_for_token),
    };

    (msg, help)
}

/// Format a token type for display
fn pretty_token(tt: &TokenType) -> String {
    match tt {
        TokenType::Identifier(_) => "identifier".to_string(),
        other => format!("{other:?}"),
    }
}

/// Build a Span that covers an entire token
fn span_from_token(token: &Token) -> Span {
    Span {
        start: token.location.offset,
        end: token.location.offset + token.lexeme.len(),
        line: token.location.line as u32,
        column: token.location.column as u32,
    }
}

/// Compute the inclusive end column for a token based on its Unicode character length.
///
/// Columns are 1-based. If a token starts at column C and spans N Unicode scalar values,
/// the inclusive end column is C + N - 1. This respects multi-byte UTF-8 characters.
pub fn end_column_inclusive_from_token(token: &Token) -> u32 {
    let start_col = token.location.column as u32;
    let char_len = token.lexeme.as_str().chars().count() as u32;
    // Guard against empty lexemes (should not happen for real tokens)
    if char_len == 0 {
        start_col
    } else {
        start_col + char_len - 1
    }
}

/// Compute the exclusive end column (one past the last character) for a token.
///
/// This is useful for rendering spans as ranges. For a token that starts at column C
/// and spans N Unicode scalar values, the exclusive end column is C + N.
pub fn end_column_exclusive_from_token(token: &Token) -> u32 {
    token.location.column as u32 + token.lexeme.as_str().chars().count() as u32
}

/// Render a single-line annotated snippet for a diagnostic.
///
/// This formatter shows the source line and underlines the span using carets/tiles with
/// a left gutter that includes the line number and a guidance message when available.
/// If the span cannot be precisely located, it falls back to showing only the message.
pub fn render_snippet(diag: &Diagnostic, source: &str) -> String {
    // Map severity to a human-friendly lowercase label
    let severity_label = match diag.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Note => "note",
    };

    // Attempt to get the line text for the diagnostic span
    let line_idx = if diag.span.line > 0 {
        (diag.span.line - 1) as usize
    } else {
        0
    };
    let mut acc: usize = 0; // running byte offset
    let mut line_text: Option<(&str, usize)> = None; // (line_str, line_start_byte_offset)
    for (idx, line) in source.split_inclusive('\n').enumerate() {
        let line_start = acc;
        let line_end = acc + line.len();
        if idx == line_idx {
            // remove trailing newline for display
            let display_line = if let Some(stripped) = line.strip_suffix('\n') {
                stripped
            } else {
                line
            };
            line_text = Some((display_line, line_start));
            break;
        }
        acc = line_end;
    }

    let mut out = String::new();
    use std::fmt::Write as _;
    let _ = write!(
        out,
        "{sev}: {msg}\n --> line {line}, col {col}\n",
        sev = severity_label,
        msg = diag.message,
        line = diag.span.line.max(1),
        col = diag.span.column.max(1)
    );

    if let Some((line_str, line_start)) = line_text {
        // Compute start/end columns using UTF-8 char boundaries within this line
        let start_byte_in_line = diag.span.start.saturating_sub(line_start);
        let end_byte_in_line = diag.span.end.saturating_sub(line_start).min(line_str.len());

        // Convert to character indices for caret placement
        let prefix_char_count = line_str[..start_byte_in_line.min(line_str.len())]
            .chars()
            .count();
        let highlight_char_count = if end_byte_in_line > start_byte_in_line {
            line_str[start_byte_in_line..end_byte_in_line]
                .chars()
                .count()
        } else {
            1
        };

        // Gutter with line number
        let _ = write!(
            out,
            "  |\n{ln:>2} | {text}\n  | ",
            ln = diag.span.line.max(1),
            text = line_str
        );
        // Underline with carets/tiles
        for _ in 0..prefix_char_count {
            let _ = write!(out, " ");
        }
        if highlight_char_count == 1 {
            let _ = write!(out, "^");
        } else {
            for _ in 0..highlight_char_count {
                let _ = write!(out, "~");
            }
        }
        if let Some(help) = &diag.help {
            let _ = write!(out, "  help: {help}");
        }
        let _ = writeln!(out);
    } else {
        // Fallback when we cannot pinpoint the line
        if let Some(help) = &diag.help {
            let _ = writeln!(out, "help: {help}");
        }
    }

    out
}

// Optional mapping from expression-specific errors if/when used by expression parser
#[allow(dead_code)]
impl From<crate::parser::expressions::nested::ExpressionError> for Diagnostic {
    fn from(err: crate::parser::expressions::nested::ExpressionError) -> Self {
        Diagnostic {
            severity: Severity::Error,
            message: err.to_string(),
            span: Span {
                start: 0,
                end: 0,
                line: 1,   // File-level default when no precise token is available
                column: 1, // Avoid 0/0 coordinates
            },
            help: None,
        }
    }
}
