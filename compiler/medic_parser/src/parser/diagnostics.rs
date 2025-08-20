use medic_ast::visit::Span;
use medic_lexer::token::{Token, TokenType};
use nom::error::{Error as NomError, ErrorKind};
use nom::Err as NomErr;

/// Severity levels for diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
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
            line: 0,
            column: 0,
        });
        Self {
            severity: Severity::Error,
            message,
            span,
            help,
        }
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
                line: 0,
                column: 0,
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
                line: 0,
                column: 0,
            },
            help: None,
        }
    }
}
