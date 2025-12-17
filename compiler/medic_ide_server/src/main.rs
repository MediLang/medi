use axum::{
    extract::Json,
    http::Method,
    routing::{get, post},
    Router,
};
use medic_ast::visit::Span;
use medic_env::env::TypeEnv;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{
    parse_program_recovering, parse_program_with_diagnostics, Diagnostic, TokenSlice,
};
use medic_type::types::MediType;
use medic_typeck::type_checker::TypeChecker;
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use tower_http::cors::{Any, CorsLayer};

#[derive(Debug, Deserialize)]
struct AnalyzeRequest {
    source: String,
}

#[derive(Debug, Deserialize)]
struct CompleteRequest {
    source: String,
    offset: usize,
}

#[derive(Debug, Deserialize)]
struct HoverRequest {
    source: String,
    offset: usize,
}

#[derive(Debug, Serialize)]
struct DiagnosticDto {
    message: String,
    severity: String,
    start: usize,
    end: usize,
    line: usize,
    column: usize,
    help: Option<String>,
}

#[derive(Debug, Serialize)]
struct PrivacySpanDto {
    start: usize,
    end: usize,
    label: String,
}

#[derive(Debug, Serialize)]
struct CompletionItemDto {
    label: String,
    kind: String,
}

#[derive(Debug, Serialize)]
struct AnalyzeResponse {
    diagnostics: Vec<DiagnosticDto>,
    privacy: Vec<PrivacySpanDto>,
}

#[derive(Debug, Serialize)]
struct CompleteResponse {
    from: usize,
    items: Vec<CompletionItemDto>,
}

#[derive(Debug, Serialize)]
struct HoverResponse {
    token: Option<String>,
    token_start: Option<usize>,
    token_end: Option<usize>,
    type_info: Option<String>,
    privacy: Option<String>,
    unit: Option<String>,
}

#[tokio::main]
async fn main() {
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods([Method::GET, Method::POST])
        .allow_headers(Any);

    let app = Router::new()
        .route("/health", get(health))
        .route("/analyze", post(analyze))
        .route("/complete", post(complete))
        .route("/hover", post(hover))
        .layer(cors);

    let addr = SocketAddr::from(([127, 0, 0, 1], 8710));
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    println!("medic_ide_server listening on http://{addr}");
    axum::serve(listener, app).await.unwrap();
}

async fn health() -> &'static str {
    "ok"
}

async fn analyze(Json(req): Json<AnalyzeRequest>) -> Json<AnalyzeResponse> {
    let source = req.source;

    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let input = TokenSlice::new(&tokens);

    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    let program = match parse_program_with_diagnostics(input) {
        Ok(p) => {
            let _ = parse_program_recovering(TokenSlice::new(&tokens), &mut diagnostics);
            Some(p)
        }
        Err(diag) => {
            diagnostics.push(diag);
            None
        }
    };

    let mut privacy: Vec<PrivacySpanDto> = Vec::new();
    if let Some(program) = program {
        let mut env = TypeEnv::with_prelude();
        let mut checker = TypeChecker::new(&mut env);
        let mut _errs = checker.check_program(&program);
        _errs.extend(checker.take_errors());

        for ((start, end), label) in checker.privacy_table_map().iter() {
            privacy.push(PrivacySpanDto {
                start: *start,
                end: *end,
                label: format!("{label:?}"),
            });
        }
    }

    let diagnostics = diagnostics
        .into_iter()
        .map(|d| DiagnosticDto {
            message: d.message,
            severity: format!("{:?}", d.severity).to_lowercase(),
            start: d.span.start,
            end: d.span.end,
            line: d.span.line as usize,
            column: d.span.column as usize,
            help: d.help,
        })
        .collect();

    Json(AnalyzeResponse {
        diagnostics,
        privacy,
    })
}

fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_uppercase() || b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'_'
}

fn prefix_range(source: &str, offset: usize) -> (usize, String) {
    let bytes = source.as_bytes();
    let mut start = offset.min(bytes.len());
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    let end = offset.min(bytes.len());
    let prefix = source.get(start..end).unwrap_or("").to_string();
    (start, prefix)
}

fn best_span_containing<'a, T>(
    items: impl Iterator<Item = (&'a (usize, usize), &'a T)>,
    offset: usize,
) -> Option<((usize, usize), &'a T)> {
    let mut best: Option<((usize, usize), &'a T)> = None;
    for ((start, end), v) in items {
        if *start <= offset && offset < *end {
            let len = end.saturating_sub(*start);
            match best {
                None => best = Some(((*start, *end), v)),
                Some(((bs, be), _)) => {
                    if len < be.saturating_sub(bs) {
                        best = Some(((*start, *end), v));
                    }
                }
            }
        }
    }
    best
}

async fn complete(Json(req): Json<CompleteRequest>) -> Json<CompleteResponse> {
    let (from, prefix) = prefix_range(&req.source, req.offset);
    let prefix_lower = prefix.to_lowercase();

    let keywords: Vec<&'static str> = vec![
        "module",
        "import",
        "fn",
        "let",
        "const",
        "type",
        "struct",
        "enum",
        "trait",
        "impl",
        "pub",
        "priv",
        "return",
        "while",
        "for",
        "in",
        "match",
        "if",
        "else",
        "loop",
        "break",
        "continue",
        "true",
        "false",
        "nil",
        "regulate",
        "scope",
        "federated",
        "query",
        "fhir_query",
        "of",
        "per",
    ];

    let builtins: Vec<&'static str> = vec![
        "print",
        "println",
        "log",
        "debug",
        "trace",
        "export",
        "write_file",
        "writeFile",
        "save",
        "send_http",
        "http_post",
        "http_get",
    ];

    let mut items: Vec<CompletionItemDto> = Vec::new();
    for k in keywords {
        if prefix_lower.is_empty() || k.starts_with(&prefix_lower) {
            items.push(CompletionItemDto {
                label: k.to_string(),
                kind: "keyword".to_string(),
            });
        }
    }

    for b in builtins {
        if prefix_lower.is_empty() || b.to_lowercase().starts_with(&prefix_lower) {
            items.push(CompletionItemDto {
                label: b.to_string(),
                kind: "function".to_string(),
            });
        }
    }

    let env = TypeEnv::with_prelude();
    for (name, ty) in env.collect_symbol_types() {
        let name_lower = name.to_lowercase();
        if !prefix_lower.is_empty() && !name_lower.starts_with(&prefix_lower) {
            continue;
        }
        let kind = match ty {
            MediType::Function { .. } => "function",
            _ => "symbol",
        };
        items.push(CompletionItemDto {
            label: name,
            kind: kind.to_string(),
        });
    }

    items.sort_by(|a, b| a.label.cmp(&b.label));
    items.dedup_by(|a, b| a.label == b.label);

    Json(CompleteResponse { from, items })
}

async fn hover(Json(req): Json<HoverRequest>) -> Json<HoverResponse> {
    let source = req.source;
    let offset = req.offset;

    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let tok = tokens.iter().find(|t| {
        let start = t.location.offset;
        let end = start.saturating_add(t.lexeme.len());
        start <= offset && offset < end
    });

    let (token, token_start, token_end) = if let Some(t) = tok {
        let start = t.location.offset;
        let end = start.saturating_add(t.lexeme.len());
        (Some(t.lexeme.as_str().to_string()), Some(start), Some(end))
    } else {
        (None, None, None)
    };

    let input = TokenSlice::new(&tokens);
    let program = parse_program_with_diagnostics(input).ok();

    let mut type_info: Option<String> = None;
    let mut privacy: Option<String> = None;
    let mut unit: Option<String> = None;

    if let Some(program) = program {
        let mut env = TypeEnv::with_prelude();
        let mut checker = TypeChecker::new(&mut env);
        let mut _errs = checker.check_program(&program);
        _errs.extend(checker.take_errors());

        if let Some(((start, end), ty)) = best_span_containing(checker.type_table().iter(), offset)
        {
            type_info = Some(format!("{ty:?}"));
            let span = Span {
                start,
                end,
                line: 1,
                column: 1,
            };
            if let Some(p) = checker.get_privacy_at_span(&span) {
                privacy = Some(format!("{p:?}"));
            }
            if let Some(u) = checker.get_unit_at_span(&span) {
                unit = Some(u.clone());
            }
        } else if let Some(((start, end), p)) =
            best_span_containing(checker.privacy_table_map().iter(), offset)
        {
            privacy = Some(format!("{p:?}"));
            let span = Span {
                start,
                end,
                line: 1,
                column: 1,
            };
            if let Some(u) = checker.get_unit_at_span(&span) {
                unit = Some(u.clone());
            }
        }

        if type_info.is_none() || privacy.is_none() || unit.is_none() {
            if let (Some(ts), Some(te)) = (token_start, token_end) {
                let span = Span {
                    start: ts,
                    end: te,
                    line: 1,
                    column: 1,
                };
                if type_info.is_none() {
                    if let Some(ty) = checker.get_type_at_span(&span) {
                        type_info = Some(format!("{ty:?}"));
                    }
                }
                if privacy.is_none() {
                    if let Some(p) = checker.get_privacy_at_span(&span) {
                        privacy = Some(format!("{p:?}"));
                    }
                }
                if unit.is_none() {
                    if let Some(u) = checker.get_unit_at_span(&span) {
                        unit = Some(u.clone());
                    }
                }
            }
        }
    }

    if type_info.is_none() {
        if let Some(t) = token.as_deref() {
            let t = t.trim();
            if (t.starts_with('"') && t.ends_with('"'))
                || (t.starts_with('\'') && t.ends_with('\''))
            {
                type_info = Some("String".to_string());
            } else if t == "true" || t == "false" {
                type_info = Some("Bool".to_string());
            } else if t == "nil" || t == "null" {
                type_info = Some("Void".to_string());
            } else if !t.is_empty() && t.chars().all(|c| c.is_ascii_digit()) {
                type_info = Some("Int".to_string());
            } else if t.parse::<f64>().is_ok() && t.contains('.') {
                type_info = Some("Float".to_string());
            } else {
                let env = TypeEnv::with_prelude();
                if let Some(ty) = env.get(t) {
                    type_info = Some(format!("{ty:?}"));
                }
            }
        }
    }

    Json(HoverResponse {
        token,
        token_start,
        token_end,
        type_info,
        privacy,
        unit,
    })
}
