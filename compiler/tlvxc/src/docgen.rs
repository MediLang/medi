use std::path::{Path, PathBuf};

use tlvxc_ast::ast::{ProgramNode, StatementNode};
use tlvxc_ast::visit::Span;
use tlvxc_lexer::lexer::Lexer;
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program_with_diagnostics, TokenSlice};

use tlvxc_parser::parser::{Diagnostic, Severity};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocCommentKind {
    Outer,
    Inner,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocComment {
    pub kind: DocCommentKind,
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocItemKind {
    Function,
    Type,
    Regulate,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocItem {
    pub kind: DocItemKind,
    pub name: String,
    pub doc: Option<DocComment>,
    pub span: Span,
    pub source_path: Option<PathBuf>,
}

pub struct DocWarnings {
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocLintKind {
    MissingDocs,
    MissingDocExamples,
    BrokenIntraDocLink,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocLint {
    pub kind: DocLintKind,
    pub diagnostic: Diagnostic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocTestMode {
    Run,
    NoRun,
    Ignore,
    CompileFail,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocTestCase {
    pub item_name: String,
    pub mode: DocTestMode,
    pub source: String,
}

pub fn extract_doc_tests(items: &[DocItem]) -> Vec<DocTestCase> {
    let mut out: Vec<DocTestCase> = Vec::new();
    for it in items {
        let Some(doc) = &it.doc else {
            continue;
        };
        let tests = extract_doc_tests_from_text(&it.name, &doc.text);
        out.extend(tests);
    }
    out
}

fn extract_doc_tests_from_text(item_name: &str, text: &str) -> Vec<DocTestCase> {
    let mut out: Vec<DocTestCase> = Vec::new();
    let mut lines = text.lines().peekable();

    while let Some(line) = lines.next() {
        let t = line.trim_start();
        if let Some(info) = t.strip_prefix("```") {
            let info = info.trim();
            if !info.starts_with("tolvex") {
                continue;
            }

            let mode = parse_doc_test_mode(info);
            if mode == DocTestMode::Ignore {
                // Skip until closing fence
                for l in lines.by_ref() {
                    if l.trim_start().starts_with("```") {
                        break;
                    }
                }
                continue;
            }

            let mut body_lines: Vec<String> = Vec::new();
            for body in lines.by_ref() {
                if body.trim_start().starts_with("```") {
                    break;
                }
                if let Some(rest) = body.strip_prefix("# ") {
                    body_lines.push(rest.to_string());
                } else {
                    body_lines.push(body.to_string());
                }
            }

            let src = body_lines.join("\n");
            out.push(DocTestCase {
                item_name: item_name.to_string(),
                mode,
                source: src,
            });
        }
    }

    out
}

fn parse_doc_test_mode(info: &str) -> DocTestMode {
    // Accepted:
    // - medi
    // - medi,no_run
    // - medi,ignore
    // - medi,compile_fail
    // Also allow ```medi,no_run``` (no comma space) and ```medi no_run```.
    let mut flags: Vec<&str> = Vec::new();
    if let Some(rest) = info.strip_prefix("tolvex") {
        let rest = rest.trim();
        if let Some(rest) = rest.strip_prefix(',') {
            flags.extend(rest.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()));
        } else if !rest.is_empty() {
            flags.extend(rest.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()));
        }
    }

    if flags.contains(&"ignore") {
        return DocTestMode::Ignore;
    }
    if flags.contains(&"compile_fail") {
        return DocTestMode::CompileFail;
    }
    if flags.contains(&"no_run") {
        return DocTestMode::NoRun;
    }
    DocTestMode::Run
}

pub fn extract_doc_items_from_source(
    source: &str,
    source_path: Option<&Path>,
) -> Result<(Vec<DocComment>, Vec<DocItem>), String> {
    let comments = extract_doc_comments(source);

    let tokens: Vec<Token> = Lexer::new(source).collect();
    let input = TokenSlice::new(&tokens);
    let program: ProgramNode = parse_program_with_diagnostics(input)
        .map_err(|diag| format!("failed to parse program for docs: {diag:?}"))?;

    let mut items: Vec<DocItem> = Vec::new();
    for stmt in program.statements.iter() {
        if let Some((kind, name, span)) = top_level_item_info(stmt) {
            let doc = find_attached_doc_comment(&comments, source, span.start);
            items.push(DocItem {
                kind,
                name,
                doc,
                span,
                source_path: source_path.map(|p| p.to_path_buf()),
            });
        }
    }

    Ok((comments, items))
}

pub fn resolve_intra_doc_links(items: &mut [DocItem]) -> DocWarnings {
    let mut warnings: Vec<String> = Vec::new();
    let mut anchors: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for it in items.iter() {
        anchors.insert(it.name.clone(), anchor_for_name(&it.name));
    }

    for it in items.iter_mut() {
        let Some(doc) = it.doc.as_mut() else {
            continue;
        };
        let (new_text, unresolved) = rewrite_intra_doc_links(&doc.text, &anchors);
        doc.text = new_text;
        for name in unresolved {
            warnings.push(format!(
                "unresolved intra-doc link [`{name}`] in item '{}'",
                it.name
            ));
        }
    }

    DocWarnings { warnings }
}

pub fn lint_doc_items(items: &[DocItem]) -> Vec<DocLint> {
    let mut out: Vec<DocLint> = Vec::new();

    // Build set of names that are linkable.
    let mut anchors: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for it in items.iter() {
        anchors.insert(it.name.clone(), anchor_for_name(&it.name));
    }

    for it in items {
        if it.doc.is_none() {
            out.push(DocLint {
                kind: DocLintKind::MissingDocs,
                diagnostic: Diagnostic::at_span(it.span, format!("missing docs for '{}'", it.name))
                    .with_severity(Severity::Warning)
                    .with_help("Add a doc comment (///) describing this item"),
            });
            continue;
        }

        let doc = it.doc.as_ref().unwrap();
        if !doc_contains_examples(&doc.text) {
            out.push(DocLint {
                kind: DocLintKind::MissingDocExamples,
                diagnostic: Diagnostic::at_span(
                    it.span,
                    format!("missing doc examples for '{}'", it.name),
                )
                .with_severity(Severity::Warning)
                .with_help("Include an 'Examples' section with a ```medi code block"),
            });
        }

        for name in find_unresolved_intra_doc_link_names(&doc.text, &anchors) {
            out.push(DocLint {
                kind: DocLintKind::BrokenIntraDocLink,
                diagnostic: Diagnostic::at_span(
                    it.span,
                    format!("broken intra-doc link [`{name}`] in '{}'", it.name),
                )
                .with_severity(Severity::Warning)
                .with_help("Ensure the referenced item exists in this file or update the link"),
            });
        }
    }

    out
}

fn doc_contains_examples(text: &str) -> bool {
    // Minimal heuristic:
    // - any medi code fence, or
    // - mentions "examples" somewhere
    let lower = text.to_ascii_lowercase();
    if lower.contains("```medi") {
        return true;
    }
    lower.contains("examples")
}

fn find_unresolved_intra_doc_link_names(
    text: &str,
    anchors: &std::collections::HashMap<String, String>,
) -> Vec<String> {
    let mut unresolved: Vec<String> = Vec::new();
    let bytes = text.as_bytes();
    let mut i: usize = 0;
    while i < bytes.len() {
        if bytes[i] == b'[' && i + 1 < bytes.len() && bytes[i + 1] == b'`' {
            if let Some(end) = find_backtick_bracket_end(bytes, i + 2) {
                let name = &text[i + 2..end];
                if !anchors.contains_key(name) {
                    unresolved.push(name.to_string());
                }
                i = end + 2;
                continue;
            }
        }
        i += 1;
    }
    unresolved
}

fn anchor_for_name(name: &str) -> String {
    // GitHub-flavored Markdown-ish: lowercase, spaces to '-', keep alnum and '-' only.
    let mut out = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
        } else if ch == ' ' || ch == '-' || ch == '_' {
            out.push('-');
        }
    }
    while out.contains("--") {
        out = out.replace("--", "-");
    }
    out.trim_matches('-').to_string()
}

fn rewrite_intra_doc_links(
    text: &str,
    anchors: &std::collections::HashMap<String, String>,
) -> (String, Vec<String>) {
    // Minimal rustdoc-like support: only resolves patterns of the form [`Name`].
    // This avoids pulling in a regex dependency.
    let mut out = String::with_capacity(text.len());
    let mut unresolved: Vec<String> = Vec::new();
    let bytes = text.as_bytes();
    let mut i: usize = 0;

    while i < bytes.len() {
        if bytes[i] == b'[' && i + 1 < bytes.len() && bytes[i + 1] == b'`' {
            // Find closing `]
            if let Some(end) = find_backtick_bracket_end(bytes, i + 2) {
                let name = &text[i + 2..end];
                if let Some(anchor) = anchors.get(name) {
                    out.push_str("[`");
                    out.push_str(name);
                    out.push_str("`](#");
                    out.push_str(anchor);
                    out.push(')');
                } else {
                    unresolved.push(name.to_string());
                    out.push_str(&text[i..end + 2]);
                }
                i = end + 2;
                continue;
            }
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    (out, unresolved)
}

fn find_backtick_bracket_end(bytes: &[u8], start: usize) -> Option<usize> {
    // bytes[start..] begins after the opening backtick.
    // We want the index of the closing backtick (not inclusive) such that the sequence is `]
    let mut j = start;
    while j + 1 < bytes.len() {
        if bytes[j] == b'`' && bytes[j + 1] == b']' {
            return Some(j);
        }
        j += 1;
    }
    None
}

fn top_level_item_info(stmt: &StatementNode) -> Option<(DocItemKind, String, Span)> {
    match stmt {
        StatementNode::Function(node) => {
            Some((DocItemKind::Function, node.name.name.to_string(), node.span))
        }
        StatementNode::TypeDecl(node) => {
            Some((DocItemKind::Type, node.name.name.to_string(), node.span))
        }
        StatementNode::Regulate(node) => Some((
            DocItemKind::Regulate,
            node.standard.name.to_string(),
            node.span,
        )),
        _ => None,
    }
}

fn find_attached_doc_comment(
    comments: &[DocComment],
    source: &str,
    item_start_offset: usize,
) -> Option<DocComment> {
    let mut best: Option<&DocComment> = None;
    for c in comments {
        if c.kind != DocCommentKind::Outer {
            continue;
        }
        if c.span.end > item_start_offset {
            continue;
        }
        // Only attach if there is only whitespace between end of comment and start of item
        if !source[c.span.end..item_start_offset].trim().is_empty() {
            continue;
        }
        if let Some(prev) = best {
            if c.span.end <= prev.span.end {
                continue;
            }
        }
        best = Some(c);
    }
    best.cloned()
}

pub fn extract_doc_comments(source: &str) -> Vec<DocComment> {
    let mut out: Vec<DocComment> = Vec::new();

    let mut offset: usize = 0;
    let bytes = source.as_bytes();
    let mut line_no: u32 = 1;

    while offset < bytes.len() {
        // Scan line starts for line doc comments
        let line_start = offset;
        let mut line_end = offset;
        while line_end < bytes.len() && bytes[line_end] != b'\n' {
            line_end += 1;
        }

        let line = &source[line_start..line_end];
        let trimmed = line.trim_start();
        let ws_prefix_len = line.len() - trimmed.len();
        let col_no: u32 = (ws_prefix_len as u32) + 1;

        // Merge contiguous line doc comments into a single DocComment
        if trimmed.starts_with("///") || trimmed.starts_with("//!") {
            let kind = if trimmed.starts_with("//!") {
                DocCommentKind::Inner
            } else {
                DocCommentKind::Outer
            };
            let start_offset = line_start + ws_prefix_len;
            let start_line = line_no;
            let start_col = col_no;

            let mut text_lines: Vec<String> = Vec::new();
            let mut cur_offset = line_start;
            let mut cur_line_no = line_no;
            let mut end_offset = line_end;

            loop {
                let cur_line_start = cur_offset;
                let mut cur_line_end = cur_offset;
                while cur_line_end < bytes.len() && bytes[cur_line_end] != b'\n' {
                    cur_line_end += 1;
                }
                let cur_line = &source[cur_line_start..cur_line_end];
                let cur_trimmed = cur_line.trim_start();
                if kind == DocCommentKind::Outer {
                    if let Some(rest) = cur_trimmed.strip_prefix("///") {
                        text_lines.push(rest.trim_start().to_string());
                    } else {
                        break;
                    }
                } else if let Some(rest) = cur_trimmed.strip_prefix("//!") {
                    text_lines.push(rest.trim_start().to_string());
                } else {
                    break;
                }

                end_offset = cur_line_end;
                if cur_line_end >= bytes.len() {
                    break;
                }
                // next line
                cur_offset = cur_line_end + 1;
                cur_line_no += 1;

                if cur_offset >= bytes.len() {
                    break;
                }
                // ensure the next line is the same kind to continue merging
                let next_line_start = cur_offset;
                let mut next_line_end = cur_offset;
                while next_line_end < bytes.len() && bytes[next_line_end] != b'\n' {
                    next_line_end += 1;
                }
                let next_line = &source[next_line_start..next_line_end];
                let next_trimmed = next_line.trim_start();
                let is_next = if kind == DocCommentKind::Outer {
                    next_trimmed.starts_with("///")
                } else {
                    next_trimmed.starts_with("//!")
                };
                if !is_next {
                    break;
                }
            }

            out.push(DocComment {
                kind,
                span: Span {
                    start: start_offset,
                    end: end_offset,
                    line: start_line,
                    column: start_col,
                },
                text: text_lines.join("\n"),
            });

            // advance cursor to after the merged block
            offset = if end_offset < bytes.len() && bytes.get(end_offset) == Some(&b'\n') {
                end_offset + 1
            } else {
                end_offset
            };
            line_no = cur_line_no;
            if offset > 0 && offset <= bytes.len() && bytes.get(offset - 1) == Some(&b'\n') {
                line_no += 1;
            }
            continue;
        }

        // Scan for block doc comments at (trimmed) line start
        if trimmed.starts_with("/**") || trimmed.starts_with("/*!") {
            let kind = if trimmed.starts_with("/*!") {
                DocCommentKind::Inner
            } else {
                DocCommentKind::Outer
            };
            let block_start = line_start + ws_prefix_len;
            // Find end '*/'
            if let Some(rel_end) = source[block_start..].find("*/") {
                let end = block_start + rel_end + 2;
                let raw = &source[block_start + 3..end - 2];
                let text = normalize_block_doc_text(raw);
                out.push(DocComment {
                    kind,
                    span: Span {
                        start: block_start,
                        end,
                        line: line_no,
                        column: col_no,
                    },
                    text,
                });

                // advance offset and line_no accounting for newlines inside the block
                let consumed = &source[line_start..end];
                line_no += consumed.as_bytes().iter().filter(|b| **b == b'\n').count() as u32;
                offset = end;
                if offset < bytes.len() && bytes[offset] == b'\n' {
                    offset += 1;
                    line_no += 1;
                }
                continue;
            }
        }

        offset = if line_end < bytes.len() {
            line_end + 1
        } else {
            line_end
        };
        line_no += 1;
    }

    out
}

fn normalize_block_doc_text(raw: &str) -> String {
    // Strip a leading newline if present and common leading '* ' patterns.
    let raw = raw.strip_prefix('\n').unwrap_or(raw);
    let mut lines: Vec<&str> = raw.lines().collect();
    // Remove leading '*' optionally preceded by whitespace.
    for line in &mut lines {
        let t = line.trim_start();
        if let Some(rest) = t.strip_prefix('*') {
            *line = rest.strip_prefix(' ').unwrap_or(rest);
        }
    }
    lines.join("\n").trim().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_line_doc_comments() {
        let src = "/// hello\nfn a() {}\n//! crate\n";
        let comments = extract_doc_comments(src);
        assert_eq!(comments.len(), 2);
        assert_eq!(comments[0].kind, DocCommentKind::Outer);
        assert_eq!(comments[0].text, "hello");
        assert_eq!(comments[1].kind, DocCommentKind::Inner);
        assert_eq!(comments[1].text, "crate");
    }

    #[test]
    fn extracts_block_doc_comments() {
        let src = "/**\n * hello\n * world\n */\nfn a() {}\n";
        let comments = extract_doc_comments(src);
        assert_eq!(comments.len(), 1);
        assert_eq!(comments[0].kind, DocCommentKind::Outer);
        assert_eq!(comments[0].text, "hello\nworld");
    }

    #[test]
    fn attaches_outer_doc_to_next_item() {
        let src = "/// docs\nfn a() { }\n";
        let (_comments, items) = extract_doc_items_from_source(src, None).unwrap();
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "a");
        assert!(items[0].doc.is_some());
        assert_eq!(items[0].doc.as_ref().unwrap().text, "docs");
    }

    #[test]
    fn resolves_intra_doc_links_to_same_file_items() {
        let src = "/// See [`b`]\nfn a() { }\n\n/// B docs\nfn b() { }\n";
        let (_comments, mut items) = extract_doc_items_from_source(src, None).unwrap();
        let warnings = resolve_intra_doc_links(&mut items);
        assert!(warnings.warnings.is_empty());
        let a_doc = items
            .iter()
            .find(|i| i.name == "a")
            .and_then(|i| i.doc.as_ref())
            .unwrap();
        assert_eq!(a_doc.text, "See [`b`](#b)");
    }

    #[test]
    fn warns_on_unresolved_intra_doc_links() {
        let src = "/// See [`nope`]\nfn a() { }\n";
        let (_comments, mut items) = extract_doc_items_from_source(src, None).unwrap();
        let warnings = resolve_intra_doc_links(&mut items);
        assert_eq!(warnings.warnings.len(), 1);
    }

    #[test]
    fn extracts_medi_doc_tests() {
        let src = "/// Example\n/// ```medi\n/// let x = 1;\n/// ```\nfn a() { }\n";
        let (_comments, items) = extract_doc_items_from_source(src, None).unwrap();
        let tests = extract_doc_tests(&items);
        assert_eq!(tests.len(), 1);
        assert_eq!(tests[0].mode, DocTestMode::Run);
        assert!(tests[0].source.contains("let x = 1;"));
    }

    #[test]
    fn supports_doc_test_modifiers_and_hidden_lines() {
        let src = "/// Example\n/// ```medi,no_run\n/// # let hidden = 1;\n/// let x = hidden;\n/// ```\nfn a() { }\n";
        let (_comments, items) = extract_doc_items_from_source(src, None).unwrap();
        let tests = extract_doc_tests(&items);
        assert_eq!(tests.len(), 1);
        assert_eq!(tests[0].mode, DocTestMode::NoRun);
        assert!(tests[0].source.contains("let hidden = 1;"));
        assert!(!tests[0].source.contains("# let hidden"));
    }

    #[test]
    fn lints_missing_docs_and_examples_and_broken_links() {
        let src = "fn a() { }\n\n/// See [`nope`]\nfn b() { }\n";
        let (_comments, items) = extract_doc_items_from_source(src, None).unwrap();
        let lints = lint_doc_items(&items);

        assert!(lints.iter().any(|l| l.kind == DocLintKind::MissingDocs));
        assert!(lints
            .iter()
            .any(|l| l.kind == DocLintKind::MissingDocExamples));
        assert!(lints
            .iter()
            .any(|l| l.kind == DocLintKind::BrokenIntraDocLink));
    }
}
