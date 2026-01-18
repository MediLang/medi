use clap::{Args, Parser, Subcommand};
use medic_borrowck::BorrowChecker;
use medic_borrowck::RtConstraintChecker;
use medic_env::env::TypeEnv;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{
    parse_program_recovering, parse_program_with_diagnostics, render_snippet, TokenSlice,
};
use medic_runtime::{init_gc_with_params, maybe_incremental_step, GcParams};
use medic_typeck::{compliance::check_compliance, type_checker::TypeChecker};
use serde_json::Value as JsonValue;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::path::PathBuf;
use std::process::Command as ProcessCommand;

mod docgen;

#[cfg(feature = "llvm-backend")]
use medic_ast::ast::ProgramNode;

#[cfg(feature = "llvm-backend")]
use medic_codegen_llvm::{
    generate_ir_string_with_types_and_specs, generate_riscv32_object_with_opts_types_and_specs,
    generate_wasm32_unknown_object_with_opts_types_and_specs,
    generate_wasm32_wasi_object_with_opts_types_and_specs,
    generate_x86_64_object_with_opts_types_and_specs, TargetKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputMode {
    Text,
    Json,
}

fn run_test(args: &TestArgs) -> i32 {
    if !args.doc {
        eprintln!("error: no test mode selected (try: medic test --doc <files>)");
        return 2;
    }
    if args.inputs.is_empty() {
        eprintln!("error: no input files provided");
        return 2;
    }

    let mut failures: Vec<String> = Vec::new();
    let mut total: usize = 0;

    for input in &args.inputs {
        let src = match fs::read_to_string(input) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("failed to read '{}': {e}", input.display()));
                continue;
            }
        };

        let (_comments, mut items) = match docgen::extract_doc_items_from_source(&src, Some(input))
        {
            Ok(v) => v,
            Err(e) => {
                failures.push(format!(
                    "failed to extract docs from '{}': {e}",
                    input.display()
                ));
                continue;
            }
        };

        // Link rewrite isn't required for tests, but helps ensure we don't accidentally
        // break link parsing as docgen evolves.
        let warnings = docgen::resolve_intra_doc_links(&mut items);
        for w in warnings.warnings {
            eprintln!("warning: {w}");
        }

        let tests = docgen::extract_doc_tests(&items);
        for (i, t) in tests.into_iter().enumerate() {
            total += 1;
            let ok = match t.mode {
                docgen::DocTestMode::Ignore => true,
                docgen::DocTestMode::CompileFail => run_doctest_compile(&t.source).is_err(),
                docgen::DocTestMode::NoRun | docgen::DocTestMode::Run => {
                    run_doctest_compile(&t.source).is_ok()
                }
            };
            if !ok {
                failures.push(format!(
                    "doctest failed: file='{}' item='{}' case={} mode={:?}",
                    input.display(),
                    t.item_name,
                    i + 1,
                    t.mode
                ));
            }
        }
    }

    if failures.is_empty() {
        if total == 0 {
            eprintln!("warning: no documentation tests found");
        }
        return 0;
    }

    for f in failures {
        eprintln!("error: {f}");
    }
    1
}

fn run_doctest_compile(source: &str) -> Result<(), String> {
    let tokens: Vec<Token> = Lexer::new(source).collect();
    let input = TokenSlice::new(&tokens);
    let program = parse_program_with_diagnostics(input).map_err(|diag| format!("{diag:?}"))?;

    let mut env = TypeEnv::with_prelude();
    let mut checker = TypeChecker::new(&mut env);
    let mut errs = checker.check_program(&program);
    errs.extend(checker.take_errors());
    if errs.is_empty() {
        Ok(())
    } else {
        let joined = errs
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        Err(joined)
    }
}

#[derive(Debug, Parser)]
#[command(
    name = "medic",
    version,
    author = "MediLang Team",
    about = "The Medi language compiler for healthcare-safe programming",
    long_about = "medic is the command-line compiler for the Medi programming language.\n\n\
        Medi is designed for healthcare applications with built-in HIPAA compliance,\n\
        privacy-aware type checking, and clinician-friendly diagnostics.\n\n\
        EXAMPLES:\n\
        \n  medic check patient_records.medi          Check a Medi source file\n\
        \n  medic check --emit=x86_64 -o out.o app.medi   Compile to x86_64 object\n\
        \n  medic check --emit=wasm32 -o app.wasm app.medi Compile to WebAssembly\n\
        \n  medic repl                                 Start interactive REPL\n\
        \n  echo 'let x = 1;' | medic check            Check code from stdin",
    after_help = "For more information, visit: https://github.com/MediLang/medi"
)]
struct Cli {
    /// Increase verbosity level (-v, -vv, -vvv)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Check and optionally compile a Medi source file
    #[command(
        about = "Check and optionally compile a Medi source file",
        long_about = "Parses, type-checks, and optionally compiles a Medi source file.\n\n\
            By default, reads from the specified file or stdin if no file is given.\n\
            Use --emit to compile to a specific target architecture."
    )]
    Check(CheckArgs),

    /// Output diagnostics and analysis as JSON
    #[command(about = "Output diagnostics and analysis as JSON for IDE integration")]
    Json(CheckArgs),

    /// Start an interactive Read-Eval-Print Loop
    #[command(
        about = "Start an interactive REPL session",
        long_about = "Start an interactive Read-Eval-Print Loop for experimenting with Medi code.\n\n\
            Commands:\n\
            \n  :help   Show available REPL commands\n\
            \n  :quit   Exit the REPL (also :q, :exit)"
    )]
    Repl,

    /// Generate documentation from Medi source files
    #[command(about = "Generate Markdown documentation from doc comments")]
    Docs(DocsArgs),

    /// Run tests (including documentation tests)
    #[command(about = "Run Medi tests")]
    Test(TestArgs),

    /// Manage Medi packages and dependencies
    #[command(about = "Manage Medi packages and dependencies")]
    Pack(PackArgs),
}

#[derive(Debug, Args, Clone)]
struct CheckArgs {
    /// Input Medi source file (reads from stdin if not provided)
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// JSON file with external type definitions for FFI functions
    #[arg(long = "types-json", value_name = "FILE")]
    types_json: Option<PathBuf>,

    /// Target architecture to compile to (x86_64, wasm32, wasm32-unknown, riscv32)
    #[cfg(feature = "llvm-backend")]
    #[arg(
        long = "emit",
        visible_alias = "target",
        value_name = "TARGET",
        value_parser = ["x86_64", "wasm32", "wasm32-unknown", "riscv32"]
    )]
    emit: Option<String>,

    /// Output file path for compiled artifacts
    #[cfg(feature = "llvm-backend")]
    #[arg(short = 'o', long = "out", value_name = "FILE")]
    out: Option<PathBuf>,

    /// Optimization level (0-3, default: 2 for release, 1 for debug)
    #[cfg(feature = "llvm-backend")]
    #[arg(long = "opt", value_name = "LEVEL", value_parser = clap::value_parser!(u8).range(0..=3))]
    opt: Option<u8>,

    /// Target CPU model (e.g., 'x86-64', 'generic')
    #[cfg(feature = "llvm-backend")]
    #[arg(long = "cpu", value_name = "CPU")]
    cpu: Option<String>,

    /// Target CPU features (comma-separated, e.g., '+sse4.2,+avx')
    #[cfg(feature = "llvm-backend")]
    #[arg(long = "features", value_name = "FEATURES")]
    features: Option<String>,

    /// Optimization pipeline preset (default, minimal, aggressive, debug)
    #[cfg(feature = "llvm-backend")]
    #[arg(
        long = "opt-pipeline",
        value_name = "PIPELINE",
        value_parser = ["default", "minimal", "aggressive", "debug"]
    )]
    opt_pipeline: Option<String>,
}

#[derive(Debug, Args, Clone)]
struct DocsArgs {
    /// Output directory for generated documentation (default: docs_out)
    #[arg(long = "out-dir", visible_alias = "output", value_name = "DIR")]
    out_dir: Option<PathBuf>,

    /// Delete the output directory before generating documentation
    #[arg(long = "clean")]
    clean: bool,

    /// Open generated documentation in the default system viewer
    #[arg(long = "open")]
    open: bool,

    /// Version tag for generated docs (writes into out_dir/<version>/ and updates out_dir/latest)
    #[arg(long = "version", value_name = "TAG")]
    version: Option<String>,

    /// Output format for generated documentation (md or html)
    #[arg(long = "output-format", value_name = "FORMAT", default_value = "md", value_parser = ["md", "html", "json"])]
    output_format: String,

    /// Medi source files to generate documentation from
    #[arg(value_name = "FILES")]
    inputs: Vec<PathBuf>,
}

#[derive(Debug, Args, Clone)]
struct TestArgs {
    /// Run documentation tests extracted from doc comments
    #[arg(long = "doc")]
    doc: bool,

    /// Medi source files to test (for --doc)
    #[arg(value_name = "FILES")]
    inputs: Vec<PathBuf>,
}

#[derive(Debug, Args, Clone)]
struct PackArgs {
    #[command(subcommand)]
    command: Option<PackCommand>,
}

#[derive(Debug, Subcommand, Clone)]
enum PackCommand {
    /// Initialize a new medipack.toml manifest
    Init,
    /// List dependencies from medipack.toml
    List,
}

#[derive(Debug, Clone, PartialEq)]
enum ReplValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
}

impl std::fmt::Display for ReplValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplValue::Int(n) => write!(f, "{n}"),
            ReplValue::Float(x) => write!(f, "{x}"),
            ReplValue::Bool(b) => write!(f, "{b}"),
            ReplValue::String(s) => write!(f, "{s}"),
            ReplValue::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug)]
struct ReplSession {
    buffer: String,
    accumulated_source: String,
    env: medic_env::env::TypeEnv,
    values: std::collections::HashMap<String, ReplValue>,
    prelude_names: std::collections::HashSet<String>,
}

impl ReplSession {
    fn new() -> Self {
        let prelude = medic_env::env::TypeEnv::with_prelude();
        let prelude_names: std::collections::HashSet<String> = prelude
            .collect_symbol_types()
            .into_iter()
            .map(|(n, _)| n)
            .collect();
        Self {
            buffer: String::new(),
            accumulated_source: String::new(),
            env: medic_env::env::TypeEnv::with_prelude(),
            values: std::collections::HashMap::new(),
            prelude_names,
        }
    }

    fn prompt(&self) -> &'static str {
        if self.buffer.is_empty() {
            "medic> "
        } else {
            "....> "
        }
    }

    fn is_complete_input(s: &str) -> bool {
        let mut paren: i32 = 0;
        let mut brace: i32 = 0;
        let mut bracket: i32 = 0;
        let mut in_str: bool = false;
        let mut prev_backslash = false;

        for ch in s.chars() {
            if in_str {
                if prev_backslash {
                    prev_backslash = false;
                    continue;
                }
                if ch == '\\' {
                    prev_backslash = true;
                    continue;
                }
                if ch == '"' {
                    in_str = false;
                }
                continue;
            }

            match ch {
                '"' => in_str = true,
                '(' => paren += 1,
                ')' => paren -= 1,
                '{' => brace += 1,
                '}' => brace -= 1,
                '[' => bracket += 1,
                ']' => bracket -= 1,
                _ => {}
            }
        }

        if in_str {
            return false;
        }
        if paren != 0 || brace != 0 || bracket != 0 {
            return false;
        }
        let trimmed = s.trim_end();
        trimmed.ends_with(';') || trimmed.ends_with('}')
    }

    fn handle_command(&mut self, line: &str) -> (Vec<String>, bool) {
        let trimmed = line.trim();
        if trimmed == ":help" {
            return (
                vec![
                    "commands: :help, :quit, :load <file>, :vars".to_string(),
                    "note: end statements with ';' or close blocks with '}'".to_string(),
                ],
                false,
            );
        }

        if trimmed == ":q" || trimmed == ":quit" || trimmed == ":exit" {
            return (Vec::new(), true);
        }

        if let Some(rest) = trimmed.strip_prefix(":load") {
            let path_s = rest.trim();
            if path_s.is_empty() {
                return (vec!["error: usage: :load <file>".to_string()], false);
            }
            let p = std::path::Path::new(path_s);
            let text = match std::fs::read_to_string(p) {
                Ok(t) => t,
                Err(e) => {
                    return (
                        vec![format!(
                            "error: failed to read '{}': {e}",
                            p.to_string_lossy()
                        )],
                        false,
                    )
                }
            };
            let (out, _ok) = self.submit_source(&text);
            (out, false)
        } else if trimmed == ":vars" {
            let mut lines = Vec::new();
            let mut syms: Vec<(String, medic_type::types::MediType)> = self
                .env
                .collect_symbol_types()
                .into_iter()
                .filter(|(n, _)| !self.prelude_names.contains(n))
                .collect();
            syms.sort_by(|a, b| a.0.cmp(&b.0));
            if syms.is_empty() {
                lines.push("(no session bindings)".to_string());
            } else {
                for (n, t) in syms {
                    lines.push(format!("{n}: {t:?}"));
                }
            }
            (lines, false)
        } else {
            (vec![format!("error: unknown command '{trimmed}'")], false)
        }
    }

    fn handle_line(&mut self, line: &str) -> (Vec<String>, bool, bool) {
        let trimmed = line.trim();
        if self.buffer.is_empty() && trimmed.starts_with(':') {
            let (out, exit) = self.handle_command(trimmed);
            return (out, exit, true);
        }

        if trimmed.is_empty() {
            return (Vec::new(), false, false);
        }

        self.buffer.push_str(line);
        self.buffer.push('\n');
        if Self::is_complete_input(&self.buffer) {
            let code = std::mem::take(&mut self.buffer);
            let (out, _ok) = self.submit_source(&code);
            return (out, false, true);
        }
        (Vec::new(), false, false)
    }

    fn submit_source(&mut self, src: &str) -> (Vec<String>, bool) {
        let mut full = String::new();
        if !self.accumulated_source.is_empty() {
            full.push_str(&self.accumulated_source);
            if !self.accumulated_source.ends_with('\n') {
                full.push('\n');
            }
        }
        full.push_str(src);

        let tokens: Vec<medic_lexer::token::Token> =
            medic_lexer::lexer::Lexer::new(&full).collect();
        let input = medic_parser::parser::TokenSlice::new(&tokens);

        let mut out = Vec::new();
        let program: medic_ast::ast::ProgramNode =
            match medic_parser::parser::parse_program_with_diagnostics(input) {
                Ok(p) => p,
                Err(diag) => {
                    out.push(medic_parser::parser::render_snippet(&diag, &full));
                    return (out, false);
                }
            };

        let mut diags = Vec::new();
        let _ = medic_parser::parser::parse_program_recovering(
            medic_parser::parser::TokenSlice::new(&tokens),
            &mut diags,
        );
        for d in diags {
            out.push(medic_parser::parser::render_snippet(&d, &full));
        }

        let mut trial_env = self.env.clone();
        let mut checker = medic_typeck::type_checker::TypeChecker::new(&mut trial_env);
        let mut errs = checker.check_program(&program);
        errs.extend(checker.take_errors());
        if !errs.is_empty() {
            for e in errs {
                out.push(format!("type error: {e}"));
            }
            return (out, false);
        }

        let mut last_value: Option<ReplValue> = None;
        for stmt in &program.statements {
            match self.eval_stmt(stmt) {
                Ok(v) => last_value = Some(v),
                Err(e) => {
                    out.push(format!("runtime error: {e}"));
                    return (out, false);
                }
            }
        }

        self.env = trial_env;
        self.accumulated_source = full;
        if let Some(v) = last_value {
            if v != ReplValue::Unit {
                out.push(v.to_string());
            } else {
                out.push("ok".to_string());
            }
        } else {
            out.push("ok".to_string());
        }
        (out, true)
    }

    fn eval_stmt(&mut self, stmt: &medic_ast::ast::StatementNode) -> Result<ReplValue, String> {
        match stmt {
            medic_ast::ast::StatementNode::Let(ls) => {
                let v = if let Some(expr) = &ls.value {
                    self.eval_expr(expr)?
                } else {
                    ReplValue::Unit
                };
                self.values.insert(ls.name.name().to_string(), v);
                Ok(ReplValue::Unit)
            }
            medic_ast::ast::StatementNode::Assignment(assign) => {
                if let medic_ast::ast::ExpressionNode::Identifier(id) = &assign.target {
                    let v = self.eval_expr(&assign.value)?;
                    self.values.insert(id.node.name.to_string(), v);
                    Ok(ReplValue::Unit)
                } else {
                    Err("unsupported assignment target".to_string())
                }
            }
            medic_ast::ast::StatementNode::Expr(expr) => self.eval_expr(expr),
            _ => Ok(ReplValue::Unit),
        }
    }

    fn eval_expr(&mut self, expr: &medic_ast::ast::ExpressionNode) -> Result<ReplValue, String> {
        match expr {
            medic_ast::ast::ExpressionNode::Literal(lit) => self.eval_lit(&lit.node),
            medic_ast::ast::ExpressionNode::Identifier(id) => self
                .values
                .get(id.node.name.as_str())
                .cloned()
                .ok_or_else(|| format!("unknown identifier '{}'", id.node.name)),
            medic_ast::ast::ExpressionNode::Binary(bin) => {
                let l = self.eval_expr(&bin.node.left)?;
                let r = self.eval_expr(&bin.node.right)?;
                self.eval_binary(bin.node.operator, l, r)
            }
            _ => Ok(ReplValue::Unit),
        }
    }

    fn eval_lit(&self, lit: &medic_ast::ast::LiteralNode) -> Result<ReplValue, String> {
        match lit {
            medic_ast::ast::LiteralNode::Int(n) => Ok(ReplValue::Int(*n)),
            medic_ast::ast::LiteralNode::Float(x) => Ok(ReplValue::Float(*x)),
            medic_ast::ast::LiteralNode::Bool(b) => Ok(ReplValue::Bool(*b)),
            medic_ast::ast::LiteralNode::String(s) => Ok(ReplValue::String(s.clone())),
        }
    }

    fn eval_binary(
        &self,
        op: medic_ast::ast::BinaryOperator,
        l: ReplValue,
        r: ReplValue,
    ) -> Result<ReplValue, String> {
        match op {
            medic_ast::ast::BinaryOperator::Add => match (l, r) {
                (ReplValue::Int(a), ReplValue::Int(b)) => Ok(ReplValue::Int(a + b)),
                (ReplValue::Float(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a + b)),
                (ReplValue::Int(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a as f64 + b)),
                (ReplValue::Float(a), ReplValue::Int(b)) => Ok(ReplValue::Float(a + b as f64)),
                (ReplValue::String(a), ReplValue::String(b)) => Ok(ReplValue::String(a + &b)),
                _ => Err("unsupported '+' operands".to_string()),
            },
            medic_ast::ast::BinaryOperator::Sub => match (l, r) {
                (ReplValue::Int(a), ReplValue::Int(b)) => Ok(ReplValue::Int(a - b)),
                (ReplValue::Float(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a - b)),
                (ReplValue::Int(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a as f64 - b)),
                (ReplValue::Float(a), ReplValue::Int(b)) => Ok(ReplValue::Float(a - b as f64)),
                _ => Err("unsupported '-' operands".to_string()),
            },
            medic_ast::ast::BinaryOperator::Mul => match (l, r) {
                (ReplValue::Int(a), ReplValue::Int(b)) => Ok(ReplValue::Int(a * b)),
                (ReplValue::Float(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a * b)),
                (ReplValue::Int(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a as f64 * b)),
                (ReplValue::Float(a), ReplValue::Int(b)) => Ok(ReplValue::Float(a * b as f64)),
                _ => Err("unsupported '*' operands".to_string()),
            },
            medic_ast::ast::BinaryOperator::Div => match (l, r) {
                (ReplValue::Int(_), ReplValue::Int(0)) => Err("division by zero".to_string()),
                (ReplValue::Int(a), ReplValue::Int(b)) => Ok(ReplValue::Int(a / b)),
                (ReplValue::Float(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a / b)),
                (ReplValue::Int(a), ReplValue::Float(b)) => Ok(ReplValue::Float(a as f64 / b)),
                (ReplValue::Float(a), ReplValue::Int(b)) => Ok(ReplValue::Float(a / b as f64)),
                _ => Err("unsupported '/' operands".to_string()),
            },
            medic_ast::ast::BinaryOperator::Eq => Ok(ReplValue::Bool(l == r)),
            medic_ast::ast::BinaryOperator::Ne => Ok(ReplValue::Bool(l != r)),
            medic_ast::ast::BinaryOperator::Lt => self.eval_cmp(l, r, |a, b| a < b),
            medic_ast::ast::BinaryOperator::Le => self.eval_cmp(l, r, |a, b| a <= b),
            medic_ast::ast::BinaryOperator::Gt => self.eval_cmp(l, r, |a, b| a > b),
            medic_ast::ast::BinaryOperator::Ge => self.eval_cmp(l, r, |a, b| a >= b),
            _ => Ok(ReplValue::Unit),
        }
    }

    fn eval_cmp<F>(&self, l: ReplValue, r: ReplValue, f: F) -> Result<ReplValue, String>
    where
        F: Fn(f64, f64) -> bool,
    {
        let (a, b) = match (l, r) {
            (ReplValue::Int(a), ReplValue::Int(b)) => (a as f64, b as f64),
            (ReplValue::Float(a), ReplValue::Float(b)) => (a, b),
            (ReplValue::Int(a), ReplValue::Float(b)) => (a as f64, b),
            (ReplValue::Float(a), ReplValue::Int(b)) => (a, b as f64),
            _ => return Err("unsupported comparison operands".to_string()),
        };
        Ok(ReplValue::Bool(f(a, b)))
    }
}

fn parse_medi_type_str(s: &str) -> Option<medic_type::types::MediType> {
    use medic_type::types::MediType as MT;
    match s {
        "Int" | "int" => Some(MT::Int),
        "Float" | "float" => Some(MT::Float),
        "Bool" | "bool" => Some(MT::Bool),
        "String" | "string" => Some(MT::String),
        "Void" | "void" => Some(MT::Void),
        "Unknown" | "unknown" => Some(MT::Unknown),
        other if other.starts_with("TypeVar:") => Some(MT::TypeVar(other[8..].to_string())),
        _ => None,
    }
}

#[cfg(test)]
mod repl_tests {
    use super::ReplSession;

    #[test]
    fn repl_help_command() {
        let mut s = ReplSession::new();
        let (out, exit, committed) = s.handle_line(":help");
        assert!(!exit);
        assert!(committed);
        assert!(out.iter().any(|l| l.contains("commands:")));
    }

    #[test]
    fn repl_quit_command() {
        let mut s = ReplSession::new();
        let (_out, exit, _committed) = s.handle_line(":quit");
        assert!(exit);
    }

    #[test]
    fn repl_multiline_then_commit() {
        let mut s = ReplSession::new();
        let (out1, exit1, committed1) = s.handle_line("let x = 1");
        assert!(!exit1);
        assert!(!committed1);
        assert!(out1.is_empty());

        let (out2, exit2, committed2) = s.handle_line(";");
        assert!(!exit2);
        assert!(committed2);
        assert!(!out2.is_empty());
    }

    #[test]
    fn repl_incremental_binding_visible_in_later_input() {
        let mut s = ReplSession::new();
        let (_o1, _e1, c1) = s.handle_line("let x = 2;");
        assert!(c1);
        let (o2, _e2, c2) = s.handle_line("x + 3;");
        assert!(c2);
        assert!(o2.iter().any(|l| l.trim() == "5"));
    }

    #[test]
    fn repl_vars_lists_session_bindings() {
        let mut s = ReplSession::new();
        let (_o1, _e1, _c1) = s.handle_line("let x = 2;");
        let (out, _exit, _committed) = s.handle_line(":vars");
        assert!(out.iter().any(|l| l.starts_with("x:")));
    }
}

fn load_types_json(env: &mut TypeEnv, json_text: &str) -> Result<(), String> {
    let v: JsonValue = serde_json::from_str(json_text).map_err(|e| e.to_string())?;
    // Accept either {"functions":[{"name":"f","params":[..],"return":".."}]} or {"f":{"params":[..],"return":".."}}
    if let Some(funcs) = v.get("functions").and_then(|x| x.as_array()) {
        for f in funcs {
            let name = f
                .get("name")
                .and_then(|n| n.as_str())
                .ok_or("function missing name")?;
            let params = f
                .get("params")
                .and_then(|p| p.as_array())
                .ok_or("function missing params")?;
            let ret = f
                .get("return")
                .and_then(|r| r.as_str())
                .ok_or("function missing return")?;
            let params_mt: Option<Vec<_>> = params
                .iter()
                .map(|s| s.as_str().and_then(parse_medi_type_str))
                .collect();
            let params_mt = params_mt.ok_or("invalid param type string")?;
            let ret_mt = parse_medi_type_str(ret).ok_or("invalid return type string")?;
            env.insert(
                name.to_string(),
                medic_type::types::MediType::Function {
                    params: params_mt,
                    return_type: Box::new(ret_mt),
                    param_privacy: None,
                    return_privacy: None,
                },
            );
        }
        return Ok(());
    }

    if let Some(obj) = v.as_object() {
        for (name, def) in obj {
            if let Some(params) = def.get("params").and_then(|p| p.as_array()) {
                if let Some(ret_s) = def.get("return").and_then(|r| r.as_str()) {
                    let params_mt: Option<Vec<_>> = params
                        .iter()
                        .map(|s| s.as_str().and_then(parse_medi_type_str))
                        .collect();
                    let params_mt = params_mt.ok_or("invalid param type string")?;
                    let ret_mt = parse_medi_type_str(ret_s).ok_or("invalid return type string")?;
                    env.insert(
                        name.to_string(),
                        medic_type::types::MediType::Function {
                            params: params_mt,
                            return_type: Box::new(ret_mt),
                            param_privacy: None,
                            return_privacy: None,
                        },
                    );
                }
            }
        }
        return Ok(());
    }
    Err("unsupported types json format".into())
}

fn init_gc_from_env() {
    // Initialize runtime GC with tunable parameters via environment
    // MEDI_GC_NURSERY_BYTES, MEDI_GC_PROMOTION_THRESHOLD, MEDI_GC_MAX_PAUSE_MS
    let mut gc_params = GcParams::default();
    if let Ok(s) = std::env::var("MEDI_GC_NURSERY_BYTES") {
        if let Ok(n) = s.parse::<usize>() {
            gc_params.nursery_threshold_bytes = n;
        }
    }
    if let Ok(s) = std::env::var("MEDI_GC_PROMOTION_THRESHOLD") {
        if let Ok(n) = s.parse::<usize>() {
            gc_params.gen_promotion_threshold = n;
        }
    }
    if let Ok(s) = std::env::var("MEDI_GC_MAX_PAUSE_MS") {
        if let Ok(n) = s.parse::<u64>() {
            gc_params.max_pause_ms = n;
        }
    }
    let _gc = init_gc_with_params(gc_params);
    maybe_incremental_step();
}

fn read_source_from_input(input: &Option<PathBuf>) -> Result<String, String> {
    if let Some(path) = input {
        fs::read_to_string(path).map_err(|e| format!("failed to read '{}': {e}", path.display()))
    } else {
        let mut buf = String::new();
        io::stdin()
            .read_to_string(&mut buf)
            .map_err(|e| format!("failed to read from stdin: {e}"))?;
        Ok(buf)
    }
}

fn apply_llvm_env_overrides(_args: &CheckArgs) {
    #[cfg(feature = "llvm-backend")]
    {
        if let Some(opt) = _args.opt {
            std::env::set_var("MEDI_LLVM_OPT", opt.to_string());
        }
        if let Some(ref cpu) = _args.cpu {
            std::env::set_var("MEDI_LLVM_CPU", cpu);
        }
        if let Some(ref feats) = _args.features {
            std::env::set_var("MEDI_LLVM_FEATURES", feats);
        }
        if let Some(ref pipe) = _args.opt_pipeline {
            let v = match pipe.as_str() {
                "default" | "minimal" | "aggressive" | "debug" => pipe.as_str(),
                _ => "minimal",
            };
            std::env::set_var("MEDI_LLVM_PIPE", v);
        }
    }
}

#[cfg(feature = "llvm-backend")]
fn parse_emit_target(s: &str) -> Option<TargetKind> {
    match s {
        "x86_64" => Some(TargetKind::X86_64),
        "wasm32" => Some(TargetKind::Wasm32),
        "wasm32-unknown" => Some(TargetKind::Wasm32),
        "riscv32" => Some(TargetKind::RiscV32),
        _ => None,
    }
}

fn run_check(source: &str, types_json_path: &Option<PathBuf>, mode: OutputMode) -> i32 {
    // Tokenize
    let tokens: Vec<Token> = Lexer::new(source).collect();
    let input = TokenSlice::new(&tokens);
    maybe_incremental_step();

    // First, try strict parse that returns a single diagnostic on error
    match parse_program_with_diagnostics(input) {
        Ok(program) => match mode {
            OutputMode::Text => {
                // Also run the recovering path to surface non-fatal diagnostics (e.g., lexer errors)
                let mut diags = Vec::new();
                let _ = parse_program_recovering(TokenSlice::new(&tokens), &mut diags);
                for d in diags {
                    eprintln!("{}", render_snippet(&d, source));
                }

                // Initialize type environment first (so RT checker can consult it)
                let mut env = TypeEnv::with_prelude();
                // Optional: load function types from a JSON file to seed generics like fn(T)->T
                if let Some(ref p) = types_json_path {
                    match fs::read_to_string(p) {
                        Ok(text) => {
                            if let Err(e) = load_types_json(&mut env, &text) {
                                eprintln!(
                                    "warning: failed to load --types-json file '{}': {e}",
                                    p.display()
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!(
                                "warning: cannot read --types-json file '{}': {e}",
                                p.display()
                            );
                        }
                    }
                }

                // Optional: real-time constraint checking (env-gated)
                if std::env::var("MEDI_RT_CHECK").ok().as_deref() == Some("1") {
                    use std::collections::HashSet;
                    // Seed from defaults
                    let mut disallowed: HashSet<String> = [
                        "medi_gc_alloc_string",
                        "medi_gc_collect",
                        "spawn_task",
                        "create_channel",
                    ]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect();
                    // Project-specific RT-unsafe tags via TypeEnv
                    if let Ok(tags) = std::env::var("MEDI_RT_UNSAFE") {
                        for name in tags.split(',') {
                            let s = name.trim();
                            if !s.is_empty() {
                                env.set_rt_unsafe_fn(s);
                            }
                        }
                    }
                    // Extend from MEDI_RT_DISALLOW env var
                    if let Ok(extra) = std::env::var("MEDI_RT_DISALLOW") {
                        for name in extra.split(',') {
                            let s = name.trim();
                            if !s.is_empty() {
                                disallowed.insert(s.to_string());
                            }
                        }
                    }
                    // Extend from TypeEnv rt_unsafe flags for all visible functions
                    for (fname, _ty) in env.collect_function_types() {
                        if env.is_rt_unsafe_fn(&fname) {
                            disallowed.insert(fname);
                        }
                    }
                    let checker = RtConstraintChecker::new(
                        "rt_begin",
                        "rt_end",
                        disallowed.into_iter().collect::<Vec<_>>(),
                    );
                    if let Err(errors) = checker.check_program(&program) {
                        for err in errors {
                            eprintln!("rt check error: {err}");
                        }
                        return 1;
                    }
                }

                let mut checker = TypeChecker::new(&mut env);
                let mut type_errors = checker.check_program(&program);
                // Translate privacy/type policy issues into compliance violations
                let compliance_violations = check_compliance(&checker, &program);
                maybe_incremental_step();
                // Include any collected validation/type errors from expression checks
                type_errors.extend(checker.take_errors());

                if !type_errors.is_empty() {
                    for err in &type_errors {
                        eprintln!("type error: {err}");
                    }
                    return 1;
                }

                if !compliance_violations.is_empty() {
                    for v in &compliance_violations {
                        if let Some(span) = v.span {
                            eprintln!(
                                "compliance violation: {} (line {}, col {})",
                                v.message, span.line, span.column
                            );
                        } else {
                            eprintln!("compliance violation: {}", v.message);
                        }
                    }
                    return 1;
                }

                // Run borrow checker after successful type checking
                let privacy_labels = env.collect_privacy_labels();
                let mut bchk = BorrowChecker::with_privacy_labels(privacy_labels);
                if let Err(errors) = bchk.check_program(&program) {
                    for err in errors {
                        eprintln!("borrow error: {err}");
                    }
                    return 1;
                }

                0
            }
            OutputMode::Json => {
                // Use library API to produce structured JSON report
                let report = medic::analyze_source(source);
                match serde_json::to_string_pretty(&report) {
                    Ok(json) => println!("{json}"),
                    Err(e) => {
                        eprintln!("error: failed to serialize JSON: {e}");
                        return 2;
                    }
                }
                if !report.errors.is_empty() {
                    return 1;
                }
                0
            }
        },
        Err(diag) => {
            eprintln!("{}", render_snippet(&diag, source));
            1
        }
    }
}

#[cfg(feature = "llvm-backend")]
fn maybe_emit(
    args: &CheckArgs,
    _source: &str,
    _tokens: &[Token],
    program: &ProgramNode,
    mut env: TypeEnv,
) -> Option<i32> {
    let emit_s = args.emit.as_deref()?;
    let target = match parse_emit_target(emit_s) {
        Some(t) => t,
        None => {
            eprintln!(
                "warning: unknown emit target '{emit_s}', expected x86_64|wasm32|wasm32-unknown|riscv32"
            );
            return Some(2);
        }
    };

    let mut checker = TypeChecker::new(&mut env);
    let mut type_errors = checker.check_program(program);
    let compliance_violations = check_compliance(&checker, program);
    type_errors.extend(checker.take_errors());
    if !type_errors.is_empty() {
        for err in &type_errors {
            eprintln!("type error: {err}");
        }
        return Some(1);
    }
    if !compliance_violations.is_empty() {
        for v in &compliance_violations {
            if let Some(span) = v.span {
                eprintln!(
                    "compliance violation: {} (line {}, col {})",
                    v.message, span.line, span.column
                );
            } else {
                eprintln!("compliance violation: {}", v.message);
            }
        }
        return Some(1);
    }

    let mut bchk = BorrowChecker::new();
    if let Err(errors) = bchk.check_program(program) {
        for err in errors {
            eprintln!("borrow error: {err}");
        }
        return Some(1);
    }

    maybe_incremental_step();
    let specs = checker.collect_function_specializations(program);
    drop(checker);
    let fun_tys = env.collect_function_types();

    let out_path = args.out.as_ref().map(|p| p.to_string_lossy().to_string());
    match target {
        TargetKind::X86_64 => {
            let mut opt = std::env::var("MEDI_LLVM_OPT")
                .ok()
                .and_then(|s| s.parse::<u8>().ok());
            let mut cpu = std::env::var("MEDI_LLVM_CPU").ok();
            let mut feats = std::env::var("MEDI_LLVM_FEATURES").ok();
            if opt.is_none() {
                #[cfg(debug_assertions)]
                {
                    opt = Some(1);
                }
                #[cfg(not(debug_assertions))]
                {
                    opt = Some(3);
                }
            }
            if std::env::var("MEDI_LLVM_PIPE").is_err() {
                #[cfg(debug_assertions)]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "debug");
                }
                #[cfg(not(debug_assertions))]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "default");
                }
            }
            if cpu.is_none() {
                cpu = Some("x86-64".to_string());
            }
            if feats.is_none() {
                feats = Some("".to_string());
            }
            let result = {
                let opt = opt.unwrap_or(2);
                let cpu = cpu.unwrap_or_else(|| "x86-64".to_string());
                let feats = feats.unwrap_or_default();
                generate_x86_64_object_with_opts_types_and_specs(
                    program, opt, &cpu, &feats, &fun_tys, &specs,
                )
            };
            match result {
                Ok(obj) => {
                    if let Some(path) = out_path {
                        if let Some(parent) = std::path::Path::new(&path).parent() {
                            if !parent.as_os_str().is_empty() {
                                if let Err(e) = fs::create_dir_all(parent) {
                                    eprintln!(
                                        "error: failed to create output directory '{}': {e}",
                                        parent.display()
                                    );
                                    return Some(2);
                                }
                            }
                        }
                        if let Err(e) = fs::write(&path, &obj) {
                            eprintln!("error: failed to write object file '{path}': {e}");
                            return Some(2);
                        }
                        let opt_used =
                            std::env::var("MEDI_LLVM_OPT").unwrap_or_else(|_| "2".into());
                        let cpu_used =
                            std::env::var("MEDI_LLVM_CPU").unwrap_or_else(|_| "x86-64".into());
                        let feats_used =
                            std::env::var("MEDI_LLVM_FEATURES").unwrap_or_else(|_| "".into());
                        eprintln!(
                             "wrote x86_64 object to {path} ({} bytes) [opt={}, cpu='{}', features='{}']",
                             obj.len(), opt_used, cpu_used, feats_used
                         );
                        return Some(0);
                    }
                    eprintln!("note: no --out path provided; printing LLVM IR instead");
                    match generate_ir_string_with_types_and_specs(program, &fun_tys, &specs) {
                        Ok(ir) => {
                            println!("{ir}");
                            Some(0)
                        }
                        Err(e) => {
                            eprintln!("error: IR generation failed: {e}");
                            Some(2)
                        }
                    }
                }
                Err(e) => {
                    eprintln!("error: x86_64 code emission failed: {e}");
                    Some(2)
                }
            }
        }
        TargetKind::Wasm32 => {
            let is_unknown = emit_s == "wasm32-unknown";
            let mut opt = std::env::var("MEDI_LLVM_OPT")
                .ok()
                .and_then(|s| s.parse::<u8>().ok());
            if opt.is_none() {
                #[cfg(debug_assertions)]
                {
                    opt = Some(1);
                }
                #[cfg(not(debug_assertions))]
                {
                    opt = Some(3);
                }
            }
            if std::env::var("MEDI_LLVM_PIPE").is_err() {
                #[cfg(debug_assertions)]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "debug");
                }
                #[cfg(not(debug_assertions))]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "default");
                }
            }
            let cpu = std::env::var("MEDI_LLVM_CPU").unwrap_or_else(|_| "generic".to_string());
            let feats = std::env::var("MEDI_LLVM_FEATURES").unwrap_or_else(|_| "".to_string());
            let result = if is_unknown {
                generate_wasm32_unknown_object_with_opts_types_and_specs(
                    program,
                    opt.unwrap_or(2),
                    &cpu,
                    &feats,
                    &fun_tys,
                    &specs,
                )
            } else {
                generate_wasm32_wasi_object_with_opts_types_and_specs(
                    program,
                    opt.unwrap_or(2),
                    &cpu,
                    &feats,
                    &fun_tys,
                    &specs,
                )
            };
            match result {
                Ok(wasm) => {
                    if let Some(path) = out_path {
                        if let Some(parent) = std::path::Path::new(&path).parent() {
                            if !parent.as_os_str().is_empty() {
                                if let Err(e) = fs::create_dir_all(parent) {
                                    eprintln!(
                                        "error: failed to create output directory '{}': {e}",
                                        parent.display()
                                    );
                                    return Some(2);
                                }
                            }
                        }
                        if let Err(e) = fs::write(&path, &wasm) {
                            eprintln!("error: failed to write wasm file '{path}': {e}");
                            return Some(2);
                        }
                        let opt_used =
                            std::env::var("MEDI_LLVM_OPT").unwrap_or_else(|_| "2".into());
                        let cpu_used =
                            std::env::var("MEDI_LLVM_CPU").unwrap_or_else(|_| "generic".into());
                        let feats_used =
                            std::env::var("MEDI_LLVM_FEATURES").unwrap_or_else(|_| "".into());
                        if is_unknown {
                            eprintln!(
                                 "wrote wasm32-unknown-unknown to {path} ({} bytes) [opt={}, cpu='{}', features='{}']",
                                 wasm.len(), opt_used, cpu_used, feats_used
                             );
                        } else {
                            eprintln!(
                                 "wrote wasm32-wasi to {path} ({} bytes) [opt={}, cpu='{}', features='{}']",
                                 wasm.len(), opt_used, cpu_used, feats_used
                             );
                        }
                        return Some(0);
                    }
                    eprintln!("note: no --out path provided; printing LLVM IR instead");
                    match generate_ir_string_with_types_and_specs(program, &fun_tys, &specs) {
                        Ok(ir) => {
                            println!("{ir}");
                            Some(0)
                        }
                        Err(e) => {
                            eprintln!("error: IR generation failed: {e}");
                            Some(2)
                        }
                    }
                }
                Err(e) => {
                    eprintln!("error: wasm32 emission failed: {e}");
                    Some(2)
                }
            }
        }
        TargetKind::RiscV32 => {
            let mut opt = std::env::var("MEDI_LLVM_OPT")
                .ok()
                .and_then(|s| s.parse::<u8>().ok());
            if opt.is_none() {
                #[cfg(debug_assertions)]
                {
                    opt = Some(1);
                }
                #[cfg(not(debug_assertions))]
                {
                    opt = Some(3);
                }
            }
            if std::env::var("MEDI_LLVM_PIPE").is_err() {
                #[cfg(debug_assertions)]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "debug");
                }
                #[cfg(not(debug_assertions))]
                {
                    std::env::set_var("MEDI_LLVM_PIPE", "default");
                }
            }
            let cpu = std::env::var("MEDI_LLVM_CPU").unwrap_or_else(|_| "generic".to_string());
            let feats = std::env::var("MEDI_LLVM_FEATURES").unwrap_or_else(|_| "".to_string());
            let result = generate_riscv32_object_with_opts_types_and_specs(
                program,
                opt.unwrap_or(2),
                &cpu,
                &feats,
                &fun_tys,
                &specs,
            );
            match result {
                Ok(obj) => {
                    if let Some(path) = out_path {
                        if let Some(parent) = std::path::Path::new(&path).parent() {
                            if !parent.as_os_str().is_empty() {
                                if let Err(e) = fs::create_dir_all(parent) {
                                    eprintln!(
                                        "error: failed to create output directory '{}': {e}",
                                        parent.display()
                                    );
                                    return Some(2);
                                }
                            }
                        }
                        if let Err(e) = fs::write(&path, &obj) {
                            eprintln!("error: failed to write object file '{path}': {e}");
                            return Some(2);
                        }
                        let opt_used =
                            std::env::var("MEDI_LLVM_OPT").unwrap_or_else(|_| "2".into());
                        let cpu_used =
                            std::env::var("MEDI_LLVM_CPU").unwrap_or_else(|_| "generic".into());
                        let feats_used =
                            std::env::var("MEDI_LLVM_FEATURES").unwrap_or_else(|_| "".into());
                        eprintln!(
                             "wrote riscv32 object to {path} ({} bytes) [opt={}, cpu='{}', features='{}']",
                             obj.len(), opt_used, cpu_used, feats_used
                         );
                        Some(0)
                    } else {
                        eprintln!("note: no --out path provided; printing LLVM IR instead");
                        match generate_ir_string_with_types_and_specs(program, &fun_tys, &specs) {
                            Ok(ir) => {
                                println!("{ir}");
                                Some(0)
                            }
                            Err(e) => {
                                eprintln!("error: IR generation failed: {e}");
                                Some(2)
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("error: riscv32 code emission failed: {e}");
                    Some(2)
                }
            }
        }
    }
}

fn run_repl() -> i32 {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;
    let mut rl = match Editor::<(), rustyline::history::DefaultHistory>::new() {
        Ok(e) => e,
        Err(e) => {
            eprintln!("error: failed to initialize repl: {e}");
            return 2;
        }
    };

    let mut session = ReplSession::new();
    loop {
        let prompt = session.prompt();
        match rl.readline(prompt) {
            Ok(line) => {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    let _ = rl.add_history_entry(trimmed);
                }
                let (out, exit, committed) = session.handle_line(&line);
                for l in out {
                    println!("{l}");
                }
                if exit {
                    return 0;
                }
                if committed {
                    medic_runtime::maybe_incremental_step();
                }
            }
            Err(ReadlineError::Interrupted) => {
                session.buffer.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                return 0;
            }
            Err(e) => {
                eprintln!("error: repl failed: {e}");
                return 2;
            }
        }
    }
}

fn run_docs(args: &DocsArgs) -> i32 {
    let base_out_dir = args
        .out_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from("docs_out"));

    let out_dir = if let Some(v) = &args.version {
        base_out_dir.join(v)
    } else {
        base_out_dir.clone()
    };
    if args.inputs.is_empty() {
        eprintln!("error: no input files provided");
        return 2;
    }

    if args.clean && out_dir.exists() {
        if let Err(e) = fs::remove_dir_all(&out_dir) {
            eprintln!(
                "error: failed to clean output directory '{}': {e}",
                out_dir.display()
            );
            return 2;
        }
    }

    if args.clean {
        let latest_dir = base_out_dir.join("latest");
        if latest_dir.exists() {
            let _ = fs::remove_dir_all(&latest_dir);
        }
    }

    if let Err(e) = fs::create_dir_all(&out_dir) {
        eprintln!(
            "error: failed to create output directory '{}': {e}",
            out_dir.display()
        );
        return 2;
    }

    let rc = if args.output_format == "html" {
        run_docs_html(args, &base_out_dir, &out_dir)
    } else if args.output_format == "json" {
        run_docs_json(args, &out_dir)
    } else {
        run_docs_md(args, &out_dir)
    };

    if rc == 0 {
        if let Some(v) = &args.version {
            if let Err(e) = update_latest_redirect(&base_out_dir, v, &args.output_format) {
                eprintln!("warning: failed to update latest redirect: {e}");
            }
        }
    }

    if rc == 0 && args.open {
        let index_path = if args.output_format == "html" {
            out_dir.join("index.html")
        } else if args.output_format == "json" {
            out_dir.join("docs.json")
        } else {
            out_dir.join("index.md")
        };
        match ProcessCommand::new("xdg-open").arg(&index_path).status() {
            Ok(status) => {
                if !status.success() {
                    eprintln!("warning: failed to open docs (xdg-open exit status: {status})");
                }
            }
            Err(e) => {
                eprintln!("warning: failed to open docs: {e}");
            }
        }
    }

    rc
}

fn run_docs_md(args: &DocsArgs, out_dir: &Path) -> i32 {
    let mut index = String::new();
    index.push_str("# Medi Docs\n\n");

    for input in &args.inputs {
        let src = match fs::read_to_string(input) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("warning: failed to read '{}': {e}", input.display());
                continue;
            }
        };

        let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("doc");
        let out_file = out_dir.join(format!("{stem}.md"));

        let mut out = String::new();
        out.push_str(&format!("# {}\n\n", input.display()));

        let (_comments, items) = match docgen::extract_doc_items_from_source(&src, Some(input)) {
            Ok(v) => v,
            Err(e) => {
                eprintln!(
                    "warning: failed to extract docs from '{}': {e}",
                    input.display()
                );
                continue;
            }
        };

        let mut items = items;
        let warnings = docgen::resolve_intra_doc_links(&mut items);
        for w in warnings.warnings {
            eprintln!("warning: {w}");
        }

        let lints = docgen::lint_doc_items(&items);
        for lint in lints {
            eprintln!("warning: {}", lint.diagnostic.message);
        }

        if items.is_empty() {
            out.push_str("(no documentable items found)\n");
        } else {
            for item in items {
                out.push_str(&format!("## {}\n\n", item.name));
                if let Some(doc) = item.doc {
                    out.push_str(&doc.text);
                    out.push('\n');
                } else {
                    out.push_str("(no doc comments)\n");
                }
                out.push('\n');
            }
        }

        if let Err(e) = fs::write(&out_file, out) {
            eprintln!(
                "warning: failed to write docs file '{}': {e}",
                out_file.display()
            );
            continue;
        }

        index.push_str(&format!(
            "- [{}]({})\n",
            input.display(),
            out_file.file_name().unwrap().to_string_lossy()
        ));
    }

    let index_path = out_dir.join("index.md");
    if let Err(e) = fs::write(&index_path, index) {
        eprintln!("error: failed to write '{}': {e}", index_path.display());
        return 2;
    }

    0
}

fn run_docs_json(args: &DocsArgs, out_dir: &Path) -> i32 {
    if args.inputs.is_empty() {
        eprintln!("error: no input files provided");
        return 2;
    }

    let mut files: Vec<serde_json::Value> = Vec::new();

    for input in &args.inputs {
        let src = match fs::read_to_string(input) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("warning: failed to read '{}': {e}", input.display());
                continue;
            }
        };

        let (_comments, items0) = match docgen::extract_doc_items_from_source(&src, Some(input)) {
            Ok(v) => v,
            Err(e) => {
                eprintln!(
                    "warning: failed to extract docs from '{}': {e}",
                    input.display()
                );
                continue;
            }
        };

        let mut items = items0;
        let link_warnings = docgen::resolve_intra_doc_links(&mut items);
        for w in link_warnings.warnings {
            eprintln!("warning: {w}");
        }

        let lints = docgen::lint_doc_items(&items);
        for lint in &lints {
            eprintln!("warning: {}", lint.diagnostic.message);
        }

        let items_json: Vec<serde_json::Value> = items
            .iter()
            .map(|it| {
                serde_json::json!({
                    "name": it.name,
                    "kind": format!("{:?}", it.kind),
                    "span": {
                        "start": it.span.start,
                        "end": it.span.end,
                        "line": it.span.line,
                        "column": it.span.column,
                    },
                    "doc": it.doc.as_ref().map(|d| d.text.clone()),
                })
            })
            .collect();

        let lints_json: Vec<serde_json::Value> = lints
            .iter()
            .map(|l| {
                serde_json::json!({
                    "kind": format!("{:?}", l.kind),
                    "message": l.diagnostic.message,
                    "span": {
                        "start": l.diagnostic.span.start,
                        "end": l.diagnostic.span.end,
                        "line": l.diagnostic.span.line,
                        "column": l.diagnostic.span.column,
                    },
                })
            })
            .collect();

        files.push(serde_json::json!({
            "path": input.display().to_string(),
            "items": items_json,
            "lints": lints_json,
        }));
    }

    let docset = serde_json::json!({
        "schema_version": 1,
        "tool": "medic docs",
        "files": files,
    });

    let out_path = out_dir.join("docs.json");
    let text = match serde_json::to_string_pretty(&docset) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: failed to serialize docs.json: {e}");
            return 2;
        }
    };
    if let Err(e) = fs::write(&out_path, text) {
        eprintln!("error: failed to write '{}': {e}", out_path.display());
        return 2;
    }

    0
}

fn run_docs_html(args: &DocsArgs, base_out_dir: &Path, out_dir: &Path) -> i32 {
    let versions = list_doc_versions(base_out_dir);
    let current_version = args.version.as_deref();

    let style = r#"body{margin:0;font-family:ui-sans-serif,system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial}header{position:sticky;top:0;background:#111827;color:#fff;padding:12px 16px;display:flex;gap:12px;align-items:center}header a{color:#fff;text-decoration:none;font-weight:600}main{display:grid;grid-template-columns:280px 1fr;min-height:calc(100vh - 48px)}nav{border-right:1px solid #e5e7eb;padding:12px 10px;overflow:auto}nav input{width:100%;padding:8px;border:1px solid #d1d5db;border-radius:8px}nav ul{list-style:none;margin:10px 0 0 0;padding:0}nav li{margin:6px 0}nav a{text-decoration:none;color:#111827}nav a:hover{text-decoration:underline}article{padding:18px;max-width:900px}pre.doc{white-space:pre-wrap;background:#f9fafb;border:1px solid #e5e7eb;padding:12px;border-radius:10px}code{font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace}h2{margin-top:28px}small.muted{color:#6b7280}"#;

    let search_js = r#"async function loadIndex(){const r=await fetch('search-index.json');return await r.json();}
function clearList(ul){while(ul.firstChild)ul.removeChild(ul.firstChild);} 
function renderResults(ul, items){clearList(ul); for(const it of items){const li=document.createElement('li'); const a=document.createElement('a'); a.href=it.file + '#' + it.anchor; a.textContent=it.name + ' (' + it.kind + ')'; li.appendChild(a); ul.appendChild(li);} }
document.addEventListener('DOMContentLoaded', async ()=>{const input=document.getElementById('search'); const ul=document.getElementById('results'); if(!input||!ul) return; const idx=await loadIndex(); renderResults(ul, idx.items);
input.addEventListener('input', ()=>{const q=input.value.trim().toLowerCase(); if(!q){renderResults(ul, idx.items); return;} const filtered=idx.items.filter(x=> (x.name||'').toLowerCase().includes(q) || (x.excerpt||'').toLowerCase().includes(q)); renderResults(ul, filtered.slice(0,200));});
document.addEventListener('keydown', (e)=>{ if(e.key==='s' && (e.target===document.body||e.target===document.documentElement)){ e.preventDefault(); input.focus(); }});
});"#;

    if let Err(e) = fs::write(out_dir.join("style.css"), style) {
        eprintln!("error: failed to write style.css: {e}");
        return 2;
    }
    if let Err(e) = fs::write(out_dir.join("search.js"), search_js) {
        eprintln!("error: failed to write search.js: {e}");
        return 2;
    }

    let src_dir = out_dir.join("src");
    if let Err(e) = fs::create_dir_all(&src_dir) {
        eprintln!("error: failed to create '{}': {e}", src_dir.display());
        return 2;
    }

    let mut nav_links: Vec<(String, String)> = Vec::new();
    let mut search_items: Vec<serde_json::Value> = Vec::new();

    for input in &args.inputs {
        let src = match fs::read_to_string(input) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("warning: failed to read '{}': {e}", input.display());
                continue;
            }
        };

        let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("doc");
        let out_file = format!("{stem}.html");
        let src_file = format!("{stem}.html");
        let src_href = format!("src/{src_file}");
        nav_links.push((input.display().to_string(), out_file.clone()));

        let source_page = render_source_page(&src, &format!("{}", input.display()));
        if let Err(e) = fs::write(src_dir.join(&src_file), source_page) {
            eprintln!(
                "warning: failed to write source file '{}': {e}",
                src_dir.join(&src_file).display()
            );
        }

        let (_comments, items0) = match docgen::extract_doc_items_from_source(&src, Some(input)) {
            Ok(v) => v,
            Err(e) => {
                eprintln!(
                    "warning: failed to extract docs from '{}': {e}",
                    input.display()
                );
                continue;
            }
        };

        let mut items = items0;
        let warnings = docgen::resolve_intra_doc_links(&mut items);
        for w in warnings.warnings {
            eprintln!("warning: {w}");
        }

        let lints = docgen::lint_doc_items(&items);
        for lint in lints {
            eprintln!("warning: {}", lint.diagnostic.message);
        }

        for it in &items {
            let anchor = anchor_for_name(&it.name);
            let excerpt = it
                .doc
                .as_ref()
                .map(|d| excerpt_first_line(&d.text))
                .unwrap_or_default();
            search_items.push(serde_json::json!({
                "name": it.name,
                "kind": format!("{:?}", it.kind),
                "file": out_file,
                "anchor": anchor,
                "excerpt": excerpt,
            }));
        }

        let page = render_html_page(
            &format!("{}", input.display()),
            &nav_links,
            &items,
            &src_href,
            current_version,
            &versions,
        );
        if let Err(e) = fs::write(out_dir.join(&out_file), page) {
            eprintln!(
                "warning: failed to write docs file '{}': {e}",
                out_dir.join(&out_file).display()
            );
            continue;
        }
    }

    let index_html = render_html_index(&nav_links, current_version, &versions);
    if let Err(e) = fs::write(out_dir.join("index.html"), index_html) {
        eprintln!("error: failed to write index.html: {e}");
        return 2;
    }

    let search_index = serde_json::json!({"items": search_items});
    let search_index_path = out_dir.join("search-index.json");
    if let Err(e) = fs::write(
        &search_index_path,
        serde_json::to_string_pretty(&search_index).unwrap(),
    ) {
        eprintln!(
            "error: failed to write '{}': {e}",
            search_index_path.display()
        );
        return 2;
    }

    0
}

fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

fn anchor_for_name(name: &str) -> String {
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

fn excerpt_first_line(text: &str) -> String {
    text.lines().next().unwrap_or("").trim().to_string()
}

fn render_html_index(
    nav_links: &[(String, String)],
    current_version: Option<&str>,
    versions: &[String],
) -> String {
    let mut list = String::new();
    for (label, href) in nav_links {
        list.push_str(&format!(
            "<li><a href=\"{}\">{}</a></li>",
            html_escape(href),
            html_escape(label)
        ));
    }

    let version_ui = render_version_selector(current_version, versions);
    format!(
        "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>Medi Docs</title><link rel=\"stylesheet\" href=\"style.css\"></head><body><header><a href=\"index.html\">Medi Docs</a><small class=\"muted\">press 's' to search</small>{}</header><main><nav><input id=\"search\" placeholder=\"Search...\" autocomplete=\"off\"><ul id=\"results\">{}</ul></nav><article><h1>Medi Docs</h1><p>Open a file from the sidebar or use search.</p><h2>Files</h2><ul>{}</ul></article></main><script src=\"search.js\"></script>{}</body></html>",
        version_ui.0,
        list,
        list,
        version_ui.1
    )
}

fn render_html_page(
    title: &str,
    nav_links: &[(String, String)],
    items: &[docgen::DocItem],
    src_href: &str,
    current_version: Option<&str>,
    versions: &[String],
) -> String {
    let mut nav = String::new();
    for (label, href) in nav_links {
        nav.push_str(&format!(
            "<li><a href=\"{}\">{}</a></li>",
            html_escape(href),
            html_escape(label)
        ));
    }

    let mut body = String::new();
    body.push_str(&format!("<h1>{}</h1>", html_escape(title)));
    if items.is_empty() {
        body.push_str("<p>(no documentable items found)</p>");
    } else {
        for it in items {
            let anchor = anchor_for_name(&it.name);
            let line = it.span.line.max(1);
            let src_link = format!("{src_href}#L{line}");
            body.push_str(&format!(
                "<h2 id=\"{}\">{} <a class=\"muted\" href=\"{}\">[src]</a></h2>",
                html_escape(&anchor),
                html_escape(&it.name),
                html_escape(&src_link)
            ));
            if let Some(doc) = &it.doc {
                body.push_str("<pre class=\"doc\"><code>");
                body.push_str(&html_escape(&doc.text));
                body.push_str("</code></pre>");
            } else {
                body.push_str("<p>(no doc comments)</p>");
            }
        }
    }

    let version_ui = render_version_selector(current_version, versions);

    format!(
        "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>{}</title><link rel=\"stylesheet\" href=\"style.css\"></head><body><header><a href=\"index.html\">Medi Docs</a><small class=\"muted\">press 's' to search</small>{}</header><main><nav><input id=\"search\" placeholder=\"Search...\" autocomplete=\"off\"><ul id=\"results\">{}</ul></nav><article>{}</article></main><script src=\"search.js\"></script>{}</body></html>",
        html_escape(title),
        version_ui.0,
        nav,
        body,
        version_ui.1
    )
}

fn render_source_page(source: &str, title: &str) -> String {
    let mut lines_html = String::new();
    for (i, line) in source.lines().enumerate() {
        let ln = i + 1;
        lines_html.push_str(&format!(
            "<div id=\"L{ln}\"><a href=\"#L{ln}\" class=\"muted\">{ln:>4}</a> <code>{}</code></div>",
            html_escape(line)
        ));
    }

    format!(
        "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>{}</title><link rel=\"stylesheet\" href=\"../style.css\"></head><body><header><a href=\"../index.html\">Medi Docs</a><small class=\"muted\">source</small></header><main><nav><input id=\"search\" placeholder=\"Search...\" autocomplete=\"off\"><ul id=\"results\"></ul></nav><article><h1>{}</h1><pre class=\"doc\">{}</pre></article></main><script src=\"../search.js\"></script></body></html>",
        html_escape(title),
        html_escape(title),
        lines_html
    )
}

fn list_doc_versions(base_out_dir: &Path) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let Ok(rd) = fs::read_dir(base_out_dir) else {
        return out;
    };
    for ent in rd.flatten() {
        let Ok(ft) = ent.file_type() else {
            continue;
        };
        if !ft.is_dir() {
            continue;
        }
        let name = ent.file_name().to_string_lossy().to_string();
        if name == "latest" {
            continue;
        }
        out.push(name);
    }
    out.sort();
    out
}

fn render_version_selector(current_version: Option<&str>, versions: &[String]) -> (String, String) {
    let Some(cur) = current_version else {
        return (String::new(), String::new());
    };
    if versions.is_empty() {
        return (String::new(), String::new());
    }
    let mut options = String::new();
    for v in versions {
        let selected = if v == cur { " selected" } else { "" };
        options.push_str(&format!(
            "<option value=\"{}\"{}>{}</option>",
            html_escape(v),
            selected,
            html_escape(v)
        ));
    }

    let html = format!(
        "<label style=\"margin-left:auto;display:flex;gap:8px;align-items:center\"><small class=\"muted\">version</small><select id=\"version\" style=\"padding:6px;border-radius:8px;border:1px solid #374151;background:#111827;color:#fff\">{options}</select></label>",
    );

    let js = format!(
        r#"<script>(function(){{
const cur={cur:?};
const sel=document.getElementById('version');
if(!sel) return;
sel.addEventListener('change', function(){{
  const next=this.value;
  const parts=window.location.pathname.split('/');
  const idx=parts.lastIndexOf(cur);
  if(idx>=0){{parts[idx]=next; window.location.pathname=parts.join('/'); return;}}
  window.location.href='../'+next+'/index.html';
}});
}})();</script>"#,
    );

    (html, js)
}

fn update_latest_redirect(base_out_dir: &Path, version: &str, format: &str) -> Result<(), String> {
    let latest_dir = base_out_dir.join("latest");
    if latest_dir.exists() {
        fs::remove_dir_all(&latest_dir)
            .map_err(|e| format!("failed to remove {}: {e}", latest_dir.display()))?;
    }
    fs::create_dir_all(&latest_dir)
        .map_err(|e| format!("failed to create {}: {e}", latest_dir.display()))?;

    if format == "html" {
        let html = format!(
            "<!doctype html><meta charset=\"utf-8\"><meta http-equiv=\"refresh\" content=\"0; url=../{}/index.html\"><title>Redirecting...</title><a href=\"../{}/index.html\">Open latest docs</a>",
            html_escape(version),
            html_escape(version)
        );
        fs::write(latest_dir.join("index.html"), html)
            .map_err(|e| format!("failed to write latest/index.html: {e}"))?;
    } else if format == "md" {
        let md = format!("# Latest Medi Docs\n\nOpen: ../{version}/index.md\n");
        fs::write(latest_dir.join("index.md"), md)
            .map_err(|e| format!("failed to write latest/index.md: {e}"))?;
    } else if format == "json" {
        let json = serde_json::json!({
            "schema_version": 1,
            "latest": version,
            "path": format!("../{}/docs.json", version),
        });
        let text = serde_json::to_string_pretty(&json)
            .map_err(|e| format!("failed to serialize latest docs.json: {e}"))?;
        fs::write(latest_dir.join("docs.json"), text)
            .map_err(|e| format!("failed to write latest/docs.json: {e}"))?;
    }

    Ok(())
}

fn run_pack(args: &PackArgs) -> i32 {
    let manifest = PathBuf::from("medipack.toml");
    match args.command.clone().unwrap_or(PackCommand::List) {
        PackCommand::Init => {
            if manifest.exists() {
                eprintln!("error: '{}' already exists", manifest.display());
                return 2;
            }
            let contents = "name = \"your_package\"\nversion = \"0.1.0\"\n\n[dependencies]\n";
            if let Err(e) = fs::write(&manifest, contents) {
                eprintln!("error: failed to write '{}': {e}", manifest.display());
                return 2;
            }
            0
        }
        PackCommand::List => {
            if !manifest.exists() {
                eprintln!(
                    "error: '{}' not found (run 'medic pack init')",
                    manifest.display()
                );
                return 2;
            }
            let text = match fs::read_to_string(&manifest) {
                Ok(t) => t,
                Err(e) => {
                    eprintln!("error: failed to read '{}': {e}", manifest.display());
                    return 2;
                }
            };
            let mut in_deps = false;
            for line in text.lines() {
                let t = line.trim();
                if t.starts_with('[') {
                    in_deps = t == "[dependencies]";
                    continue;
                }
                if in_deps {
                    if t.is_empty() || t.starts_with('#') {
                        continue;
                    }
                    println!("{t}");
                }
            }
            0
        }
    }
}

fn normalize_cli_args(args: Vec<OsString>) -> Vec<OsString> {
    if args.len() <= 1 {
        return args;
    }

    let first = args[1].to_string_lossy();
    let is_known_subcommand = matches!(
        first.as_ref(),
        "check"
            | "json"
            | "repl"
            | "docs"
            | "test"
            | "pack"
            | "help"
            | "--help"
            | "-h"
            | "--version"
            | "-V"
    );
    if is_known_subcommand {
        return args;
    }

    let mut out: Vec<OsString> = Vec::with_capacity(args.len() + 1);
    out.push(args[0].clone());

    let mut subcmd = OsString::from("check");
    let mut rest: Vec<OsString> = Vec::with_capacity(args.len().saturating_sub(1));
    let iter = args.into_iter().skip(1);
    for a in iter {
        let s = a.to_string_lossy();
        if s == "--json" || s == "-j" {
            subcmd = OsString::from("json");
            continue;
        }
        rest.push(a);
    }

    out.push(subcmd);
    out.extend(rest);
    out
}

fn normalized_cli_args() -> Vec<OsString> {
    normalize_cli_args(std::env::args_os().collect())
}

fn run_cli() -> i32 {
    init_gc_from_env();
    let cli = Cli::parse_from(normalized_cli_args());

    let cmd = cli.command.unwrap_or(Command::Check(CheckArgs {
        input: None,
        types_json: None,
        #[cfg(feature = "llvm-backend")]
        emit: None,
        #[cfg(feature = "llvm-backend")]
        out: None,
        #[cfg(feature = "llvm-backend")]
        opt: None,
        #[cfg(feature = "llvm-backend")]
        cpu: None,
        #[cfg(feature = "llvm-backend")]
        features: None,
        #[cfg(feature = "llvm-backend")]
        opt_pipeline: None,
    }));

    match cmd {
        Command::Check(args) => {
            apply_llvm_env_overrides(&args);
            let source = match read_source_from_input(&args.input) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: {e}");
                    return 2;
                }
            };

            #[cfg(feature = "llvm-backend")]
            {
                if args.emit.is_some() {
                    let tokens: Vec<Token> = Lexer::new(&source).collect();
                    let input = TokenSlice::new(&tokens);
                    match parse_program_with_diagnostics(input) {
                        Ok(program) => {
                            let mut env = TypeEnv::with_prelude();
                            if let Some(ref p) = args.types_json {
                                match fs::read_to_string(p) {
                                    Ok(text) => {
                                        if let Err(e) = load_types_json(&mut env, &text) {
                                            eprintln!(
												"warning: failed to load --types-json file '{}': {e}",
												p.display()
											);
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!(
                                            "warning: cannot read --types-json file '{}': {e}",
                                            p.display()
                                        );
                                    }
                                }
                            }
                            if let Some(rc) = maybe_emit(&args, &source, &tokens, &program, env) {
                                return rc;
                            }
                        }
                        Err(diag) => {
                            eprintln!("{}", render_snippet(&diag, &source));
                            return 1;
                        }
                    }
                }
            }

            let rc = run_check(&source, &args.types_json, OutputMode::Text);
            if cli.verbose > 0 {
                eprintln!("note: check completed with exit code {rc}");
            }
            rc
        }
        Command::Json(args) => {
            let source = match read_source_from_input(&args.input) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: {e}");
                    return 2;
                }
            };
            run_check(&source, &args.types_json, OutputMode::Json)
        }
        Command::Repl => run_repl(),
        Command::Docs(args) => run_docs(&args),
        Command::Test(args) => run_test(&args),
        Command::Pack(args) => run_pack(&args),
    }
}

fn main() {
    std::process::exit(run_cli());
}

#[cfg(test)]
mod tests {
    use super::*;

    static CWD_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn legacy_args_are_mapped_to_check_subcommand() {
        let args = vec![
            OsString::from("medic"),
            OsString::from("--emit=x86_64"),
            OsString::from("--opt=2"),
            OsString::from("--out=artifacts/x86_64.o"),
            OsString::from("input.medi"),
        ];
        let out = normalize_cli_args(args);
        assert_eq!(out[1].to_string_lossy(), "check");
    }

    #[test]
    fn legacy_json_flag_is_mapped_to_json_subcommand() {
        let args = vec![OsString::from("medic"), OsString::from("--json")];
        let out = normalize_cli_args(args);
        assert_eq!(out[1].to_string_lossy(), "json");
    }

    #[test]
    fn docs_and_pack_commands_work_on_temp_dir() {
        let _guard = CWD_LOCK.lock().unwrap();
        let dir = tempfile::tempdir().unwrap();
        let old = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// hello\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs");
        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
            clean: false,
            open: false,
            version: None,
            output_format: "md".to_string(),
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);
        assert!(out_dir.join("index.md").exists());
        assert!(out_dir.join("sample.md").exists());

        let rc = run_pack(&PackArgs {
            command: Some(PackCommand::Init),
        });
        assert_eq!(rc, 0);
        assert!(dir.path().join("medipack.toml").exists());

        let rc = run_pack(&PackArgs {
            command: Some(PackCommand::List),
        });
        assert_eq!(rc, 0);

        std::env::set_current_dir(old).unwrap();
    }

    #[test]
    fn json_docs_generates_docs_json() {
        let _guard = CWD_LOCK.lock().unwrap();
        let dir = tempfile::tempdir().unwrap();
        let old = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// Docs for main\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs_json");
        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
            clean: false,
            open: false,
            version: None,
            output_format: "json".to_string(),
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);

        let path = out_dir.join("docs.json");
        assert!(path.exists());
        let text = fs::read_to_string(&path).unwrap();
        let v: serde_json::Value = serde_json::from_str(&text).unwrap();
        assert_eq!(v["schema_version"], 1);
        assert_eq!(v["tool"], "medic docs");
        assert!(!v["files"].as_array().unwrap().is_empty());

        let files = v["files"].as_array().unwrap();
        let f0 = &files[0];
        let items = f0["items"].as_array().unwrap();
        assert!(items.iter().any(|it| it["name"] == "main"));

        std::env::set_current_dir(old).unwrap();
    }

    #[test]
    fn html_docs_generate_src_pages_and_links() {
        let dir = tempfile::tempdir().unwrap();
        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// hello\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs_html");
        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
            clean: false,
            open: false,
            version: None,
            output_format: "html".to_string(),
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);
        assert!(out_dir.join("index.html").exists());
        assert!(out_dir.join("search-index.json").exists());
        assert!(out_dir.join("style.css").exists());
        assert!(out_dir.join("search.js").exists());

        // per-file doc page and source page
        assert!(out_dir.join("sample.html").exists());
        assert!(out_dir.join("src").join("sample.html").exists());

        let html = fs::read_to_string(out_dir.join("sample.html")).unwrap();
        assert!(html.contains("src/sample.html#L"));
        assert!(html.contains("[src]"));
    }

    #[test]
    fn versioned_html_docs_create_latest_redirect() {
        let _guard = CWD_LOCK.lock().unwrap();
        let dir = tempfile::tempdir().unwrap();
        let old = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// hello\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs_versions");

        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
            clean: false,
            open: false,
            version: Some("v1.0".to_string()),
            output_format: "html".to_string(),
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);
        assert!(out_dir.join("v1.0").join("index.html").exists());
        assert!(out_dir.join("latest").join("index.html").exists());

        std::env::set_current_dir(old).unwrap();
    }

    #[test]
    fn docs_clean_removes_existing_files() {
        let _guard = CWD_LOCK.lock().unwrap();
        let dir = tempfile::tempdir().unwrap();
        let old = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// hello\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs_clean");
        fs::create_dir_all(&out_dir).unwrap();
        fs::write(out_dir.join("sentinel.txt"), "old").unwrap();
        assert!(out_dir.join("sentinel.txt").exists());

        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
            clean: true,
            open: false,
            version: None,
            output_format: "md".to_string(),
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);
        assert!(!out_dir.join("sentinel.txt").exists());
        assert!(out_dir.join("index.md").exists());

        std::env::set_current_dir(old).unwrap();
    }

    #[test]
    fn cli_help_contains_expected_content() {
        use clap::CommandFactory;
        let mut cmd = Cli::command();
        let mut buf = Vec::new();
        cmd.write_long_help(&mut buf).unwrap();
        let help = String::from_utf8(buf).unwrap();

        // Verify key help content is present
        assert!(help.contains("medic"), "help should mention 'medic'");
        assert!(
            help.contains("healthcare"),
            "help should mention healthcare focus"
        );
        assert!(
            help.contains("HIPAA"),
            "help should mention HIPAA compliance"
        );
        assert!(
            help.contains("EXAMPLES"),
            "help should include examples section"
        );
        assert!(help.contains("check"), "help should list check subcommand");
        assert!(help.contains("repl"), "help should list repl subcommand");
        assert!(help.contains("--version"), "help should show version flag");
        assert!(help.contains("--help"), "help should show help flag");
    }

    #[test]
    fn cli_version_is_set() {
        use clap::CommandFactory;
        let cmd = Cli::command();
        let version = cmd.get_version().expect("version should be set");
        assert!(!version.is_empty(), "version should not be empty");
    }

    #[test]
    fn check_subcommand_help_contains_expected_content() {
        use clap::CommandFactory;
        let cmd = Cli::command();
        let check_cmd = cmd
            .get_subcommands()
            .find(|c| c.get_name() == "check")
            .expect("check subcommand should exist");

        let about = check_cmd
            .get_about()
            .map(|s| s.to_string())
            .unwrap_or_default();
        assert!(
            about.contains("Check") || about.contains("compile"),
            "check about should describe checking/compiling"
        );
    }

    #[test]
    fn repl_subcommand_help_mentions_commands() {
        use clap::CommandFactory;
        let cmd = Cli::command();
        let repl_cmd = cmd
            .get_subcommands()
            .find(|c| c.get_name() == "repl")
            .expect("repl subcommand should exist");

        let long_about = repl_cmd
            .get_long_about()
            .map(|s| s.to_string())
            .unwrap_or_default();
        assert!(
            long_about.contains(":help") || long_about.contains(":quit"),
            "repl long_about should mention REPL commands"
        );
    }

    #[test]
    fn known_subcommands_are_not_normalized() {
        for subcmd in [
            "check",
            "json",
            "repl",
            "docs",
            "test",
            "pack",
            "help",
            "--help",
            "-h",
            "--version",
            "-V",
        ] {
            let args = vec![OsString::from("medic"), OsString::from(subcmd)];
            let out = normalize_cli_args(args.clone());
            assert_eq!(
                out, args,
                "known subcommand '{subcmd}' should not be modified"
            );
        }
    }

    #[test]
    fn doctest_command_runs_on_temp_dir() {
        let dir = tempfile::tempdir().unwrap();
        let input_path = dir.path().join("sample.medi");
        fs::write(
            &input_path,
            "/// # Examples\n/// ```medi\n/// let x = 1;\n/// ```\nfn a() {}\n",
        )
        .unwrap();

        let rc = run_test(&TestArgs {
            doc: true,
            inputs: vec![input_path.clone()],
        });
        assert_eq!(rc, 0);
    }

    #[test]
    fn empty_args_are_not_modified() {
        let args = vec![OsString::from("medic")];
        let out = normalize_cli_args(args.clone());
        assert_eq!(out, args, "single arg should not be modified");
    }

    #[test]
    fn cli_parses_verbose_flag() {
        let cli = Cli::try_parse_from(["medic", "-vvv"]).unwrap();
        assert_eq!(cli.verbose, 3, "verbose count should be 3 for -vvv");
    }

    #[test]
    fn cli_parses_check_with_file() {
        let cli = Cli::try_parse_from(["medic", "check", "test.medi"]).unwrap();
        match cli.command {
            Some(Command::Check(args)) => {
                assert_eq!(args.input, Some(PathBuf::from("test.medi")));
            }
            _ => panic!("expected Check command"),
        }
    }

    #[test]
    fn cli_parses_types_json_flag() {
        let cli =
            Cli::try_parse_from(["medic", "check", "--types-json", "types.json", "test.medi"])
                .unwrap();
        match cli.command {
            Some(Command::Check(args)) => {
                assert_eq!(args.types_json, Some(PathBuf::from("types.json")));
            }
            _ => panic!("expected Check command"),
        }
    }
}
