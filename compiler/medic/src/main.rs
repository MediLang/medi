use std::ffi::OsString;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

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
    #[arg(long = "out-dir", value_name = "DIR")]
    out_dir: Option<PathBuf>,

    /// Medi source files to generate documentation from
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
                let mut bchk = BorrowChecker::new();
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
    let mut buffer = String::new();
    loop {
        let prompt = if buffer.is_empty() {
            "medic> "
        } else {
            "....> "
        };
        match rl.readline(prompt) {
            Ok(line) => {
                let trimmed = line.trim();
                if buffer.is_empty() {
                    if trimmed == ":q" || trimmed == ":quit" || trimmed == ":exit" {
                        return 0;
                    }
                    if trimmed == ":help" {
                        println!("commands: :help, :quit");
                        continue;
                    }
                }
                if !trimmed.is_empty() {
                    let _ = rl.add_history_entry(trimmed);
                }
                buffer.push_str(&line);
                buffer.push('\n');
                if trimmed.ends_with(';') || trimmed.ends_with('}') {
                    let code = buffer.clone();
                    buffer.clear();
                    let rc = run_check(&code, &None, OutputMode::Text);
                    if rc == 0 {
                        println!("ok");
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                buffer.clear();
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
    let out_dir = args
        .out_dir
        .clone()
        .unwrap_or_else(|| PathBuf::from("docs_out"));
    if args.inputs.is_empty() {
        eprintln!("error: no input files provided");
        return 2;
    }

    if let Err(e) = fs::create_dir_all(&out_dir) {
        eprintln!(
            "error: failed to create output directory '{}': {e}",
            out_dir.display()
        );
        return 2;
    }

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

        let mut any = false;
        for line in src.lines() {
            let t = line.trim_start();
            if let Some(rest) = t.strip_prefix("///") {
                any = true;
                out.push_str(rest.trim_start());
                out.push('\n');
            } else if let Some(rest) = t.strip_prefix("//!") {
                any = true;
                out.push_str(rest.trim_start());
                out.push('\n');
            }
        }

        if !any {
            out.push_str("(no doc comments found)\n");
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
        "check" | "json" | "repl" | "docs" | "pack" | "help" | "--help" | "-h" | "--version" | "-V"
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
        Command::Pack(args) => run_pack(&args),
    }
}

fn main() {
    std::process::exit(run_cli());
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let dir = tempfile::tempdir().unwrap();
        let old = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let input_path = dir.path().join("sample.medi");
        fs::write(&input_path, "/// hello\nfn main() {}\n").unwrap();
        let out_dir = dir.path().join("out_docs");
        let rc = run_docs(&DocsArgs {
            out_dir: Some(out_dir.clone()),
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
