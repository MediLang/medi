use std::env;
use std::fs;
use std::io::{self, Read};

use medic_env::env::TypeEnv;
use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{
    parse_program_recovering, parse_program_with_diagnostics, render_snippet, TokenSlice,
};
use medic_typeck::type_checker::TypeChecker;

#[cfg(feature = "llvm-backend")]
use medic_codegen_llvm::{
    generate_x86_64_object_default, generate_x86_64_object_with_opts, TargetKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputMode {
    Text,
    Json,
}

fn main() {
    // Read source either from file arg or stdin; recognize --json flag
    let args: Vec<String> = env::args().collect();
    let mut mode = OutputMode::Text;
    let mut path_arg: Option<String> = None;
    #[cfg(feature = "llvm-backend")]
    let mut emit_target: Option<TargetKind> = None;
    #[cfg(feature = "llvm-backend")]
    let mut out_path: Option<String> = None;
    for a in args.iter().skip(1) {
        match a.as_str() {
            "--json" | "-j" => mode = OutputMode::Json,
            #[cfg(feature = "llvm-backend")]
            s if s == "--emit" || s == "-E" => {
                // Next arg should be a target
                // We'll parse it in a second pass if needed; for now just skip here.
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--emit=") => {
                let val = s.splitn(2, '=').nth(1).unwrap_or("");
                emit_target = match val {
                    "x86_64" => Some(TargetKind::X86_64),
                    "wasm32" => Some(TargetKind::Wasm32),
                    "riscv32" => Some(TargetKind::RiscV32),
                    other => {
                        eprintln!("warning: unknown emit target '{other}', expected x86_64|wasm32|riscv32");
                        None
                    }
                };
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--out=") => {
                out_path = s.splitn(2, '=').nth(1).map(|s| s.to_string());
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--opt=") => {
                // Parse integer 0..=3
                if let Some(val) = s.splitn(2, '=').nth(1) {
                    if let Ok(n) = val.parse::<u8>() {
                        // stash in env vars for simplicity; we will read below
                        std::env::set_var("MEDI_LLVM_OPT", n.to_string());
                    }
                }
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--cpu=") => {
                if let Some(val) = s.splitn(2, '=').nth(1) {
                    std::env::set_var("MEDI_LLVM_CPU", val);
                }
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--features=") => {
                if let Some(val) = s.splitn(2, '=').nth(1) {
                    std::env::set_var("MEDI_LLVM_FEATURES", val);
                }
            }
            #[cfg(feature = "llvm-backend")]
            s if s.starts_with("--opt-pipeline=") => {
                if let Some(val) = s.splitn(2, '=').nth(1) {
                    // Allowed values: minimal (default), default, aggressive, debug
                    let v = match val {
                        "default" | "minimal" | "aggressive" | "debug" => val,
                        other => {
                            eprintln!("warning: unknown opt pipeline '{other}', expected minimal|default|aggressive|debug; using minimal");
                            "minimal"
                        }
                    };
                    std::env::set_var("MEDI_LLVM_PIPE", v);
                }
            }
            s if s.starts_with('-') => {
                eprintln!("warning: unknown flag '{s}'");
            }
            other => {
                path_arg = Some(other.to_string());
            }
        }
    }

    let source = if let Some(path) = path_arg {
        match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error: failed to read '{path}': {e}");
                std::process::exit(2);
            }
        }
    } else {
        let mut buf = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut buf) {
            eprintln!("error: failed to read from stdin: {e}");
            std::process::exit(2);
        }
        buf
    };

    // Tokenize
    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let input = TokenSlice::new(&tokens);

    // First, try strict parse that returns a single diagnostic on error
    match parse_program_with_diagnostics(input) {
        Ok(program) => {
            match mode {
                OutputMode::Text => {
                    // Also run the recovering path to surface non-fatal diagnostics (e.g., lexer errors)
                    let mut diags = Vec::new();
                    let _ = parse_program_recovering(TokenSlice::new(&tokens), &mut diags);
                    for d in diags {
                        eprintln!("{}", render_snippet(&d, &source));
                    }

                    // Run type checking and inference over the parsed program
                    let mut env = TypeEnv::with_prelude();
                    let mut checker = TypeChecker::new(&mut env);
                    let mut type_errors = checker.check_program(&program);
                    // Include any collected validation/type errors from expression checks
                    type_errors.extend(checker.take_errors());

                    if !type_errors.is_empty() {
                        for err in &type_errors {
                            eprintln!("type error: {err}");
                        }
                        std::process::exit(1);
                    }

                    // Optional: code emission when llvm-backend is enabled
                    #[cfg(feature = "llvm-backend")]
                    if let Some(target) = emit_target {
                        match target {
                            TargetKind::X86_64 => {
                                // Check optional overrides from env
                                let mut opt = std::env::var("MEDI_LLVM_OPT")
                                    .ok()
                                    .and_then(|s| s.parse::<u8>().ok());
                                let mut cpu = std::env::var("MEDI_LLVM_CPU").ok();
                                let mut feats = std::env::var("MEDI_LLVM_FEATURES").ok();
                                // If not provided, choose sensible defaults based on build profile
                                if opt.is_none() {
                                    // Debug builds default to lower opts; release builds to higher
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
                                    // Default pipeline: debug build => debug; release => default
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
                                    let feats = feats.unwrap_or_else(|| "".to_string());
                                    generate_x86_64_object_with_opts(&program, opt, &cpu, &feats)
                                };
                                match result {
                                    Ok(obj) => {
                                        if let Some(path) = out_path {
                                            if let Err(e) = fs::write(&path, &obj) {
                                                eprintln!("error: failed to write object file '{path}': {e}");
                                                std::process::exit(2);
                                            } else {
                                                // Report config used
                                                let opt_used = std::env::var("MEDI_LLVM_OPT")
                                                    .unwrap_or_else(|_| "2".into());
                                                let cpu_used = std::env::var("MEDI_LLVM_CPU")
                                                    .unwrap_or_else(|_| "x86-64".into());
                                                let feats_used =
                                                    std::env::var("MEDI_LLVM_FEATURES")
                                                        .unwrap_or_else(|_| "".into());
                                                eprintln!(
                                                    "wrote x86_64 object to {path} ({} bytes) [opt={}, cpu='{}', features='{}']",
                                                    obj.len(), opt_used, cpu_used, feats_used
                                                );
                                            }
                                        } else {
                                            eprintln!("note: no --out=path provided; printing LLVM IR instead");
                                            match medic_codegen_llvm::generate_ir_string(&program) {
                                                Ok(ir) => println!("{ir}"),
                                                Err(e) => {
                                                    eprintln!("error: IR generation failed: {e}");
                                                    std::process::exit(2);
                                                }
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!("error: x86_64 code emission failed: {e}");
                                        std::process::exit(2);
                                    }
                                }
                            }
                            _ => {
                                // Fallback: just emit IR for other targets until implemented
                                match medic_codegen_llvm::generate_ir_string(&program) {
                                    Ok(ir) => {
                                        if let Some(path) = out_path {
                                            if let Err(e) = fs::write(&path, ir.as_bytes()) {
                                                eprintln!(
                                                    "error: failed to write IR file '{path}': {e}"
                                                );
                                                std::process::exit(2);
                                            } else {
                                                eprintln!(
                                                    "wrote LLVM IR to {path} ({} bytes)",
                                                    ir.len()
                                                );
                                            }
                                        } else {
                                            println!("{ir}");
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!("error: IR generation failed: {e}");
                                        std::process::exit(2);
                                    }
                                }
                            }
                        }
                    }
                }
                OutputMode::Json => {
                    // Use library API to produce structured JSON report
                    let report = medic::analyze_source(&source);
                    match serde_json::to_string_pretty(&report) {
                        Ok(json) => println!("{json}"),
                        Err(e) => {
                            eprintln!("error: failed to serialize JSON: {e}");
                            std::process::exit(2);
                        }
                    }
                    if !report.errors.is_empty() {
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(diag) => {
            eprintln!("{}", render_snippet(&diag, &source));
            std::process::exit(1);
        }
    }
}
