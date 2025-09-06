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
use medic_codegen_llvm::TargetKind;

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
                        // For now, we invoke the skeleton backend functions as a smoke test.
                        match medic_codegen_llvm::generate_llvm_ir("/* TODO: pass real AST */")
                            .and_then(|_| medic_codegen_llvm::optimize_module(0))
                            .and_then(|_| medic_codegen_llvm::generate_target_code(target))
                        {
                            Ok(bytes) => {
                                if let Some(path) = out_path {
                                    if let Err(e) = fs::write(&path, &bytes) {
                                        eprintln!(
                                            "error: failed to write output file '{path}': {e}"
                                        );
                                        std::process::exit(2);
                                    } else {
                                        eprintln!("emitted {} bytes to {path}", bytes.len());
                                    }
                                } else {
                                    // If no path provided, write to stdout as binary may be messy; informively print size
                                    eprintln!("emitted {} bytes (no --out provided)", bytes.len());
                                }
                            }
                            Err(e) => {
                                eprintln!("error: codegen failed: {e}");
                                std::process::exit(2);
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
