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
use serde_json::Value as JsonValue;

#[cfg(feature = "llvm-backend")]
use medic_codegen_llvm::{
    generate_ir_string_with_types_and_specs,
    generate_wasm32_unknown_object_with_opts_types_and_specs,
    generate_wasm32_wasi_object_with_opts_types_and_specs, generate_x86_64_object_default,
    generate_x86_64_object_with_opts, generate_x86_64_object_with_opts_types_and_specs, TargetKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputMode {
    Text,
    Json,
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

fn main() {
    // Read source either from file arg or stdin; recognize --json flag
    let args: Vec<String> = env::args().collect();
    let mut mode = OutputMode::Text;
    let mut path_arg: Option<String> = None;
    let mut types_json_path: Option<String> = None;
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
                    // Accept alias for browser target; we’ll route separately below
                    "wasm32-unknown" => Some(TargetKind::Wasm32),
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
            s if s.starts_with("--types-json=") => {
                types_json_path = s.split_once('=').map(|x| x.1.to_string());
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
                    // Optional: load function types from a JSON file to seed generics like fn(T)->T
                    if let Some(ref p) = types_json_path {
                        match fs::read_to_string(p) {
                            Ok(text) => {
                                if let Err(e) = load_types_json(&mut env, &text) {
                                    eprintln!(
                                        "warning: failed to load --types-json file '{p}': {e}"
                                    );
                                }
                            }
                            Err(e) => {
                                eprintln!("warning: cannot read --types-json file '{p}': {e}");
                            }
                        }
                    }
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
                        // Compute concrete specializations now (checker holds mutable borrow of env)
                        let specs = checker.collect_function_specializations(&program);
                        // Release checker (and its &mut env) before immutably borrowing env for function types
                        drop(checker);
                        let fun_tys = env.collect_function_types();
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
                                    // Use the new API that registers MediType function signatures and specializations first.
                                    generate_x86_64_object_with_opts_types_and_specs(
                                        &program, opt, &cpu, &feats, &fun_tys, &specs,
                                    )
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
                                            match generate_ir_string_with_types_and_specs(
                                                &program, &fun_tys, &specs,
                                            ) {
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
                            TargetKind::Wasm32 => {
                                // If the user explicitly requested wasm32-unknown, route to unknown target emission
                                let is_unknown = args.iter().any(|a| a == "--emit=wasm32-unknown");
                                // Set defaults similar to x86 path
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
                                let cpu = std::env::var("MEDI_LLVM_CPU")
                                    .unwrap_or_else(|_| "generic".to_string());
                                let feats = std::env::var("MEDI_LLVM_FEATURES")
                                    .unwrap_or_else(|_| "".to_string());

                                let result = if is_unknown {
                                    generate_wasm32_unknown_object_with_opts_types_and_specs(
                                        &program,
                                        opt.unwrap_or(2),
                                        &cpu,
                                        &feats,
                                        &fun_tys,
                                        &specs,
                                    )
                                } else {
                                    generate_wasm32_wasi_object_with_opts_types_and_specs(
                                        &program,
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
                                            if let Err(e) = fs::write(&path, &wasm) {
                                                eprintln!("error: failed to write wasm file '{path}': {e}");
                                                std::process::exit(2);
                                            } else {
                                                let opt_used = std::env::var("MEDI_LLVM_OPT")
                                                    .unwrap_or_else(|_| "2".into());
                                                let cpu_used = std::env::var("MEDI_LLVM_CPU")
                                                    .unwrap_or_else(|_| "generic".into());
                                                let feats_used =
                                                    std::env::var("MEDI_LLVM_FEATURES")
                                                        .unwrap_or_else(|_| "".into());
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
                                            }
                                        } else {
                                            eprintln!("note: no --out=path provided; printing LLVM IR instead");
                                            match generate_ir_string_with_types_and_specs(
                                                &program, &fun_tys, &specs,
                                            ) {
                                                Ok(ir) => println!("{ir}"),
                                                Err(e) => {
                                                    eprintln!("error: IR generation failed: {e}");
                                                    std::process::exit(2);
                                                }
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!("error: wasm32 emission failed: {e}");
                                        std::process::exit(2);
                                    }
                                }
                            }
                            _ => {
                                // Fallback: just emit IR for other targets until implemented
                                match generate_ir_string_with_types_and_specs(
                                    &program, &fun_tys, &specs,
                                ) {
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
