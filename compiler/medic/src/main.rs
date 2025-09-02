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
    for a in args.iter().skip(1) {
        match a.as_str() {
            "--json" | "-j" => mode = OutputMode::Json,
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
