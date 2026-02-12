use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use tlvxc_lexer::lexer::Lexer;
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program, TokenSlice};

fn main() {
    println!("Loading test file...");
    let manifest_dir: &str = env!("CARGO_MANIFEST_DIR");
    let candidates = [
        // When running with benches as the manifest, file lives next to Cargo.toml
        "large_test_file.tlvx",
        // Fallback if executed from workspace root paths
        "benches/large_test_file.tlvx",
        // Another fallback if run from repo root directly
        "compiler/benches/large_test_file.tlvx",
    ];
    println!("CARGO_MANIFEST_DIR={manifest_dir}");
    let mut path: Option<PathBuf> = None;
    for cand in candidates.iter() {
        let p = Path::new(manifest_dir).join(cand);
        println!("Trying: {} (exists: {})", p.display(), p.exists());
        if p.exists() {
            path = Some(p);
            break;
        }
    }
    let path = path.expect("Could not locate large_test_file.tlvx");
    println!("Using file: {}", path.display());
    let content = fs::read_to_string(&path).expect("Failed to read test file");

    // Lex once and keep tokens to avoid timing file IO
    println!("Lexing...");
    let tokens: Vec<Token> = Lexer::new(&content).collect();
    println!("Tokenized: {} tokens", tokens.len());

    // Leak tokens to get a 'static slice for TokenSlice convenience, matching existing tests
    let boxed = Box::new(tokens);
    let leaked: &'static [Token] = Box::leak(boxed);

    // Warm up (ignore success/failure; file might not be fully valid Tolvex source)
    println!("Warming up parser...");
    let _ = parse_program(TokenSlice(leaked));

    // Benchmark N iterations
    let iters = 5;
    let mut times = Vec::with_capacity(iters);

    for i in 0..iters {
        print!("\rParsing iteration {}/{}...", i + 1, iters);
        std::io::stdout().flush().unwrap();
        let start = Instant::now();
        let result = parse_program(TokenSlice(leaked));
        let elapsed = start.elapsed();
        // Ensure some work is used to avoid optimizing away
        let stmt_count = match &result {
            Ok((_rest, program)) => program.statements.len(),
            Err(_) => 0,
        };
        let status = if result.is_ok() { "OK" } else { "ERR" };
        println!(
            "\rIteration {}: status={}, {} statements, {} ms",
            i + 1,
            status,
            stmt_count,
            elapsed.as_millis()
        );
        times.push(elapsed.as_micros());
    }

    if !times.is_empty() {
        let sum: u128 = times.iter().copied().sum();
        let avg = sum as f64 / times.len() as f64;
        let min = *times.iter().min().unwrap();
        let max = *times.iter().max().unwrap();
        println!(
            "\nParser benchmark over {} runs: avg {:.2} ms, min {:.2} ms, max {:.2} ms",
            times.len(),
            avg / 1000.0,
            min as f64 / 1000.0,
            max as f64 / 1000.0
        );
    }
}
