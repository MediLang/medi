use std::env;
use std::fs;
use std::io::{self, Read};

use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{
    parse_program_recovering, parse_program_with_diagnostics, render_snippet, TokenSlice,
};

fn main() {
    // Read source either from first CLI argument (file path) or stdin
    let args: Vec<String> = env::args().collect();
    let source = if args.len() > 1 {
        match fs::read_to_string(&args[1]) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error: failed to read '{}': {e}", &args[1]);
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
        Ok(_program) => {
            // Also run the recovering path to surface non-fatal diagnostics (e.g., lexer errors)
            let mut diags = Vec::new();
            let _ = parse_program_recovering(TokenSlice::new(&tokens), &mut diags);
            for d in diags {
                eprintln!("{}", render_snippet(&d, &source));
            }
        }
        Err(diag) => {
            eprintln!("{}", render_snippet(&diag, &source));
            std::process::exit(1);
        }
    }
}
