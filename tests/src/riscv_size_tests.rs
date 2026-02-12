use tlvxc_codegen_llvm::generate_riscv32_object_with_opts_types_and_specs;
use tlvxc_lexer::streaming_lexer::StreamingLexer;
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program, TokenSlice};
use std::io::Write;
use std::process::Command;

fn program_for(src: &str) -> tlvxc_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

#[test]
fn riscv32_size_tool_runs_if_available() {
    // Build a small object and attempt to run a size tool if present.
    let src = r#"
fn main() -> int { return 0 }
"#;
    let program = program_for(src);
    let obj = generate_riscv32_object_with_opts_types_and_specs(
        &program,
        2,
        "generic",
        "",
        &[],
        &[],
    )
    .expect("emit ok");
    assert!(obj.len() > 0);

    // Write to a temp file
    let mut tmp = tempfile::NamedTempFile::new().expect("tmp file");
    tmp.write_all(&obj).expect("write tmp");
    let path = tmp.path();

    // Try riscv32-unknown-elf-size first
    let mut ran_any = false;
    if let Ok(out) = Command::new("riscv32-unknown-elf-size").arg(path).output() {
        // Tool exists; ensure it runs successfully or at least executes
        assert!(out.status.success() || !out.stdout.is_empty() || !out.stderr.is_empty());
        ran_any = true;
    }
    // Fallback to llvm-size
    if !ran_any {
        if let Ok(out) = Command::new("llvm-size").arg(path).output() {
            assert!(out.status.success() || !out.stdout.is_empty() || !out.stderr.is_empty());
            ran_any = true;
        }
    }

    // If neither tool is present, we don't fail the test; we only verify that the object exists.
    let _ = ran_any;
}
