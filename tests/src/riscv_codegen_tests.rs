use tlvxc_codegen_llvm::generate_riscv32_object_with_opts_types_and_specs;
use tlvxc_lexer::streaming_lexer::StreamingLexer;
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program, TokenSlice};

fn program_for(src: &str) -> tlvxc_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

#[test]
fn riscv32_emit_object_simple_main() {
    // Minimal program should compile to a non-empty RV32 object
    let src = r#"
fn main() -> int {
  let a = 1;
  let b = 2;
  return a + b
}
"#;
    let program = program_for(src);
    // Use conservative defaults for embedded-like targets
    let obj = generate_riscv32_object_with_opts_types_and_specs(
        &program,
        2,              // opt level (Default)
        "generic",     // cpu
        "",            // features
        &[],            // no extra function type metadata
        &[],            // no specializations
    )
    .expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty riscv32 object bytes");
}
