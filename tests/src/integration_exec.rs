use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use medic_lexer::streaming_lexer::StreamingLexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program, TokenSlice};

#[cfg(target_os = "linux")]
#[allow(dead_code)]
fn program_for(src: &str) -> medic_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

// macOS equivalents
#[cfg(target_os = "macos")]
#[allow(dead_code)]
fn program_for(src: &str) -> medic_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

#[cfg(target_os = "macos")]
fn link_with_clang(obj_path: &std::path::Path) -> PathBuf {
    let exe_path = obj_path.with_extension("exe");
    let status = Command::new("clang")
        .arg("-o")
        .arg(&exe_path)
        .arg(obj_path)
        .status()
        .expect("failed to spawn clang");
    assert!(
        status.success(),
        "clang failed to link object into executable"
    );
    exe_path
}

// Windows helpers
#[cfg(target_os = "windows")]
#[allow(dead_code)]
fn program_for(src: &str) -> medic_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

#[cfg(target_os = "windows")]
fn link_on_windows(obj_path: &std::path::Path) -> PathBuf {
    let exe_path = obj_path.with_extension("exe");
    // Try clang → clang-cl → cl
    if let Ok(s) = Command::new("clang")
        .arg("-o")
        .arg(&exe_path)
        .arg(obj_path)
        .status()
    {
        if s.success() {
            return exe_path;
        }
    }
    if let Ok(s) = Command::new("clang-cl")
        .arg(format!("/Fe:{}", exe_path.display()))
        .arg(obj_path)
        .status()
    {
        if s.success() {
            return exe_path;
        }
    }
    match Command::new("cl")
        .arg("/nologo")
        .arg(obj_path)
        .arg(format!("/Fe:{}", exe_path.display()))
        .status()
    {
        Ok(s) if s.success() => exe_path,
        Ok(s) => panic!("cl.exe failed with status: {:?}", s),
        Err(e) => panic!("failed to spawn any Windows linker: {e}"),
    }
}

#[cfg(target_os = "linux")]
#[allow(dead_code)]
fn link_with_clang(obj_path: &std::path::Path) -> PathBuf {
    let exe_path = obj_path.with_extension("exe");
    // Use -no-pie to avoid PIE linking issues on CI; rely on system libc/rt defaults
    // Try `clang` then fall back to `clang-15` (name on Ubuntu images)
    let try_link = |cmd: &str| -> std::io::Result<std::process::ExitStatus> {
        Command::new(cmd)
            .arg("-no-pie")
            .arg("-o")
            .arg(&exe_path)
            .arg(obj_path)
            .status()
    };
    let status = try_link("clang").or_else(|_| try_link("clang-15"));
    match status {
        Ok(s) if s.success() => {}
        Ok(s) => panic!("linker succeeded to run but failed with status: {s:?}"),
        Err(e) => panic!("failed to spawn clang/clang-15: {e}"),
    }
    exe_path
}

#[test]
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn integration_exec_simple_main_returns_exit_code() {
    let src = r#"
fn main() -> int {
  let a = 3;
  let b = 4;
  return a + b
}
"#;
    let program = program_for(src);
    let obj = medic_codegen_llvm::generate_x86_64_object_default(&program).expect("emit ok");

    // Write object to temp file
    let mut obj_path = std::env::temp_dir();
    obj_path.push("medi_codegen_integration.o");
    let mut f = File::create(&obj_path).expect("create obj file");
    f.write_all(&obj).expect("write obj bytes");

    // Link and run
    let exe = link_with_clang(&obj_path);
    let status = Command::new(&exe).status().expect("run exe");

    // On Linux, process return code equals main()'s return value
    let code = status.code().expect("no exit code");
    assert_eq!(code, 7, "expected exit code 7, got {code}");
}

#[test]
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn integration_exec_calls_function() {
    let src = r#"
fn inc(x: int) -> int { return x + 1 }
fn main() -> int { let y = inc(6); return y }
"#;
    let program = program_for(src);
    let obj = medic_codegen_llvm::generate_x86_64_object_default(&program).expect("emit ok");

    // Write object to temp file
    let mut obj_path = std::env::temp_dir();
    obj_path.push("medi_codegen_integration2.o");
    let mut f = File::create(&obj_path).expect("create obj file");
    f.write_all(&obj).expect("write obj bytes");

    // Link and run
    let exe = link_with_clang(&obj_path);
    let status = Command::new(&exe).status().expect("run exe");
    let code = status.code().expect("no exit code");
    assert_eq!(code, 7, "expected exit code 7, got {code}");
}

#[test]
#[cfg(target_os = "windows")]
fn integration_exec_windows() {
    let src = r#"
fn main() -> int {
  let a = 2;
  let b = 5;
  return a + b
}
"#;
    let program = program_for(src);
    let obj = medic_codegen_llvm::generate_x86_64_object_default(&program).expect("emit ok");

    // On Windows, COFF objects commonly use .obj
    let mut obj_path = std::env::temp_dir();
    obj_path.push("medi_codegen_integration.obj");
    let mut f = File::create(&obj_path).expect("create obj file");
    f.write_all(&obj).expect("write obj bytes");

    let exe = link_on_windows(&obj_path);
    let status = Command::new(&exe).status().expect("run exe");
    let code = status.code().expect("no exit code");
    assert_eq!(code, 7, "expected exit code 7, got {code}");
}

// On non-Linux targets, these integration tests are skipped because the current
// backend emission path is x86_64-unknown-linux-gnu oriented and CI is Linux.
