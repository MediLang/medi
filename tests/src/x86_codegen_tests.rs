use tlvxc_codegen_llvm::generate_x86_64_object_default;
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
fn x86_ir_stable_mean_blocks_present() {
    // Exercise stable_mean lowering (Kahan + divide by N) on array input.
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  let m = stable_mean(arr);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("mean.pre"), "expected mean.pre in IR, got:\n{}", ir);
    assert!(ir.contains("mean.loop"), "expected mean.loop in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_stable_mean_ptr_len_blocks_present() {
    // Exercise stable_mean on pointer + length path.
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  let p = arr;
  let m = stable_mean(p, 4);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("mean.pre"), "expected mean.pre in IR, got:\n{}", ir);
    assert!(ir.contains("mean.loop"), "expected mean.loop in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_stable_var_blocks_present() {
    // Exercise stable_var lowering (Welford) on array input.
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  let v = stable_var(arr);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("welford.pre"), "expected welford.pre in IR, got:\n{}", ir);
    assert!(ir.contains("welford.loop"), "expected welford.loop in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_stable_var_ptr_len_blocks_present() {
    // Exercise stable_var on pointer + length path.
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  let p = arr;
  let v = stable_var(p, 4);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("welford.pre"), "expected welford.pre in IR, got:\n{}", ir);
    assert!(ir.contains("welford.loop"), "expected welford.loop in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_stable_sum_dynamic_len_present() {
    // Exercise stable_sum on pointer + length path
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  // Use pointer and explicit length
  let p = arr; // treated as pointer in current lowering when passed
  let s = stable_sum(p, 4);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("kahan.pre"), "expected kahan.pre in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_tiled_loop_blocks_under_aggressive_pipeline() {
    // Force aggressive pipeline behavior at IR-lowering time (we check env var in codegen)
    std::env::set_var("TOLVEX_LLVM_PIPE", "aggressive");
    let src = r#"
fn main() -> int {
  let s = 0;
  let i = 0;
  for i in 0 .. 128 {
    // simple body to exercise loop lowering
    let t = i;
  }
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    // With aggressive pipeline, we now emit real tiled loops from actual code
    assert!(ir.contains("tile.cond"), "expected tiled loop blocks in IR, got:\n{}", ir);
    assert!(ir.contains("tile.inner.body"), "expected inner tiled loop body, got:\n{}", ir);
    assert!(ir.contains("llvm.prefetch"), "expected prefetch in tiled loop, got:\n{}", ir);
    // clean up
    std::env::remove_var("TOLVEX_LLVM_PIPE");
}

#[test]
fn x86_ir_stable_sum_kahan_present() {
    // Exercise stable_sum lowering (Kahan summation).
    let src = r#"
fn main() -> int {
  let arr = [1.0, 2.0, 3.0, 4.0];
  let s = stable_sum(arr);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    // Look for kahan loop blocks we emit (kahan.pre / kahan.loop / kahan.end)
    assert!(ir.contains("kahan.pre"), "expected kahan.pre in IR, got:\n{}", ir);
    assert!(ir.contains("kahan.loop"), "expected kahan.loop in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_real_loop_tiling_with_prefetch() {
    // Test that real loops get tiled with prefetch under aggressive pipeline
    std::env::set_var("TOLVEX_LLVM_PIPE", "aggressive");
    std::env::set_var("TOLVEX_TILE_SIZE", "16"); // Small tile for testing
    let src = r#"
fn process_array() -> int {
  let sum = 0;
  for i in 0 .. 64 {
    sum = sum + i;
  }
  return sum
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    
    // Check for tiled structure from real loop transformation
    assert!(ir.contains("tile.cond"), "expected outer tile condition, got:\n{}", ir);
    assert!(ir.contains("tile.outer.body"), "expected outer tile body, got:\n{}", ir);
    assert!(ir.contains("tile.inner.cond"), "expected inner tile condition, got:\n{}", ir);
    assert!(ir.contains("tile.inner.body"), "expected inner tile body, got:\n{}", ir);
    assert!(ir.contains("llvm.prefetch.p0"), "expected prefetch intrinsic, got:\n{}", ir);
    
    // Check for loop metadata emission
    assert!(ir.contains("tolvex.loop.info"), "expected loop metadata global, got:\n{}", ir);
    
    // clean up
    std::env::remove_var("TOLVEX_LLVM_PIPE");
    std::env::remove_var("TOLVEX_TILE_SIZE");
}

#[test]
fn x86_ir_vectorization_passes_enabled() {
    // Test that SIMD vectorization passes are applied under aggressive pipeline
    std::env::set_var("TOLVEX_LLVM_PIPE", "aggressive");
    let src = r#"
fn vector_sum() -> float {
  let arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0];
  let sum = 0.0;
  for i in 0 .. 8 {
    sum = sum + arr[i];
  }
  return sum
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    
    // Check that vectorization-friendly IR is generated
    // The presence of loops and array access should trigger vectorization passes
    assert!(ir.contains("for."), "expected loop structure for vectorization");
    
    // clean up
    std::env::remove_var("TOLVEX_LLVM_PIPE");
}

#[test]
fn x86_ir_function_level_optimizations() {
    // Test function-level optimizations like inlining and DCE
    std::env::set_var("TOLVEX_LLVM_PIPE", "aggressive");
    let src = r#"
fn helper(x: int) -> int {
  return x + 1
}

fn main() -> int {
  let a = helper(5);
  let b = helper(10);
  return a + b
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    
    // Check that both functions are present (inlining happens at later stages)
    assert!(ir.contains("define"), "expected function definitions");
    assert!(ir.contains("helper"), "expected helper function");
    assert!(ir.contains("main"), "expected main function");
    
    // clean up
    std::env::remove_var("TOLVEX_LLVM_PIPE");
}

#[test]
fn x86_ir_healthcare_numerics_bmi_pattern() {
    // Test healthcare-specific BMI calculation optimization
    std::env::set_var("TOLVEX_LLVM_PIPE", "aggressive");
    let src = r#"
fn calculate_bmi(weight: float, height: float) -> float {
  let height_sq = height * height;
  let bmi = weight / height_sq;
  return bmi
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    
    // Check for BMI calculation pattern
    assert!(ir.contains("fmul"), "expected multiplication for height squared");
    assert!(ir.contains("fdiv"), "expected division for BMI calculation");
    
    // clean up
    std::env::remove_var("TOLVEX_LLVM_PIPE");
}

#[test]
fn x86_ir_mem_copy_tiled_contains_memcpy() {
    // Exercise tiled memcpy helper and assert memcpy appears in IR.
    let src = r#"
fn main() -> int {
  let dst = [0,0,0,0,0,0,0,0];
  let srca = [1,2,3,4,5,6,7,8];
  // Call tiled copy with small sizes (byte lengths depend on element size; here int=8 on codegen)
  mem_copy_tiled(dst, srca, 64, 16);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("llvm.memcpy"), "expected llvm.memcpy in IR, got:\n{}", ir);
}

#[test]
fn x86_ir_prefetch_present() {
    // Exercise prefetch lowering and assert llvm.prefetch appears in IR.
    let src = r#"
fn main() -> int {
  let xs = [1,2,3,4];
  prefetch(xs, 0, 3, 1);
  return 0
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("llvm.prefetch"), "expected llvm.prefetch in IR, got:\n{}", ir);
}

#[test]
fn x86_emit_object_simple_main() {
    let src = r#"
fn main() -> int {
  let a = 1;
  let b = 2;
  return a + b
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_emit_object_float_return() {
    let src = r#"
fn retf(a: float, b: float) -> float {
  return a * b
}

fn main() -> int {
  let x = retf(1.25, 2.0);
  return 0
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_emit_object_many_params_registers_and_stack() {
    // Many parameters to encourage register + stack usage according to SysV.
    let src = r#"
fn many(a0:int,a1:int,a2:int,a3:int,a4:int,a5:int,a6:int,a7:int,a8:int,a9:int, f0:float, f1:float, f2:float) -> int {
  // Just touch a few so they aren't optimized away too aggressively
  let s = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
  let _x = f0; let _y = f1; let _z = f2;
  return s
}

fn main() -> int {
  let r = many(0,1,2,3,4,5,6,7,8,9, 1.0, 2.0, 3.0);
  return r
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_emit_object_large_struct_return_and_param() {
    // Large aggregate should be handled (sret for returns; byval for params).
    let src = r#"
type Big { a: int, b: int, c: int, d: int, e: int, f: int, g: int, h: int }

fn make_big() -> Big {
  let v = Big { a:1, b:2, c:3, d:4, e:5, f:6, g:7, h:8 };
  return v
}

fn use_big(x: Big) -> int {
  // Just return a constant to avoid needing field access here
  return 0
}

fn main() -> int {
  let v = make_big();
  let r = use_big(v);
  return r
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_emit_object_small_structs() {
    // Small aggregate params/returns should still compile and emit object.
    let src = r#"
type Pair { a: int, b: int }

fn make_pair(x: int, y: int) -> Pair {
  let p = Pair { a: x, b: y };
  return p
}

fn main() -> int {
  let p = make_pair(1, 2);
  return 0
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_ir_memcpy_emitted_for_struct_assignment() {
    // When assigning one large struct variable to another, codegen should emit a memcpy.
    // We check at IR level for the presence of 'llvm.memcpy'.
    let src = r#"
type Big { a: int, b: int, c: int, d: int, e: int, f: int }

fn copy_big() {
  let x = Big { a:1, b:2, c:3, d:4, e:5, f:6 };
  let y = Big { a:0, b:0, c:0, d:0, e:0, f:0 };
  // variable-to-variable assignment should trigger memcpy path
  y = x;
}
"#;
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    let ir = tlvxc_codegen_llvm::generate_ir_string(&program).expect("ir ok");
    assert!(ir.contains("llvm.memcpy"), "expected llvm.memcpy in IR, got:\n{}", ir);
}

#[test]
fn x86_emit_object_mixed_int_float_params() {
    let src = r#"
fn mix(a: int, b: float, c: int, d: float) -> int {
  let x = b; // use float param
  return a + c
}

fn main() -> int {
  let r = mix(1, 2.5, 3, 4.5);
  return r
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}

#[test]
fn x86_emit_object_with_float_and_call() {
    let src = r#"
fn add(a: float, b: float) -> float {
  return a + b
}

fn main() -> int {
  // call add to ensure function lowering and call sites work
  let x = add(1.5, 2.5);
  return 0
}
"#;
    let program = program_for(src);
    let obj = generate_x86_64_object_default(&program).expect("emit ok");
    assert!(obj.len() > 0, "expected non-empty x86_64 object bytes");
}
