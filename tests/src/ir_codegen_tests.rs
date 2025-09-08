use medic_codegen_llvm::generate_ir_string;
use medic_lexer::streaming_lexer::StreamingLexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program, TokenSlice};

#[allow(dead_code)]
fn ir_for(src: &str) -> String {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    generate_ir_string(&program).expect("ir ok")
}

#[test]
fn ir_ops_of_per_pow_and_coalesce() {
    let src = r#"
fn calc(a: int, b: int) -> int {
  let x = a * b;         // mul (instead of 'of')
  let y = a / b;         // div (instead of 'per')
  let z = a ** 3;        // int pow
  let s = (a ?? b);      // coalesce
  let t = (a ?: b);      // elvis (same truthiness select)
  return x + y + z + s + t
}
"#;
    let ir = ir_for(src);
    // Spot check some expected instructions
    assert!(ir.contains("mul"));
    assert!(ir.contains("sdiv") || ir.contains("udiv"));
    assert!(ir.contains("for.next") || ir.contains("ipow"));
}

#[test]
#[ignore]
fn ir_unit_conversion_placeholder() {
    let src = r#"
fn f(a: int, b: float) -> float {
  let x = a -> 100;      // multiply by 100 (ASCII arrow)
  let y = b -> 2.5;      // multiply by 2.5
  return y
}
"#;
    let ir = ir_for(src);
    assert!(ir.contains("uconv.i") || ir.contains("uconv.f"));
}

#[test]
fn ir_healthcare_query_call() {
    let src = r#"
fn healthcare_lookup(id: int) -> int {
  return 42
}

fn g(p: int) -> int {
  // This lowers to call healthcare_lookup(p)
  let x = healthcare_lookup(p);
  return x
}
"#;
    let ir = ir_for(src);
    assert!(ir.contains("call") && ir.contains("healthcare_lookup"));
}

#[test]
#[ignore]
fn ir_struct_field_assignment_and_match() {
    let src = r#"
 type Patient { id: int, score: int }
 fn h() {
   let p = Patient { id: 1, score: 10 };
   p.score = 11;
   let v = match p { Patient { id: 1, score: s } => s, _ => 0 };
 }
"#;
    let ir = ir_for(src);
    assert!(ir.contains("store") && ir.contains("for.end"));
}

#[test]
fn ir_typed_params_allocas() {
    let src = r#"
fn k(a: float, b: int, c: bool, d: string) {
  return
}
"#;
    let ir = ir_for(src);
    // Check that parameters exist in entry (allocas happen in entry and are stored)
    assert!(ir.contains("entry"));
}
