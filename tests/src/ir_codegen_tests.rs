use medic_ast::ast::StatementNode;
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

// Quantity IR feature-guarded tests
mod quantity_ir_tests {
    use super::ir_for;

    #[test]
    fn quantity_known_conversion_implicit_pattern() {
        let src = r#"
fn conv(a: int) {
  // implicit quantity pattern: (a * mg) -> g
  (a * mg) -> g;
}
"#;
        let ir = ir_for(src);
        // Expect a multiply on the value field for conversion
        assert!(ir.contains("uconv.qty"));
        // Should not call runtime quantity conversion for a known pair
        assert!(!ir.contains("medi_convert_q"));
    }

    #[test]
    fn quantity_unknown_conversion_calls_runtime() {
        let src = r#"
fn conv_unknown() {
  (5 * mg) -> blargh;
}
"#;
        let ir = ir_for(src);
        assert!(ir.contains("medi_convert_q"));
    }

    #[test]
    fn quantity_add_same_units_emits_q_add() {
        let src = r#"
fn addq(a: int, b: int) {
  (a * mg) + (b * mg);
}
"#;
        let ir = ir_for(src);
        // Should produce a quantity add on the value field
        assert!(ir.contains("q.add"));
    }

    #[test]
    fn quantity_add_mismatched_units_errors() {
        let src = r#"
fn addq_bad() {
  (5 * mg) + (3 * g);
}
"#;
        // Expect codegen to fail; ir_for panics on error, so catch by checking generate_ir_string error
        // Here we rely on failure manifesting as missing q.add and presence of error string in debug
        let result = std::panic::catch_unwind(|| ir_for(src));
        assert!(
            result.is_err(),
            "expected codegen error for mismatched unit add"
        );
    }

    #[test]
    fn quantity_sub_same_units_emits_q_sub() {
        let src = r#"
fn subq(a: int, b: int) {
  (a * mg) - (b * mg);
}
"#;
        let ir = ir_for(src);
        assert!(ir.contains("q.sub"));
    }

    #[test]
    fn quantity_comparison_disallowed_even_after_conversion() {
        let src = r#"
fn cmpq() {
  // Even converting one side to the other's unit, comparisons on Quantity are disallowed currently
  ((5 * mg) -> g) == (3 * g);
}
"#;
        let result = std::panic::catch_unwind(|| ir_for(src));
        assert!(
            result.is_err(),
            "expected codegen error for quantity comparison"
        );
    }

    #[test]
    fn quantity_conversion_chain_known_pairs_two_multiplies() {
        let src = r#"
fn chain(a: int) {
  // Known conversions chained: mg -> g -> mg
  ((a * mg) -> g) -> mg;
}
"#;
        let ir = ir_for(src);
        // Expect two multiplies and no runtime quantity conversion
        let count = ir.match_indices("uconv.qty").count();
        assert!(
            count >= 2,
            "expected at least two uconv.qty in chained conversions, got {count}\nIR:\n{ir}"
        );
        assert!(!ir.contains("medi_convert_q"));
    }

    #[test]
    fn quantity_conversion_chain_mg_to_g_to_kg() {
        let src = r#"
fn chain2(a: int) {
  // Known conversions chained: mg -> g -> kg
  ((a * mg) -> g) -> kg;
}
"#;
        let ir = ir_for(src);
        let count = ir.match_indices("uconv.qty").count();
        assert!(
            count >= 2,
            "expected at least two uconv.qty in chained conversions, got {count}\nIR:\n{ir}"
        );
        assert!(!ir.contains("medi_convert_q"));
    }

    #[test]
    fn quantity_dimension_mismatch_conversion_errors_without_runtime() {
        let src = r#"
fn bad_conv() {
  // Dimension mismatch: mg -> L should error at codegen/typeck, not call runtime
  (5 * mg) -> L;
}
"#;
        let result = std::panic::catch_unwind(|| ir_for(src));
        assert!(
            result.is_err(),
            "expected error for dimension mismatch conversion"
        );
    }
}

#[test]
fn ir_unit_conversion_runtime_fallback() {
    // Use unknown units to trigger runtime fallback to medi_convert
    let src = r#"
fn f() -> float {
  let x = 5 -> qux;     // LHS numeric, RHS unknown unit; parser treats RHS as identifier
  return x
}
"#;
    let ir = ir_for(src);
    // Expect a call to the runtime shim when compile-time factor is not available
    assert!(ir.contains("medi_convert"));
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
fn ir_struct_field_assignment_and_match() {
    let src = r#"
 type Patient { id: int, score: int }
 fn h() {
   let p = Patient { id: 1, score: 10 };
   p.score = 11;
   let v = p.score;
 }
"#;
    let ir = ir_for(src);
    assert!(ir.contains("store") && ir.contains("load"));
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
