use criterion::{criterion_group, criterion_main, Criterion};
use medic_lexer::streaming_lexer::StreamingLexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program, TokenSlice};

fn parse_program_from(src: &str) -> medic_ast::ast::ProgramNode {
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    program
}

fn bench_ir_and_object(c: &mut Criterion) {
    let smoke = std::env::var("MEDI_BENCH_SMOKE").ok().is_some();

    let mut group = c.benchmark_group("codegen");
    if smoke {
        group.sample_size(10);
        group.measurement_time(std::time::Duration::from_secs(2));
        group.warm_up_time(std::time::Duration::from_millis(500));
    }

    let src_small = r#"
fn add(a: int, b: int) -> int { return a + b }
fn main() -> int { let x = add(3, 4); return x }
"#;

    let src_loop = r#"
fn main() -> int {
  let s = 0;
  for i in 0 .. 256 { s = s + i; }
  return s
}
"#;

    let program_small = parse_program_from(src_small);
    let program_loop = parse_program_from(src_loop);

    group.bench_function("generate_ir_small", |b| {
        b.iter(|| {
            let ir = medic_codegen_llvm::generate_ir_string(&program_small).expect("ir");
            criterion::black_box(ir);
        })
    });

    group.bench_function("generate_object_small", |b| {
        b.iter(|| {
            let obj =
                medic_codegen_llvm::generate_x86_64_object_default(&program_small).expect("obj");
            criterion::black_box(obj.len());
        })
    });

    group.bench_function("generate_ir_loop", |b| {
        b.iter(|| {
            let ir = medic_codegen_llvm::generate_ir_string(&program_loop).expect("ir");
            criterion::black_box(ir);
        })
    });

    group.bench_function("generate_object_loop", |b| {
        b.iter(|| {
            let obj =
                medic_codegen_llvm::generate_x86_64_object_default(&program_loop).expect("obj");
            criterion::black_box(obj.len());
        })
    });

    group.finish();
}

criterion_group!(benches, bench_ir_and_object);
criterion_main!(benches);
