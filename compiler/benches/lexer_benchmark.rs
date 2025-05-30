use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    LexerConfig,
    streaming_lexer::StreamingLexer,
    chunked_lexer::{ChunkedLexer, ChunkedLexerConfig},
};

// Simple test content for benchmarking
const TEST_CONTENT: &str = r#"
// Sample patient data
let patient = {
    id: "PATIENT-123",
    name: "John Doe",
    age: 42,
    conditions: [
        { code: "ICD10:E11.65", description: "Type 2 diabetes with hyperglycemia" },
        { code: "ICD10:I10", description: "Essential hypertension" }
    ],
    medications: [
        { code: "RxNorm:197361", name: "metFORMIN 500 mg" },
        { code: "RxNorm:197379", name: "lisinopril 10 mg" }
    ]
};

// Calculate BMI
fn calculate_bmi(weight_kg: f64, height_m: f64) -> f64 {
    weight_kg / (height_m * height_m)
}

// Main function
fn main() {
    let bmi = calculate_bmi(75.0, 1.75);
    println!("Patient BMI: {:.2}", bmi);
    
    if bmi > 30.0 {
        println!("Warning: High BMI detected");
    }
}
"#;

fn bench_original_lexer(c: &mut Criterion) {
    let source = TEST_CONTENT.repeat(100);  // Repeat to get more significant measurements
    let len = source.len() as u64;

    c.benchmark_group("lexer")
        .throughput(Throughput::Bytes(len))
        .bench_function("original", |b| {
            b.iter(|| {
                let lexer = OriginalLexer::new(&source);
                let tokens: Vec<_> = lexer.collect();
                std::hint::black_box(tokens)
            })
        });
}

fn bench_streaming_lexer(c: &mut Criterion) {
    let source = TEST_CONTENT.repeat(100);
    let len = source.len() as u64;
    let config = LexerConfig {
        max_buffer_size: 1024,
        include_whitespace: false,
        include_comments: false,
    };

    c.benchmark_group("lexer")
        .throughput(Throughput::Bytes(len))
        .bench_function("streaming", |b| {
            b.iter(|| {
                let lexer = StreamingLexer::with_config(&source, config);
                let tokens: Vec<_> = lexer.collect();
                std::hint::black_box(tokens)
            })
        });
}

fn bench_chunked_lexer(c: &mut Criterion) {
    let source = TEST_CONTENT.repeat(100);
    // Use Box::leak to ensure the content lives for the entire benchmark
    let content_bytes = Box::leak(Box::new(source.into_bytes()));
    let len = content_bytes.len() as u64;
    
    c.benchmark_group("chunked_lexer")
        .throughput(Throughput::Bytes(len))
        .bench_function("tokenize", |b| {
            b.iter(|| {
                let cursor = std::io::Cursor::new(content_bytes.as_slice());
                let lexer = ChunkedLexer::from_reader(
                    cursor,
                    ChunkedLexerConfig {
                        chunk_size: 1024,
                        ..Default::default()
                    }
                );
                let tokens: Vec<_> = lexer.collect();
                assert!(!tokens.is_empty(), "No tokens were generated");
                tokens
            })
        });
}

criterion_group!(
    name = benches;
    config = Criterion::default()
        .sample_size(10)
        .warm_up_time(std::time::Duration::from_secs(1))
        .measurement_time(std::time::Duration::from_secs(5));
    targets = bench_original_lexer, bench_streaming_lexer, bench_chunked_lexer
);

criterion_main!(benches);
