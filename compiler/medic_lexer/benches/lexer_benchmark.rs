use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::{LexerConfig, StreamingLexer},
    token::TokenType,
};
use std::fs;
use std::path::Path;

fn load_test_file() -> String {
    let path = Path::new("benches/test_data/large_medical_script.medi");
    if !path.exists() {
        // Create a large test file if it doesn't exist
        let content = generate_large_medical_script();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("Failed to create test data directory");
        }
        fs::write(path, &content).expect("Failed to write test file");
        content
    } else {
        fs::read_to_string(path).expect("Failed to read test file")
    }
}

fn generate_large_medical_script() -> String {
    let mut content = String::new();
    
    // Generate a large medical script with various constructs
    for i in 0..1000 {
        content.push_str(&format!(
            r#"// Patient data for patient {}
let patient_{0} = Patient {{
    id: "PATIENT-{0}",
    name: "Patient {0}",
    age: {1},
    conditions: [
        Condition {{ code: ICD10:E11.65, description: "Type 2 diabetes mellitus with hyperglycemia" }},
        Condition {{ code: ICD10:I10, description: "Essential (primary) hypertension" }}
    ],
    medications: [
        Medication {{ code: RxNorm:197361, name: "metFORMIN 500 mg" }},
        Medication {{ code: RxNorm:197379, name: "lisinopril 10 mg" }}
    ]
}};

// Calculate some metrics
let bmi_{0} = calculate_bmi(weight_kg_{0}, height_m_{0});
let map_{0} = calculate_map(systolic_{0}, diastolic_{0});

// Decision making based on vitals
if bmi_{0} > 30.0 {{
    recommend_lifestyle_changes(patient_{0}.id);
    if map_{0} > 100.0 {{
        escalate_care(patient_{0}.id, "Elevated blood pressure");
    }}
}}

"#,
            i,
            30 + (i % 50),
        ));
    }
    
    content
}

fn bench_original_lexer(c: &mut Criterion) {
    let source = load_test_file();
    
    c.benchmark_group("lexer")
        .throughput(Throughput::Bytes(source.len() as u64))
        .bench_function("original", |b| {
            b.iter(|| {
                let mut lexer = OriginalLexer::new(&source);
                let tokens: Vec<_> = lexer.collect();
                tokens
            })
        });
}

fn bench_streaming_lexer(c: &mut Criterion) {
    let source = load_test_file();
    let config = LexerConfig {
        max_buffer_size: 1024,
        include_whitespace: false,
    };
    
    c.benchmark_group("lexer")
        .throughput(Throughput::Bytes(source.len() as u64))
        .bench_function("streaming", |b| {
            b.iter(|| {
                let mut lexer = StreamingLexer::with_config(&source, config.clone());
                let tokens: Vec<_> = lexer.collect();
                tokens
            })
        });
}

criterion_group!(
    name = benches;
    config = Criterion::default()
        .sample_size(10)  // Fewer samples for faster benchmarks
        .measurement_time(std::time::Duration::from_secs(10));
    targets = bench_original_lexer, bench_streaming_lexer
);

criterion_main!(benches);
