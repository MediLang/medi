use criterion::{criterion_group, criterion_main, Criterion, black_box, BatchSize};
use medic_lexer::lexer::Lexer as OriginalLexer;
use std::time::Duration;

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
    let source = TEST_CONTENT.repeat(100);
    
    let mut group = c.benchmark_group("lexer");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    group.warm_up_time(Duration::from_secs(1));
    
    group.bench_function("original_lexer", |b| {
        b.iter_batched(
            || source.clone(),
            |s| {
                let lexer = OriginalLexer::new(&s);
                let tokens: Vec<_> = lexer.collect();
                black_box(tokens);
            },
            BatchSize::SmallInput
        )
    });
    
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .sample_size(10)
        .warm_up_time(Duration::from_secs(1))
        .measurement_time(Duration::from_secs(5));
    targets = bench_original_lexer
}

criterion_main!(benches);
