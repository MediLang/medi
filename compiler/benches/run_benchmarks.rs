use std::time::Instant;
use medic_lexer::lexer::Lexer as OriginalLexer;

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

fn run_benchmark() -> Vec<u128> {
    let source = TEST_CONTENT.repeat(100);
    let mut results = Vec::with_capacity(10);

    for _ in 0..10 {
        let start = Instant::now();
        let lexer = OriginalLexer::new(&source);
        let tokens: Vec<_> = lexer.collect();
        let elapsed = start.elapsed();
        results.push(elapsed.as_micros());
        println!("Lexed {} tokens in {} μs", tokens.len(), elapsed.as_micros());
    }

    results
}

fn main() {
    println!("Running lexer benchmark...");
    let results = run_benchmark();
    
    let sum: u128 = results.iter().sum();
    let avg = sum as f64 / results.len() as f64;
    let min = results.iter().min().unwrap();
    let max = results.iter().max().unwrap();
    
    println!("\nBenchmark Results (μs):");
    println!("  Min: {}", min);
    println!("  Max: {}", max);
    println!("  Avg: {:.2}", avg);
}
