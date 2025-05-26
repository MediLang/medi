use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::{LexerConfig, StreamingLexer},
};
use std::fs;
use std::path::Path;

#[test]
fn test_memory_usage_comparison() {
    // Generate or load test data
    let source = generate_large_medical_script();
    
    // Test original lexer memory usage
    let original_mem = measure_memory_usage(|| {
        let mut lexer = OriginalLexer::new(&source);
        let _tokens: Vec<_> = lexer.collect();
    });
    
    // Test streaming lexer memory usage
    let streaming_mem = measure_memory_usage(|| {
        let config = LexerConfig {
            max_buffer_size: 1024,
            include_whitespace: false,
        };
        let mut lexer = StreamingLexer::with_config(&source, config);
        let _tokens: Vec<_> = lexer.collect();
    });
    
    println!("Original lexer memory usage: {} KB", original_mem / 1024);
    println!("Streaming lexer memory usage: {} KB", streaming_mem / 1024);
    
    // Assert that streaming lexer uses significantly less memory
    // (at least 50% less in this case)
    assert!(
        streaming_mem < original_mem / 2,
        "Expected streaming lexer to use less memory"
    );
}

fn generate_large_medical_script() -> String {
    let path = Path::new("tests/test_data/large_medical_script.medi");
    if path.exists() {
        return fs::read_to_string(path).expect("Failed to read test file");
    }
    
    // Create test data directory if it doesn't exist
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("Failed to create test data directory");
    }
    
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
    
    // Save the generated content for future test runs
    fs::write(path, &content).expect("Failed to write test file");
    content
}

fn measure_memory_usage<F: FnOnce()>(f: F) -> usize {
    // Get memory usage before execution
    let before = memory_stats::memory_stats()
        .map(|m| m.physical_mem)
        .unwrap_or(0);
    
    // Execute the function
    f();
    
    // Get memory usage after execution
    let after = memory_stats::memory_stats()
        .map(|m| m.physical_mem)
        .unwrap_or(0);
    
    // Return the difference in bytes
    after.saturating_sub(before)
}
