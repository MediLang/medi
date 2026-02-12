//! Example demonstrating the memory-efficient streaming lexer
//!
//! This example shows how to use the streaming lexer to process large files
//! with minimal memory usage.

use tlvxc_lexer::streaming_lexer::{LexerConfig, StreamingLexer};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

fn main() -> io::Result<()> {
    println!("Medi Streaming Lexer Demo");
    println!("--------------------------\n");

    // Example 1: Process a string directly
    println!("Example 1: Processing a small string");
    let source = r#"
        // Patient data
        let patient = Patient {
            id: "PATIENT-123",
            age: 45,
            conditions: [
                Condition { code: ICD10:E11.65, description: "Type 2 diabetes" },
                Condition { code: ICD10:I10, description: "Hypertension" }
            ]
        };
    "#;

    let config = LexerConfig {
        max_buffer_size: 128, // Small buffer for demonstration
        include_whitespace: false,
        include_comments: true, // Include comments in the token stream
    };

    let mut lexer = StreamingLexer::with_config(source, config);
    let mut token_count = 0;

    for token in &mut lexer {
        println!("Token: {token:?}");
        token_count += 1;
    }

    println!("\nProcessed {token_count} tokens\n");

    // Example 2: Process a large file in chunks
    println!("Example 2: Processing a large file in chunks");

    // Create a test file if it doesn't exist
    let temp_dir = std::env::temp_dir();
    let test_file = temp_dir.join("large_medical_script.tlvx");
    let test_file = test_file.to_str().unwrap();
    if !std::path::Path::new(test_file).exists() {
        generate_test_file(test_file, 1000)?;
    }

    // Process the file in chunks
    process_large_file(test_file)?;

    Ok(())
}

/// Process a large file in chunks to demonstrate memory efficiency
fn process_large_file(path: &str) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let start_time = std::time::Instant::now();
    let mut token_count = 0;
    let mut line_count = 0;

    for line in reader.lines() {
        let line = line?;
        line_count += 1;

        // Create a new lexer for this line
        let lexer = StreamingLexer::new(&line);

        // Process all tokens in this line
        for _token in lexer {
            token_count += 1;
            if token_count % 1000 == 0 {
                println!("Processed {token_count} tokens...");
            }
        }
    }

    let duration = start_time.elapsed();
    println!("\nFile processing complete!");
    println!("Lines processed: {line_count}");
    println!("Tokens processed: {token_count}");
    println!("Processing time: {duration:.2?}");
    println!(
        "Tokens per second: {:.0}",
        token_count as f64 / duration.as_secs_f64()
    );

    Ok(())
}

/// Generate a test file with sample medical data
fn generate_test_file(path: &str, patient_count: usize) -> io::Result<()> {
    println!("Generating test file with {patient_count} patients...");
    let mut file = std::fs::File::create(path)?;

    for i in 0..patient_count {
        let patient_data = format!(
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
        );

        file.write_all(patient_data.as_bytes())?;
    }

    println!("Test file generated: {path}");
    Ok(())
}
