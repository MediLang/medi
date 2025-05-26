use medic_lexer::{
    streaming_lexer::{LexerConfig, StreamingLexer},
    token::Token,
};
use medic_parser::parser::parse_program;

#[test]
fn test_parser_integration() {
    // Example Medi code to parse
    let source = r#"
        // Patient data
        let patient = Patient {
            id: "PATIENT-123",
            age: 45,
            conditions: [
                Condition { 
                    code: ICD10:E11.65, 
                    description: "Type 2 diabetes" 
                },
                Condition { 
                    code: ICD10:I10, 
                    description: "Hypertension" 
                }
            ]
        };

        // Calculate BMI
        let bmi = calculate_bmi(patient.weight_kg, patient.height_m);
        
        // Make a decision based on BMI
        if bmi > 25.0 {
            recommend_lifestyle_changes(patient.id);
        }
    "#;

    // Create a streaming lexer with default config
    let config = LexerConfig {
        max_buffer_size: 256,
        include_whitespace: false,
    };
    
    let mut lexer = StreamingLexer::with_config(source, config);
    
    // Collect all tokens
    let tokens: Vec<Token> = lexer.collect();
    
    // Parse the tokens
    let result = parse_program(tokens.as_slice().into());
    
    // Verify parsing was successful
    assert!(result.is_ok(), "Failed to parse tokens: {:?}", result.err());
    
    // Get the AST
    let (remaining, ast) = result.unwrap();
    
    // Verify we consumed all tokens
    assert!(remaining.is_empty(), "Not all tokens were consumed");
    
    // Verify the AST structure
    assert!(!ast.statements.is_empty(), "AST should contain statements");
    
    // Print a summary of the parsed program
    println!("Successfully parsed program with {} statements", ast.statements.len());
}

#[test]
fn test_large_file_parsing() {
    // Generate a large test file
    let test_file = "test_data/large_medical_script.medi";
    if !std::path::Path::new(test_file).exists() {
        generate_test_file(test_file, 100).expect("Failed to generate test file");
    }
    
    // Read the test file
    let source = std::fs::read_to_string(test_file).expect("Failed to read test file");
    
    // Create a streaming lexer with a small buffer to test chunking
    let config = LexerConfig {
        max_buffer_size: 128,  // Small buffer to force chunking
        include_whitespace: false,
    };
    
    let start_time = std::time::Instant::now();
    let mut lexer = StreamingLexer::with_config(&source, config);
    
    // Collect all tokens
    let tokens: Vec<Token> = lexer.collect();
    
    println!("Lexed {} tokens in {:.2?}", tokens.len(), start_time.elapsed());
    
    // Parse the tokens
    let parse_start = std::time::Instant::now();
    let result = parse_program(tokens.as_slice().into());
    
    // Verify parsing was successful
    assert!(result.is_ok(), "Failed to parse large file: {:?}", result.err());
    
    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Not all tokens were consumed");
    
    println!("Parsed AST with {} statements in {:.2?}", 
        ast.statements.len(), 
        parse_start.elapsed()
    );
}

/// Helper function to generate a test file with sample medical data
fn generate_test_file(path: &str, patient_count: usize) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::Write;
    
    // Create parent directories if they don't exist
    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent)?;
    }
    
    let mut file = File::create(path)?;
    
    // Add some helper functions
    writeln!(file, "// Helper functions")?;
    writeln!(file, "fn calculate_bmi(weight_kg: f64, height_m: f64) -> f64 {{\n    weight_kg / (height_m * height_m)\n}}\n")?;
    
    writeln!(file, "fn recommend_lifestyle_changes(patient_id: &str) {{\n    println!("Recommend lifestyle changes for {{}}", patient_id);\n}}\n")?;
    
    // Generate patient data
    for i in 0..patient_count {
        writeln!(file, "// Patient {}", i)?;
        writeln!(file, "let patient_{} = Patient {{\n    id: \"PATIENT-{}\", i, i)?;
        writeln!(file, "    name: \"Patient {}\", i)?;
        writeln!(file, "    age: {}", 20 + (i % 60))?;
        writeln!(file, "    weight_kg: {:.1}", 50.0 + (i as f64 * 0.5) % 50.0)?;
        writeln!(file, "    height_m: {:.2}", 1.5 + (i as f64 * 0.01) % 0.5)?;
        writeln!(file, "    conditions: [")?;
        
        // Add some conditions
        writeln!(file, "        Condition {{\n            code: ICD10:E11.65,\n            description: \"Type 2 diabetes mellitus with hyperglycemia\"\n        }},")?;
        
        if i % 2 == 0 {
            writeln!(file, "        Condition {{\n                code: ICD10:I10,\n                description: \"Essential (primary) hypertension\"\n            }},")?;
        }
        
        if i % 3 == 0 {
            writeln!(file, "        Condition {{\n                code: ICD10:E78.5,\n                description: \"Hyperlipidemia, unspecified\"\n            }},")?;
        }
        
        writeln!(file, "    ],")?;
        
        // Add some medications
        writeln!(file, "    medications: [")?;
        
        if i % 2 == 0 {
            writeln!(file, "        Medication {{\n                code: RxNorm:197361,\n                name: \"metFORMIN 500 mg\"\n            }},")?;
        }
        
        if i % 3 == 0 {
            writeln!(file, "        Medication {{\n                code: RxNorm:197379,\n                name: \"lisinopril 10 mg\"\n            }},")?;
        }
        
        writeln!(file, "    ]")?;
        writeln!(file, "}};\n")?;
        
        // Add some calculations
        writeln!(file, "// Calculate BMI for patient_{}", i)?;
        writeln!(file, "let bmi_{} = calculate_bmi(patient_{}.weight_kg, patient_{}.height_m);", i, i, i)?;
        
        // Add some decision making
        writeln!(file, "if bmi_{} > 25.0 {{\n    recommend_lifestyle_changes(patient_{}.id);\n}}\n", i, i)?;
    }
    
    Ok(())
}
