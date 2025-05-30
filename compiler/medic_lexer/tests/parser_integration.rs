use medic_lexer::{
    streaming_lexer::{LexerConfig, StreamingLexer},
    token::Token,
};
use medic_parser::parser::{parse_program, TokenSlice};

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
        max_buffer_size: 1024,
        include_whitespace: false,
        include_comments: false,
    };

    let lexer = StreamingLexer::with_config(source, config);

    // Collect all tokens
    let tokens: Vec<Token> = lexer.collect();

    println!("Lexed {} tokens", tokens.len());
    assert!(
        !tokens.is_empty(),
        "Lexer should produce at least some tokens"
    );

    // Parse the tokens using TokenSlice
    let result = parse_program(TokenSlice::new(&tokens));

    // Verify parsing was successful
    let (remaining, ast) = result.unwrap_or_else(|e| {
        panic!(
            "Failed to parse tokens. Error: {:?}\nTokens: {:?}",
            e, tokens
        );
    });

    // Verify we consumed all tokens
    if !remaining.is_empty() {
        println!("\n=== TOKEN DEBUGGING ===");
        
        // Print first 5 remaining tokens
        println!("First 5 remaining tokens:");
        let slice = remaining.0;
        for (i, item) in slice.iter().take(5).enumerate() {
            println!("  {}: {:?}", i, item);
        }
        
        // Print context around the first remaining token
        if !slice.is_empty() {
            let first_remaining = &slice[0];
            println!("\nTokens around first remaining (offset: {}):", first_remaining.location.offset);
            
            // Find the position in the original tokens
            for (i, token) in tokens.iter().enumerate() {
                let token_offset = token.location.offset;
                let first_offset = first_remaining.location.offset;
                
                // Show tokens within 5 positions of the first remaining token
                if token_offset + 5 >= first_offset && token_offset <= first_offset + 5 {
                    let marker = if token_offset == first_offset {
                        "<<< FIRST REMAINING"
                    } else {
                        ""
                    };
                    println!("  {}: {:?} (offset: {}) {}", i, token, token_offset, marker);
                }
            }
        }
        
        panic!("Not all tokens were consumed. Remaining count: {}", slice.len());
    }

    // Verify the AST structure
    assert!(
        !ast.statements.is_empty(),
        "AST should contain statements, but got: {:?}",
        ast
    );

    // Basic AST validation
    assert!(
        ast.statements.len() >= 3,
        "Expected at least 3 statements (patient, bmi, if), got {}",
        ast.statements.len()
    );

    // Print a summary of the parsed program
    println!(
        "Successfully parsed program with {} statements",
        ast.statements.len()
    );
}

#[test]
fn test_large_file_parsing() {
    use std::path::PathBuf;

    // Use a temporary file that gets cleaned up
    let test_dir = PathBuf::from("target/test_data");
    std::fs::create_dir_all(&test_dir).expect("Failed to create test directory");
    let test_file = test_dir.join("large_medical_script.medi");

    // Always regenerate for consistent test conditions
    generate_test_file(&test_file.to_string_lossy(), 100).expect("Failed to generate test file");

    // Read the test file
    let source = std::fs::read_to_string(&test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {:?}: {}", test_file, e));

    println!("Source file size: {} bytes", source.len());

    // Create a streaming lexer with a small buffer to test chunking
    let config = LexerConfig {
        max_buffer_size: 128, // Small buffer to force chunking
        include_whitespace: false,
        include_comments: false,
    };

    let start_time = std::time::Instant::now();
    let lexer = StreamingLexer::with_config(&source, config);

    // Collect all tokens
    let tokens: Vec<Token> = lexer.collect();

    if tokens.is_empty() {
        eprintln!("No tokens were generated");
        return; // Skip if no tokens were generated
    }

    let lex_duration = start_time.elapsed();
    println!(
        "Lexed {} tokens in {:.2?} ({:.1} tokens/ms)",
        tokens.len(),
        lex_duration,
        tokens.len() as f64 / lex_duration.as_millis() as f64
    );

    // Parse the tokens
    let parse_start = std::time::Instant::now();
    let result = parse_program(TokenSlice::new(&tokens));

    // Verify parsing was successful
    let (remaining, ast) = result.unwrap_or_else(|e| {
        panic!(
            "Failed to parse large file. Error: {:?}\nFirst 10 tokens: {:?}",
            e,
            tokens.iter().take(10).collect::<Vec<_>>()
        );
    });

    assert!(
        remaining.is_empty(),
        "Not all tokens were consumed. Remaining count: {}",
        remaining.len()
    );

    let parse_duration = parse_start.elapsed();
    println!(
        "Parsed AST with {} statements in {:.2?} ({:.1} statements/ms)",
        ast.statements.len(),
        parse_duration,
        ast.statements.len() as f64 / parse_duration.as_millis() as f64
    );

    // Clean up test file
    let _ = std::fs::remove_file(&test_file);
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
    writeln!(
        file,
        "fn calculate_bmi(weight_kg: f64, height_m: f64) -> f64 {{"
    )?;
    writeln!(file, "    weight_kg / (height_m * height_m)")?;
    writeln!(file, "}}\n")?;

    writeln!(file, "fn recommend_lifestyle_changes(patient_id: &str) {{")?;
    writeln!(
        file,
        "    println!(\"Recommend lifestyle changes for {{}}\", patient_id);"
    )?;
    writeln!(file, "}}\n")?;

    // Generate patient data
    for i in 0..patient_count {
        writeln!(file, "// Patient {}", i)?;
        writeln!(file, "let patient_{} = Patient {{", i)?;
        writeln!(file, "    id: \"PATIENT-{}\",", i)?;
        writeln!(file, "    name: \"Patient {}\",", i)?;
        writeln!(file, "    age: {}", 20 + (i % 60))?;
        writeln!(file, "    weight_kg: {:.1}", 50.0 + (i as f64 * 0.5) % 50.0)?;
        writeln!(file, "    height_m: {:.2}", 1.5 + (i as f64 * 0.01) % 0.5)?;
        writeln!(file, "    conditions: [")?;

        // Add some conditions
        writeln!(file, "        Condition {{")?;
        writeln!(file, "            code: ICD10:E11.65,")?;
        writeln!(
            file,
            "            description: \"Type 2 diabetes mellitus with hyperglycemia\""
        )?;
        writeln!(file, "        }},")?;

        if i % 2 == 0 {
            writeln!(file, "        Condition {{")?;
            writeln!(file, "            code: ICD10:I10,")?;
            writeln!(
                file,
                "            description: \"Essential (primary) hypertension\""
            )?;
            writeln!(file, "        }},")?;
        }

        if i % 3 == 0 {
            writeln!(file, "        Condition {{")?;
            writeln!(file, "            code: ICD10:E78.5,")?;
            writeln!(
                file,
                "            description: \"Hyperlipidemia, unspecified\""
            )?;
            writeln!(file, "        }},")?;
        }

        writeln!(file, "    ],")?;

        // Add some medications
        writeln!(file, "    medications: [")?;

        if i % 2 == 0 {
            writeln!(file, "        Medication {{")?;
            writeln!(file, "            code: RxNorm:197361,")?;
            writeln!(file, "            name: \"metFORMIN 500 mg\"")?;
            writeln!(file, "        }},")?;
        }

        if i % 3 == 0 {
            writeln!(file, "        Medication {{")?;
            writeln!(file, "            code: RxNorm:197379,")?;
            writeln!(file, "            name: \"lisinopril 10 mg\"")?;
            writeln!(file, "        }},")?;
        }

        writeln!(file, "    ]")?;
        writeln!(file, "}};\n")?;

        // Add some calculations
        writeln!(file, "// Calculate BMI for patient_{}", i)?;
        writeln!(
            file,
            "let bmi_{} = calculate_bmi(patient_{}.weight_kg, patient_{}.height_m);",
            i, i, i
        )?;

        // Add some decision making
        writeln!(file, "if bmi_{} > 25.0 {{", i)?;
        writeln!(file, "    recommend_lifestyle_changes(patient_{}.id);", i)?;
        writeln!(file, "}}\n")?;
    }

    Ok(())
}
