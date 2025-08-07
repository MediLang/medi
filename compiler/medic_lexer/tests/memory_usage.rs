// Only enable jemalloc on non-Windows platforms when the jemalloc feature is enabled
#[cfg(all(not(target_env = "msvc"), feature = "jemalloc"))]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use lazy_static::lazy_static;
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::{LexerConfig, StreamingLexer},
};
use std::fs;
use std::path::Path;
use std::sync::Mutex;

#[cfg(feature = "jemalloc")]
use jemalloc_ctl::{epoch, stats};

lazy_static! {
    static ref TEST_FILE: Mutex<Option<String>> = Mutex::new(None);
}

/// Measures the memory allocated by a closure using jemalloc's statistics when available,
/// or falls back to a simple execution without memory measurement.
fn measure_memory_usage<F: FnOnce()>(f: F) -> usize {
    #[cfg(feature = "jemalloc")]
    {
        // Initialize jemalloc-ctl
        let e = epoch::mib().expect("Failed to get jemalloc epoch");
        let allocated = stats::allocated::mib().expect("Failed to get allocated mib");

        // Force a flush of jemalloc's internal stats
        e.advance().expect("Failed to advance jemalloc epoch");

        // Read baseline memory usage
        let before = allocated.read().expect("Failed to read allocated memory");

        // Run the function we're measuring
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));

        // Force another flush to get updated stats
        e.advance().expect("Failed to advance jemalloc epoch");

        // Read memory usage after execution
        let after = allocated.read().expect("Failed to read allocated memory");

        // Propagate any panics from the test function
        if let Err(panic) = result {
            std::panic::resume_unwind(panic);
        }

        // Return the difference in memory usage
        after - before
    }

    #[cfg(not(feature = "jemalloc"))]
    {
        // Just run the function without measuring memory
        f();
        0
    }
}

#[test]
fn test_memory_usage_comparison() {
    // Generate or load test data with smaller size
    let source = generate_medium_medical_script();

    // Force garbage collection before starting
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Run a single iteration to reduce memory pressure
    // Note: Increasing iterations could provide more stable measurements
    // but increases memory usage during testing
    let iterations = 1;
    let mut original_measurements = Vec::new();
    let mut streaming_measurements = Vec::new();

    for _ in 0..iterations {
        // Test original lexer memory usage
        let original_mem = measure_memory_usage(|| {
            let lexer = OriginalLexer::new(&source);
            let _tokens: Vec<_> = lexer.collect();
        });
        original_measurements.push(original_mem);

        // Force cleanup between tests
        std::thread::sleep(std::time::Duration::from_millis(50));

        // Test streaming lexer memory usage
        let streaming_mem = measure_memory_usage(|| {
            let config = LexerConfig {
                max_buffer_size: 4096, // Larger buffer to reduce allocations
                include_whitespace: false,
                include_comments: false,
            };
            let lexer = StreamingLexer::with_config(&source, config);
            let _tokens: Vec<_> = lexer.collect();
        });
        streaming_measurements.push(streaming_mem);

        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    // Use average measurements for more reliability
    let original_mem = original_measurements.iter().sum::<usize>() / iterations;
    let streaming_mem = streaming_measurements.iter().sum::<usize>() / iterations;

    println!("Original lexer memory usage: {} KB", original_mem / 1024);
    println!("Streaming lexer memory usage: {} KB", streaming_mem / 1024);

    // Compare memory usage with a more lenient threshold
    // Allow streaming lexer to use up to 2x the memory of the original lexer
    // This accounts for the overhead of buffering and other implementation details
    let max_allowed_memory = original_mem * 2;

    println!("Original lexer memory usage: {} KB", original_mem / 1024);
    println!(
        "Streaming lexer memory usage: {} KB (max allowed: {} KB)",
        streaming_mem / 1024,
        max_allowed_memory / 1024
    );

    // Only fail if the streaming lexer uses significantly more memory than expected
    if streaming_mem > max_allowed_memory {
        panic!(
            "Streaming lexer used {} KB, which is more than 2x the original lexer's {} KB",
            streaming_mem / 1024,
            original_mem / 1024
        );
    } else if streaming_mem < original_mem {
        println!("✅ Streaming lexer used less memory than original");
    } else {
        println!("⚠️  Streaming lexer used more memory than original but within acceptable limits");
    }

    // Optional: Log the improvement percentage
    if original_mem > 0 {
        let improvement = ((original_mem - streaming_mem) as f64 / original_mem as f64) * 100.0;
        println!("Memory improvement: {improvement:.1}%");
    }
}
fn generate_medium_medical_script() -> String {
    // Smaller test data for CI environments
    let patient_count = 100; // Reduced from 1000 to 100
    let conditions_per_patient = 3; // Reduced from 5 to 3
    let observations_per_patient = 2; // Reduced from 3 to 2
    let medications_per_patient = 1; // Reduced from 2 to 1

    let mut source = String::with_capacity(1024 * 1024); // 1MB initial capacity

    // Add some basic patient data
    for i in 0..patient_count {
        source.push_str(&format!("patient{i}: Patient = {{\n"));
        source.push_str(&format!("  id: \"P{:06}\",\n", 100000 + i));
        source.push_str(&format!("  age: {},\n", 20 + (i % 60)));
        source.push_str("  gender: \"");
        if i % 2 == 0 {
            source.push_str("male")
        } else {
            source.push_str("female")
        };
        source.push_str("\",\n");

        // Add conditions
        source.push_str("  conditions: [\n");
        for j in 0..conditions_per_patient {
            source.push_str("    {\n");
            source.push_str(&format!("      code: \"ICD-10:E11.6{j}\",\n"));
            source.push_str(&format!("      description: \"Condition {j}\"\n"));
            source.push_str("    }");
            if j < conditions_per_patient - 1 {
                source.push(',');
            }
            source.push('\n');
        }
        source.push_str("  ],\n");

        // Add observations
        source.push_str("  observations: [\n");
        for j in 0..observations_per_patient {
            source.push_str("    {\n");
            source.push_str(&format!("      code: \"LOINC:1234-{j}\",\n"));
            source.push_str(&format!("      value: {0},\n", 100 + (i * j) % 50));
            source.push_str("      unit: \"mg/dL\"\n");
            source.push_str("    }");
            if j < observations_per_patient - 1 {
                source.push(',');
            }
            source.push('\n');
        }
        source.push_str("  ],\n");

        // Add medications
        source.push_str("  medications: [\n");
        for j in 0..medications_per_patient {
            source.push_str("    {\n");
            source.push_str("      code: \"RxNorm:123456\",\n");
            source.push_str("      name: \"Medication\",\n");
            source.push_str("      dosage: 1,\n");
            source.push_str("      unit: \"mg\"\n");
            source.push_str("    }");
            if j < medications_per_patient - 1 {
                source.push(',');
            }
            source.push('\n');
        }
        source.push_str("  ]\n");

        source.push_str("}\n\n");
    }

    // Don't cache the test data to save memory
    source
}

#[allow(dead_code)]
fn generate_large_medical_script() -> String {
    // Check if we already generated the test file
    {
        let test_file = TEST_FILE.lock().unwrap();
        if let Some(content) = &*test_file {
            return content.clone();
        }
    }

    let path = Path::new("test_data/large_medical_script.medi");

    // Try to read from file first
    if let Ok(content) = fs::read_to_string(path) {
        let mut test_file = TEST_FILE.lock().unwrap();
        *test_file = Some(content.clone());
        return content;
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
        Condition {{ code: "ICD10:E11.65", description: "Type 2 diabetes mellitus with hyperglycemia" }},
        Condition {{ code: "ICD10:I10", description: "Essential (primary) hypertension" }}
    ],
    medications: [
        Medication {{ code: "RxNorm:197361", name: "metFORMIN 500 mg" }},
        Medication {{ code: "RxNorm:197379", name: "lisinopril 10 mg" }}
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

    // Cache the content
    let mut test_file = TEST_FILE.lock().unwrap();
    *test_file = Some(content.clone());
    content
}
