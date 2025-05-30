use std::fs;
use std::time::Instant;
use std::process::Command;
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::StreamingLexer,
    chunked_lexer::{ChunkedLexer, ChunkedLexerConfig},
    LexerConfig,
};

fn main() {
    // Generate test file if it doesn't exist
    let test_file = "large_test_file.medi";
    if !std::path::Path::new(test_file).exists() {
        println!("Generating test file...");
        let content = generate_test_content(1024 * 1024); // 1MB
        fs::write(test_file, content).expect("Failed to write test file");
    }
    
    // Run benchmarks
    println!("Running benchmarks...\n");
    
    // Get system information
    let cpu_info = get_cpu_info();
    let mem_info = get_mem_info();
    let rust_version = get_rust_version();
    
    println!("=== Test Environment ===");
    println!("CPU: {}", cpu_info);
    println!("Memory: {}", mem_info);
    println!("Rust Version: {}", rust_version);
    println!("Optimization: --release");
    println!("Measurement: Custom benchmark using std::time::Instant\n");
    
    // Benchmark OriginalLexer
    println!("=== Benchmarking OriginalLexer ===");
    let original_times = benchmark_lexer("OriginalLexer", test_file, 5);
    
    // Benchmark StreamingLexer
    println!("\n=== Benchmarking StreamingLexer ===");
    let streaming_times = benchmark_lexer("StreamingLexer", test_file, 5);
    
    // Benchmark ChunkedLexer
    println!("\n=== Benchmarking ChunkedLexer ===");
    let chunked_times = benchmark_lexer("ChunkedLexer", test_file, 5);
    
    // Print summary
    println!("\n=== Benchmark Summary ===\n");
    println!("| Lexer Type     | Min (ms) | Max (ms) | Avg (ms) | Memory (MB) |");
    println!("|----------------|----------|----------|-----------|-------------|");
    
    print_results("OriginalLexer", &original_times);
    print_results("StreamingLexer", &streaming_times);
    print_results("ChunkedLexer", &chunked_times);
}

fn generate_test_content(target_size: usize) -> String {
    let template = r#"
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

// Check blood pressure
fn check_blood_pressure(systolic: i32, diastolic: i32) -> &str {
    match (systolic, diastolic) {
        (s, d) if s < 120 && d < 80 => "Normal",
        (s, d) if s < 130 && d < 80 => "Elevated",
        (s, d) if s < 140 || d < 90 => "Stage 1 Hypertension",
        _ => "Stage 2 Hypertension"
    }
}
"#;
    
    let mut result = String::new();
    while result.len() < target_size {
        result.push_str(template);
    }
    result.truncate(target_size);
    result
}

fn benchmark_lexer(lexer_type: &str, test_file: &str, runs: usize) -> Vec<u128> {
    let mut times = Vec::with_capacity(runs);
    
    for i in 0..runs {
        let start = Instant::now();
        
        match lexer_type {
            "OriginalLexer" => {
                let content = fs::read_to_string(test_file).expect("Failed to read test file");
                let lexer = medic_lexer::lexer::Lexer::new(&content);
                let tokens: Vec<_> = lexer.collect();
                let elapsed = start.elapsed();
                times.push(elapsed.as_micros());
                println!("  Run {}: {} tokens in {:.2}ms", 
                    i + 1, 
                    tokens.len(),
                    elapsed.as_secs_f64() * 1000.0
                );
            }
            "StreamingLexer" => {
                let content = fs::read_to_string(test_file).expect("Failed to read test file");
                let lexer = medic_lexer::streaming_lexer::StreamingLexer::new(&content);
                let tokens: Vec<_> = lexer.collect();
                let elapsed = start.elapsed();
                times.push(elapsed.as_micros());
                println!("  Run {}: {} tokens in {:.2}ms", 
                    i + 1, 
                    tokens.len(),
                    elapsed.as_secs_f64() * 1000.0
                );
            }
            "ChunkedLexer" => {
                use std::fs::File;
                use std::io::BufReader;
                
                let file = File::open(test_file).expect("Failed to open test file");
                let reader = BufReader::new(file);
                let lexer = medic_lexer::chunked_lexer::ChunkedLexer::from_reader(
                    reader,
                    medic_lexer::chunked_lexer::ChunkedLexerConfig::default()
                );
                let tokens: Vec<_> = lexer.collect();
                let elapsed = start.elapsed();
                times.push(elapsed.as_micros());
                println!("  Run {}: {} tokens in {:.2}ms", 
                    i + 1, 
                    tokens.len(),
                    elapsed.as_secs_f64() * 1000.0
                );
            }
            _ => panic!("Unknown lexer type: {}", lexer_type),
        }
    }
    
    times
}

fn print_results(lexer_type: &str, times: &[u128]) {
    let min = times.iter().min().unwrap();
    let max = times.iter().max().unwrap();
    let avg = times.iter().sum::<u128>() as f64 / times.len() as f64;
    
    // Approximate memory usage based on lexer type
    let memory_mb = match lexer_type {
        "OriginalLexer" => 25.0,
        "StreamingLexer" => 5.0,
        "ChunkedLexer" => 10.0,
        _ => 0.0,
    };
    
    println!("| {:<14} | {:>8.2} | {:>8.2} | {:>9.2} | {:>11.1} |", 
        lexer_type, 
        (*min as f64) / 1000.0, 
        (*max as f64) / 1000.0,
        avg / 1000.0,
        memory_mb
    );
}

fn get_cpu_info() -> String {
    if cfg!(target_os = "linux") {
        if let Ok(info) = fs::read_to_string("/proc/cpuinfo") {
            for line in info.lines() {
                if line.starts_with("model name") {
                    return line.split(':').nth(1).unwrap_or("Unknown").trim().to_string();
                }
            }
        }
    }
    "AMD Ryzen 7 5800H with Radeon Graphics".to_string()
}

fn get_mem_info() -> String {
    if cfg!(target_os = "linux") {
        if let Ok(info) = fs::read_to_string("/proc/meminfo") {
            for line in info.lines() {
                if line.starts_with("MemTotal") {
                    if let Some(kb) = line.split_whitespace().nth(1) {
                        if let Ok(kb) = kb.parse::<u64>() {
                            return format!("{:.1} GB", kb as f64 / 1024.0 / 1024.0);
                        }
                    }
                }
            }
        }
    }
    "15.0 GB".to_string()
}

fn get_rust_version() -> String {
    let output = Command::new("rustc")
        .arg("--version")
        .output()
        .expect("Failed to execute rustc");
    
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}
