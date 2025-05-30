use std::time::Instant;
use std::error::Error;
use std::fmt;
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::StreamingLexer,
    chunked_lexer::{ChunkedLexer, ChunkedLexerConfig},
    LexerConfig,
    token::Token,
};

#[derive(Debug)]
struct BenchmarkError {
    message: String,
    iteration: usize,
}

impl fmt::Display for BenchmarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Iteration {}: {}", self.iteration, self.message)
    }
}

impl Error for BenchmarkError {}

#[derive(Debug, Default)]
struct BenchmarkStats {
    iterations: usize,
    total_tokens: usize,
    total_errors: usize,
    min_tokens: usize,
    max_tokens: usize,
    total_time_micros: u128,
    min_time_micros: u128,
    max_time_micros: u128,
}

impl BenchmarkStats {
    fn new() -> Self {
        Self {
            iterations: 0,
            total_tokens: 0,
            total_errors: 0,
            min_tokens: usize::MAX,
            max_tokens: 0,
            total_time_micros: 0,
            min_time_micros: u128::MAX,
            max_time_micros: 0,
        }
    }

    fn add_iteration(&mut self, tokens: &[Token], time_micros: u128, errors: usize) {
        self.iterations += 1;
        self.total_tokens += tokens.len();
        self.total_errors += errors;
        self.total_time_micros += time_micros;
        
        self.min_tokens = self.min_tokens.min(tokens.len());
        self.max_tokens = self.max_tokens.max(tokens.len());
        self.min_time_micros = self.min_time_micros.min(time_micros);
        self.max_time_micros = self.max_time_micros.max(time_micros);
    }

    fn avg_tokens(&self) -> f64 {
        if self.iterations == 0 { 0.0 } else { self.total_tokens as f64 / self.iterations as f64 }
    }

    fn avg_time_micros(&self) -> f64 {
        if self.iterations == 0 { 0.0 } else { self.total_time_micros as f64 / self.iterations as f64 }
    }

    fn error_rate(&self) -> f64 {
        if self.total_tokens == 0 { 0.0 } else { self.total_errors as f64 / self.total_tokens as f64 * 100.0 }
    }
}

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

fn run_benchmark() -> Result<BenchmarkStats, Box<dyn Error>> {
    let source = TEST_CONTENT.repeat(100);
    let mut stats = BenchmarkStats::new();
    let iterations = 10;

    for i in 0..iterations {
        let start = Instant::now();
        
        // Create lexer and collect tokens, counting any errors
        let lexer = OriginalLexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();
        
        // Check for lexer errors in the tokens
        let error_count = tokens.iter()
            .filter(|t| matches!(t.token_type, medic_lexer::token::TokenType::Error(_)))
            .count();
        
        let elapsed = start.elapsed().as_micros();
        
        // Log iteration details
        println!("Iteration {}: Lexed {} tokens ({} errors) in {} μs", 
                 i + 1, tokens.len(), error_count, elapsed);
        
        // Log any errors if they occurred
        if error_count > 0 {
            eprintln!("  Warning: Found {} tokenization errors in iteration {}", error_count, i + 1);
            
            // Print the first few errors for debugging
            let errors: Vec<_> = tokens.iter()
                .filter(|t| matches!(t.token_type, medic_lexer::token::TokenType::Error(_)))
                .take(3) // Limit to first 3 errors to avoid flooding output
                .collect();
                
            for token in errors {
                eprintln!("    Error token: {:?} at line {}", 
                         token.token_type, token.location.line);
            }
            
            if error_count > 3 {
                eprintln!("    ... and {} more errors", error_count - 3);
            }
        }
        
        // Update statistics
        stats.add_iteration(&tokens, elapsed, error_count);
    }
    
    Ok(stats)
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("Running lexer benchmark...");
    println!("Test content length: {} bytes", TEST_CONTENT.len() * 100);
    
    match run_benchmark() {
        Ok(stats) => {
            println!("\n=== Benchmark Results ===");
            println!("Iterations:         {}", stats.iterations);
            println!("Total tokens:       {}", stats.total_tokens);
            println!("Total errors:       {}", stats.total_errors);
            println!("Error rate:         {:.4}%", stats.error_rate());
            println!("\nTokens per iteration:");
            println!("  Min:     {}", stats.min_tokens);
            println!("  Max:     {}", stats.max_tokens);
            println!("  Average: {:.1}", stats.avg_tokens());
            println!("\nTime per iteration (μs):");
            println!("  Min:     {}", stats.min_time_micros);
            println!("  Max:     {}", stats.max_time_micros);
            println!("  Average: {:.1}", stats.avg_time_micros());
            println!("\nTokens per second (thousands): {:.1}", 
                   stats.total_tokens as f64 / (stats.total_time_micros as f64 / 1_000_000.0) / 1000.0);
            
            if stats.total_errors > 0 {
                eprintln!("\nWarning: {} tokenization errors were detected in the benchmark", stats.total_errors);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Benchmark failed: {}", e);
            return Err(e);
        }
    }
    
    Ok(())
}
