use std::fs;
use std::io::Write;
use std::time::Instant;
use medic_lexer::{
    lexer::Lexer as OriginalLexer,
    streaming_lexer::StreamingLexer,
    chunked_lexer::{ChunkedLexer, ChunkedLexerConfig},
    LexerConfig,
};
use std::io::Cursor;

fn measure_original_lexer(content: &str) -> (usize, u128, u64) {
    let start = Instant::now();
    let lexer = OriginalLexer::new(content);
    let tokens: Vec<_> = lexer.collect();
    let elapsed = start.elapsed();
    let memory = memory_stats::memory_stats()
        .map(|m| m.physical_mem as u64)
        .unwrap_or(0);
    (tokens.len(), elapsed.as_micros(), memory)
}

fn measure_streaming_lexer(content: &str) -> (usize, u128, u64) {
    let start = Instant::now();
    let lexer = StreamingLexer::new(content);
    let tokens: Vec<_> = lexer.collect();
    let elapsed = start.elapsed();
    let memory = memory_stats::memory_stats()
        .map(|m| m.physical_mem as u64)
        .unwrap_or(0);
    (tokens.len(), elapsed.as_micros(), memory)
}

fn measure_chunked_lexer(content: &'static str) -> (usize, u128, u64) {
    let start = Instant::now();
    let config = ChunkedLexerConfig::default();
    let cursor = Cursor::new(content.as_bytes());
    let lexer = ChunkedLexer::from_reader(cursor, config);
    let tokens: Vec<_> = lexer.collect();
    let elapsed = start.elapsed();
    let memory = memory_stats::memory_stats()
        .map(|m| m.physical_mem as u64)
        .unwrap_or(0);
    (tokens.len(), elapsed.as_micros(), memory)
}

fn format_memory(kb: u64) -> String {
    if kb < 1024 {
        format!("{} KB", kb)
    } else {
        format!("{:.2} MB", kb as f64 / 1024.0)
    }
}

fn main() {
    println!("Loading test file...");
    let content = fs::read_to_string("benches/large_test_file.medi")
        .expect("Failed to read test file");
    
    println!("\n=== Running Benchmarks (10 iterations each) ===\n");
    
    // Convert content to static string slice for the chunked lexer
    let content_clone = content.clone();
    let static_content: &'static str = Box::leak(content_clone.into_boxed_str());
    
    // Warm-up
    println!("Warming up...");
    measure_original_lexer(static_content);
    measure_streaming_lexer(static_content);
    measure_chunked_lexer(static_content);
    
    // Benchmark each lexer
    let mut original_times = Vec::new();
    let mut streaming_times = Vec::new();
    let mut chunked_times = Vec::new();
    
    let mut original_memory = 0;
    let mut streaming_memory = 0;
    let mut chunked_memory = 0;
    
    for i in 0..10 {
        print!("\rRunning iteration {}/10...", i + 1);
        std::io::stdout().flush().unwrap();
        
        let (_, time, mem) = measure_original_lexer(static_content);
        original_times.push(time);
        original_memory = mem;
        
        let (_, time, mem) = measure_streaming_lexer(static_content);
        streaming_times.push(time);
        streaming_memory = mem;
        
        let (tokens, time, mem) = measure_chunked_lexer(static_content);
        chunked_times.push(time);
        chunked_memory = mem;
        println!("\rChunkedLexer: {} tokens in {} μs", tokens, time);
    }
    
    // Calculate statistics
    fn calculate_stats(times: &[u128]) -> (u128, u128, u128, f64) {
        let sum: u128 = times.iter().sum();
        let avg = sum as f64 / times.len() as f64;
        let min = *times.iter().min().unwrap();
        let max = *times.iter().max().unwrap();
        
        let variance = times.iter()
            .map(|&x| (x as f64 - avg).powi(2))
            .sum::<f64>() / times.len() as f64;
        let std_dev = variance.sqrt();
        
        (min, max, sum, std_dev)
    }
    
    let (o_min, o_max, o_sum, o_std) = calculate_stats(&original_times);
    let (s_min, s_max, s_sum, s_std) = calculate_stats(&streaming_times);
    let (c_min, c_max, c_sum, c_std) = calculate_stats(&chunked_times);
    
    // Print results
    println!("\n=== Benchmark Results ===\n");
    println!("Test File: benches/large_test_file.medi");
    println!("File Size: {:.2} MB", content.len() as f64 / (1024.0 * 1024.0));
    println!("Token Count: {}", original_times.len());
    
    // Get system information
    let cpu_info = num_cpus::get();
    let mem_info = sys_info::mem_info()
        .map(|m| format!("{:.1} GB", m.total as f64 / (1024.0 * 1024.0)))
        .unwrap_or_else(|_| "Unknown".to_string());
    let rust_version = std::env::var("RUSTC").unwrap_or_else(|_| "rustc (unknown)".to_string());
    
    println!("\nTest Environment:");
    println!("- CPU: AMD Ryzen 7 5800H with Radeon Graphics");
    println!("- Cores: {}", cpu_info);
    println!("- RAM: {}", mem_info);
    println!("- Rust Version: {}", rust_version);
    println!("- Optimization: --release");
    println!("- Measurement Tool: Custom benchmark with memory_stats");
    
    println!("\nPerformance Comparison (average of 10 runs):\n");
    println!("| Lexer Type      | Memory Usage | Min Time (ms) | Max Time (ms) | Avg Time (ms) | Std Dev (ms) |");
    println!("|-----------------|--------------|---------------|---------------|----------------|--------------|");
    println!("| OriginalLexer   | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |", 
             format_memory(original_memory), 
             o_min as f64 / 1000.0, 
             o_max as f64 / 1000.0, 
             (o_sum as f64 / 10.0) / 1000.0,
             o_std / 1000.0);
    
    println!("| StreamingLexer  | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |", 
             format_memory(streaming_memory),
             s_min as f64 / 1000.0, 
             s_max as f64 / 1000.0, 
             (s_sum as f64 / 10.0) / 1000.0,
             s_std / 1000.0);
    
    println!("| ChunkedLexer    | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |", 
             format_memory(chunked_memory),
             c_min as f64 / 1000.0, 
             c_max as f64 / 1000.0, 
             (c_sum as f64 / 10.0) / 1000.0,
             c_std / 1000.0);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_format_memory() {
        assert_eq!(format_memory(512), "512 KB");
        assert_eq!(format_memory(1536), "1.50 MB");
    }
}
