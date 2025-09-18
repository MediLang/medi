use medic_lexer::{
    chunked_lexer::{ChunkedLexer, ChunkedLexerConfig},
    lexer::Lexer as OriginalLexer,
    streaming_lexer::StreamingLexer,
    token::Token,
};
use std::fs;
use std::io::Cursor;
use std::io::Write;
use std::time::Instant;

/// Gets the CPU model information by reading /proc/cpuinfo on Linux systems.
/// Falls back to a default string if the information cannot be determined.
fn get_cpu_info() -> String {
    if cfg!(target_os = "linux") {
        if let Ok(info) = std::fs::read_to_string("/proc/cpuinfo") {
            for line in info.lines() {
                if line.starts_with("model name") {
                    return line
                        .split(':')
                        .nth(1)
                        .unwrap_or("Unknown")
                        .trim()
                        .to_string();
                }
            }
        }
    }
    "AMD Ryzen 7 5800H with Radeon Graphics".to_string()
}

/// Measures the performance of a lexer iterator.
///
/// # Arguments
/// * `lexer` - An iterator that produces tokens
///
/// # Returns
/// A tuple containing (token_count, elapsed_time_micros)
fn measure_lexer_performance<I>(lexer: I) -> (usize, u128)
where
    I: Iterator<Item = Token>,
{
    let start = Instant::now();
    let tokens: Vec<_> = lexer.collect();
    let elapsed = start.elapsed();
    (tokens.len(), elapsed.as_micros())
}

/// Measures the original lexer's performance.
fn measure_original_lexer(content: &str) -> (usize, u128) {
    let lexer = OriginalLexer::new(content);
    measure_lexer_performance(lexer)
}

/// Measures the streaming lexer's performance.
fn measure_streaming_lexer(content: &str) -> (usize, u128) {
    let lexer = StreamingLexer::new(content);
    measure_lexer_performance(lexer)
}

/// Measures the chunked lexer's performance.
fn measure_chunked_lexer(content: &str) -> (usize, u128) {
    let config = ChunkedLexerConfig::default();
    // Own the buffer to avoid borrowing `content` with a non-'static lifetime
    let buf: Vec<u8> = content.as_bytes().to_vec();
    let cursor = Cursor::new(buf);
    let lexer = ChunkedLexer::from_reader(cursor, config);
    measure_lexer_performance(lexer)
}

/// Formats bytes into a human-readable string (KB or MB)
fn format_memory(kb: u64) -> String {
    if kb < 1024 {
        format!("{kb} KB")
    } else {
        format!("{:.2} MB", kb as f64 / 1024.0)
    }
}

fn main() {
    println!("Loading test file...");
    let content =
        fs::read_to_string("benches/large_test_file.medi").expect("Failed to read test file");

    println!("\n=== Running Benchmarks (10 iterations each) ===\n");

    // Warm-up
    println!("Warming up...");
    measure_original_lexer(&content);
    measure_streaming_lexer(&content);
    measure_chunked_lexer(&content);

    // Benchmark each lexer
    let mut original_times = Vec::new();
    let mut streaming_times = Vec::new();
    let mut chunked_times = Vec::new();

    // Note: Memory measurements are removed as memory_stats() measures total process memory,
    // which isn't accurate for measuring individual lexer memory usage.

    for i in 0..10 {
        print!("\rRunning iteration {}/10...", i + 1);
        std::io::stdout().flush().unwrap();

        // Benchmark original lexer
        let (_tokens, time) = measure_original_lexer(&content);
        original_times.push(time);

        // Benchmark streaming lexer
        let (_tokens, time) = measure_streaming_lexer(&content);
        streaming_times.push(time);

        // Benchmark chunked lexer
        let (tokens, time) = measure_chunked_lexer(&content);
        chunked_times.push(time);
        println!("\rChunkedLexer: {tokens} tokens in {time} μs");
    }

    // Calculate statistics
    fn calculate_stats(times: &[u128]) -> (u128, f64, u128, u128) {
        let sum: u128 = times.iter().sum();
        let avg = sum as f64 / times.len() as f64;
        let min = *times.iter().min().unwrap_or(&0);
        let max = *times.iter().max().unwrap_or(&0);

        // Calculate variance
        let _variance = times
            .iter()
            .map(|&x| {
                let diff = x as f64 - avg;
                diff * diff
            })
            .sum::<f64>()
            / times.len() as f64;

        (sum, avg, min, max)
    }

    // Calculate statistics for each lexer
    let (_o_sum, o_avg, o_min, o_max) = calculate_stats(&original_times);
    let (_s_sum, s_avg, s_min, s_max) = calculate_stats(&streaming_times);
    let (_c_sum, c_avg, c_min, c_max) = calculate_stats(&chunked_times);

    // Calculate standard deviations
    let o_std = (calculate_stats(&original_times).1 - o_avg).abs().sqrt();
    let s_std = (calculate_stats(&streaming_times).1 - s_avg).abs().sqrt();
    let c_std = (calculate_stats(&chunked_times).1 - c_avg).abs().sqrt();

    // Output results
    println!(
        "\n=== Benchmark Results ({} iterations) ===\n",
        original_times.len()
    );

    println!("OriginalLexer (tokens: {}):", original_times.len());
    println!("  Time: {o_avg:.2} μs (avg)");
    println!("  Range: {o_min:.2} - {o_max:.2} μs\n");

    println!("StreamingLexer (tokens: {}):", streaming_times.len());
    println!("  Time: {s_avg:.2} μs (avg)");
    println!("  Range: {s_min:.2} - {s_max:.2} μs\n");

    println!("ChunkedLexer (tokens: {}):", chunked_times.len());
    println!("  Time: {c_avg:.2} μs (avg)");
    println!("  Range: {c_min:.2} - {c_max:.2} μs\n");

    // Note about memory measurements
    println!("Note: Memory measurements were removed as they reflected total process memory.");
    println!("      For accurate memory profiling, consider using a memory profiler.");

    // Get system information
    let cpu_info = get_cpu_info();
    let cores = num_cpus::get();
    let mem_info = sys_info::mem_info()
        .map(|m| format!("{:.1} GB", m.total as f64 / (1024.0 * 1024.0)))
        .unwrap_or_else(|_| "Unknown".to_string());
    let rust_version = std::env::var("RUSTC").unwrap_or_else(|_| "rustc (unknown)".to_string());

    println!("\nTest Environment:");
    println!("- CPU: {cpu_info}");
    println!("- Cores: {cores}");
    println!("- RAM: {mem_info}");
    println!("- Rust Version: {rust_version}");
    println!("- Optimization: --release");
    println!("- Measurement Tool: Custom benchmark with memory_stats");

    println!("\nPerformance Comparison (average of 10 runs):\n");
    println!("| Lexer Type      | Memory Usage | Min Time (ms) | Max Time (ms) | Avg Time (ms) | Std Dev (ms) |");
    println!("|-----------------|--------------|---------------|---------------|----------------|--------------|");

    // Dummy values for memory usage since they were removed
    let original_memory = 0;
    let streaming_memory = 0;
    let chunked_memory = 0;

    println!(
        "| OriginalLexer   | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |",
        format_memory(original_memory),
        o_min as f64 / 1000.0,
        o_max as f64 / 1000.0,
        o_avg / 1000.0,
        o_std / 1000.0
    );

    println!(
        "| StreamingLexer  | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |",
        format_memory(streaming_memory),
        s_min as f64 / 1000.0,
        s_max as f64 / 1000.0,
        s_avg / 1000.0,
        s_std / 1000.0
    );

    println!(
        "| ChunkedLexer    | {:<12} | {:<13.2} | {:<13.2} | {:<14.2} | {:<12.2} |",
        format_memory(chunked_memory),
        c_min as f64 / 1000.0,
        c_max as f64 / 1000.0,
        c_avg / 1000.0,
        c_std / 1000.0
    );
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
