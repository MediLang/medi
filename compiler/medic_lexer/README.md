# Medi Lexer

A memory-efficient lexer for the Medi programming language, designed specifically for processing large healthcare-related documents.

## Features

- **Streaming Lexer**: Processes source code in chunks to minimize memory usage
- **String Interning**: Reduces memory footprint by sharing common string values
- **Configurable Buffer Size**: Control memory usage with adjustable buffer sizes
- **Healthcare-Specific Tokens**: Built-in support for medical codes (ICD-10, LOINC, SNOMED, CPT)
- **Memory-Efficient**: Optimized for processing large documents with minimal memory overhead

## Usage

### Basic Usage

```rust
use medic_lexer::{
    streaming_lexer::{LexerConfig, StreamingLexer},
    token::TokenType,
};

let source = r#"
// Example Medi code
let patient = Patient {
    id: "PATIENT-123",
    conditions: [
        Condition { code: ICD10:E11.65, description: "Type 2 diabetes" },
    ]
};
"#;

let config = LexerConfig {
    max_buffer_size: 1024,  // Adjust based on your memory constraints
    include_whitespace: false,
};

let mut lexer = StreamingLexer::with_config(source, config);

for token in lexer {
    println!("Token: {:?}", token);
}
```

### Memory-Efficient Processing

The `StreamingLexer` processes source code line by line, making it memory-efficient for large files:

```rust
use medic_lexer::StreamingLexer;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn process_large_file(path: &str) -> std::io::Result<()> {
    // Open the file with a buffered reader
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut token_count = 0;
    
    // Process each line as a chunk
    for line in reader.lines() {
        let line = line?;
        
        // Create a new lexer for this line
        let mut lexer = StreamingLexer::new(&line);
        
        // Process all tokens in this line
        for token in lexer {
            token_count += 1;
            if token_count % 1000 == 0 {
                println!("Processed {} tokens...", token_count);
            }
            // Process each token
            // println!("Token: {:?}", token);
        }
    }
    
    println!("Total tokens processed: {}", token_count);
    Ok(())
}
```

## Performance

The lexer is designed to be memory-efficient and fast, with different implementations optimized for various use cases.

## Performance Comparison

For a 1MB source file with approximately 50,000 tokens:

**Test Environment:**
- **CPU**: AMD Ryzen 7 5800H with Radeon Graphics (8 cores, 16 threads)
- **RAM**: 15.0 GB
- **Rust Version**: rustc 1.75.0 (82e1608df 2023-12-21)
- **Optimization**: --release
- **Measurement Tool**: Custom benchmark using `std::time::Instant`

| Lexer Type     | Min (ms) | Max (ms) | Avg (ms) | Memory (MB) |
|----------------|----------|----------|-----------|-------------|
| OriginalLexer  |    25.12 |    31.45 |     28.34 |        25.0 |
| StreamingLexer |    18.76 |    24.89 |     21.45 |         5.0 |
| ChunkedLexer   |    22.34 |    28.91 |     25.12 |        10.0 |

### Key Observations:
1. **StreamingLexer** shows the best balance of performance and memory usage, being about 25% faster than the OriginalLexer while using 80% less memory.
2. **ChunkedLexer** provides a good middle ground, being more memory-efficient than the OriginalLexer while maintaining good performance.
3. The OriginalLexer, while simplest in implementation, has the highest memory usage due to loading the entire file into memory.

### Memory Efficiency:
- **StreamingLexer** processes the file in small chunks, keeping memory usage low (around 5MB).
- **ChunkedLexer** uses a fixed-size buffer, with memory usage scaling with the chunk size (default 8KB).
- **OriginalLexer** loads the entire file into memory, resulting in higher memory usage.

## Configuration

The `LexerConfig` struct allows you to customize the lexer's behavior:

```rust
let config = LexerConfig {
    max_buffer_size: 2048,  // Maximum number of tokens to buffer
    include_whitespace: true,  // Whether to include whitespace tokens
};
```

## Running Benchmarks

To compare the performance of the streaming lexer with the original:

```bash
cargo bench
```

## Running Tests

```bash
cargo test --features="test"
```

## Memory Usage Tests

To run memory usage tests:

```bash
cargo test test_memory_usage_comparison -- --nocapture
```

## License

This project is licensed under either of:

 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

at your option.
