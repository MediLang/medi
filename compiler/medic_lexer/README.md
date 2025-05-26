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

The streaming lexer processes tokens in chunks, making it ideal for large files:

```rust
use medic_lexer::streaming_lexer::StreamingLexer;
use std::fs::File;
use std::io::Read;

fn process_large_file(path: &str) -> std::io::Result<()> {
    let mut file = File::open(path)?;
    let mut buffer = [0; 8192]; // 8KB buffer
    let mut lexer = StreamingLexer::new("");
    
    loop {
        let bytes_read = file.read(&mut buffer)?;
        if bytes_read == 0 {
            break; // End of file
        }
        
        let chunk = std::str::from_utf8(&buffer[..bytes_read])
            .expect("Invalid UTF-8 in source file");
            
        // Process the chunk and collect tokens
        for token in lexer.process_chunk(chunk) {
            // Process each token
            println!("Token: {:?}", token);
        }
    }
    
    // Process any remaining tokens in the buffer
    for token in lexer.finish() {
        println!("Final token: {:?}", token);
    }
    
    Ok(())
}
```

## Performance

The streaming lexer is designed to be memory-efficient. Here's a comparison with the original lexer:

| Metric | Original Lexer | Streaming Lexer |
|--------|----------------|-----------------|
| Memory Usage | High (stores all tokens) | Low (buffers tokens in chunks) |
| Processing Speed | Fast (single pass) | Slightly slower (due to chunking) |
| Best For | Small to medium files | Large files or memory-constrained environments |

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
