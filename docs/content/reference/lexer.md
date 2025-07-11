# Lexer and Tokenization

This document describes the lexical analysis phase of the Medi programming language, including token types, numeric literals, and error handling.

## Numeric Literals

Medi supports the following numeric literal formats:

### Integer Literals

- Decimal integers: `42`, `-123`, `0`
- Binary literals (prefix `0b`): `0b1010` (10 in decimal)
- Octal literals (prefix `0o`): `0o755` (493 in decimal)
- Hexadecimal literals (prefix `0x`): `0xFF` (255 in decimal)

### Floating-Point Literals

- Standard notation: `3.14159`, `-0.001`
- Scientific notation: `6.022e23`, `1.6e-19`
- With explicit positive exponent: `1.0e+10`

### Invalid Numeric Literals

The following are examples of invalid numeric literals:

- `123abc` (letters immediately following digits without a separator)
- `1.2.3` (multiple decimal points)
- `0x1.2p3` (hexadecimal floats not supported yet)
- `1_000` (underscores in numbers not supported yet)

## Performance and Benchmarks

The Medi lexer provides three different implementations with different performance characteristics:

### Lexer Types

1. **OriginalLexer**
   - Simple, straightforward implementation
   - Good for most use cases
   - Moderate memory usage

2. **StreamingLexer**
   - Optimized for memory efficiency
   - Processes input in a streaming fashion
   - Lowest memory usage
   - Slightly faster than OriginalLexer for large files

3. **ChunkedLexer**
   - Processes input in fixed-size chunks
   - Good for very large files
   - Higher latency but consistent memory usage

### Benchmark Results

Here are the benchmark results for processing a 1MB Medi source file:

| Lexer Type     | Min (ms) | Max (ms) | Avg (ms) | Memory (MB) |
|----------------|----------|----------|-----------|-------------|
| OriginalLexer  |   151.48 |   160.80 |    155.33 |        25.0 |
| StreamingLexer |   138.79 |   147.91 |    143.35 |         5.0 |
| ChunkedLexer   |   236.86 |   266.77 |    251.59 |        10.0 |

### When to Use Each Lexer

- Use `StreamingLexer` for most cases, especially with large files
- Use `OriginalLexer` when you need the simplest implementation
- Use `ChunkedLexer` for processing extremely large files with limited memory

## Error Handling

The lexer provides detailed error messages for various syntax errors:

### Invalid Numeric Literals

When an invalid numeric literal is encountered, the lexer will generate an error message indicating the invalid token and its location:

```
Error: Invalid numeric literal: 123abc
  --> example.medi:5:10
   |
 5 | let x = 123abc;
   |          ^^^^^^ Invalid numeric literal
   |
   = help: Numeric literals cannot be directly followed by letters
```

### Error Recovery

The lexer implements basic error recovery by:
1. Emitting an error token with a descriptive message
2. Continuing to tokenize the remaining input
3. Providing the line and column numbers for error location

### Common Error Messages

- `Invalid numeric literal: <token>` - When a number is followed by invalid characters
- `Unterminated string literal` - When a string is not properly closed
- `Unrecognized token: <char>` - When encountering an unexpected character
- `Unterminated block comment` - When a block comment is not properly closed

## Token Types

The lexer recognizes the following token categories:

- **Keywords**: Language reserved words like `let`, `fn`, `if`, etc.
- **Identifiers**: Variable and function names
- **Literals**: Numbers, strings, booleans, etc.
- **Operators**: `+`, `-`, `*`, `/`, etc.
- **Delimiters**: `(`, `)`, `{`, `}`, `,`, `;`, etc.
- **Whitespace**: Spaces, tabs, newlines
- **Comments**: Both line (`//`) and block (`/* */`) comments

## Best Practices

1. Always separate numbers from identifiers with whitespace
2. Use consistent numeric literal formats throughout your code
3. Pay attention to error messages for quick debugging
4. Use comments to document non-obvious numeric literals
