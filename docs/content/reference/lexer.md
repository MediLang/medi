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
