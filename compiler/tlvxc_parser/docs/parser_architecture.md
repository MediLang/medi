# Medi Language Parser Architecture

## Overview

The Medi parser is a hand-written recursive descent parser that transforms a stream of tokens into an abstract syntax tree (AST). It's built using the `nom` parser combinator library, which provides a powerful framework for building composable parsers.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Parser Phases](#parser-phases)
3. [Key Components](#key-components)
4. [Expression Parsing](#expression-parsing)
5. [Error Handling](#error-handling)
6. [Performance Considerations](#performance-considerations)
7. [Testing Strategy](#testing-strategy)
8. [Future Improvements](#future-improvements)

## Architecture Overview

The parser follows a modular design with these main components:

- **Token Stream**: Wraps the token stream with position tracking and lookahead capabilities
- **Expression Parser**: Handles complex expressions with operator precedence and associativity
- **Statement Parser**: Processes control flow constructs and declarations
- **AST Generation**: Constructs the abstract syntax tree with source location information

## Parser Phases

### 1. Lexical Analysis (Lexer)
- Converts source text into tokens
- Handles whitespace and comments
- Tracks source locations for error reporting

### 2. Syntactic Analysis (Parser)
- Parses tokens into an AST
- Validates syntax according to the language grammar
- Handles operator precedence and associativity
- Performs basic semantic validation

### 3. Semantic Analysis (Future)
- Type checking
- Name resolution
- Other semantic validations

## Key Components

### Token Stream

The `TokenSlice` type provides a view into the token stream with:
- Lookahead capabilities
- Position tracking
- Convenience methods for token consumption
- Error recovery support

### Expression Parser

The expression parser handles the most complex part of the grammar, including:
- Binary and unary operators with proper precedence
- Parenthesized expressions
- Function calls and member access
- Match expressions

### Statement Parser

Handles all statement types:
- Variable declarations (`let`)
- Control flow (`if`, `while`, `for`)
- Expression statements
- Block statements
- Match statements

## Expression Parsing

The expression parser uses the **precedence climbing** algorithm to handle operator precedence and associativity. This approach is more efficient than the shunting-yard algorithm and is well-suited for recursive descent parsers.

### Operator Precedence

Operators are listed in order of increasing precedence:

| Operator(s) | Associativity | Description           |
|-------------|---------------|-----------------------|
| `||`        | Left          | Logical OR            |
| `&&`        | Left          | Logical AND           |
| `==`, `!=`  | Left          | Equality comparison   |
| `<`, `>`, `<=`, `>=` | Left  | Relational comparison |
| `+`, `-`    | Left          | Addition/Subtraction  |
| `*`, `/`, `%` | Left        | Multiplication/Division/Modulo |
| `**`        | Right         | Exponentiation        |
| `of`, `per` | Left          | Medical operators     |
| `!`, `-`    | Right         | Unary operators       |

### Grammar

```ebnf
expression     → assignment ;
assignment     → IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "||" logic_and )* ;
logic_and      → equality ( "&&" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" | "%" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")"
               | IDENTIFIER
               | "match" expression "{" match_arm* "}"
               | IDENTIFIER "{" match_arm* "}"  ; // concise match syntax (expressions too)
match_arm      → pattern "=>" expression ","? ;
pattern        → IDENTIFIER | "_" | literal ;
```

### Concise Match Syntax

In addition to the keyword form `match <expr> { ... }`, Medi supports a concise form when matching directly on an identifier. This concise form is supported in both statement and expression contexts:

```
x {
  1 => "one",
  2 => "two",
  _ => "other"
}
```

The parser recognizes both forms in statements and expressions:
- See `parse_match_statement()` in `compiler/medic_parser/src/parser/statements/mod.rs` for statement context
- See `parse_match_expression()` in `compiler/medic_parser/src/parser/expressions/mod.rs` for expression context

Patterns supported in arms include identifiers, `_` wildcard, and literals (int, float, string, bool). Trailing commas are allowed after the last arm, and missing commas between arms produce a parse error. Nested match expressions and block bodies inside arms are also supported.

## Function Declarations

Functions are parsed by `parse_function_declaration()` in `compiler/medic_parser/src/parser/statements/mod.rs`.

### Grammar

```ebnf
function_decl   → "fn" IDENTIFIER "(" param_list? ")" return_type? block ;
param_list      → param ( "," param )* ;
param           → IDENTIFIER ( ":" type_ident )? ;
return_type     → "->" type_ident ;
type_ident      → IDENTIFIER ; // simple identifiers only (no generics yet)
```

Notes:
- Parameters may optionally include a simple type annotation after `:`.
- Default parameter values and variadic parameters (e.g., `...`) are not supported.
- Return type is optional; omit `-> T` for no declared return type.
- The function body is a standard block; statement semicolons are tolerated in some contexts (e.g., missing `;` after `let` emits a warning; `return` may omit `;` at end of block).

## Error Handling

The parser provides detailed error messages with source locations and implements error recovery to continue parsing after encountering syntax errors.

### Error Recovery

- Tracks source locations for precise error reporting
- Attempts to recover from common errors
- Provides suggestions for fixing syntax errors
- Continues parsing after errors when possible

### Error Types

1. **Syntax Errors**: Invalid syntax that doesn't match the grammar
2. **Semantic Errors**: Valid syntax but invalid semantics (e.g., undefined variables)
3. **Type Errors**: Type mismatches in expressions
4. **Scope Errors**: Variable usage outside its scope

## Performance Considerations

### Memory Efficiency
- Uses slices instead of owned types where possible
- Minimizes allocations during parsing
- Implements streaming for large inputs

### Speed Optimizations
- Lookahead for fast path selection
- Minimal copying of token data
- Efficient data structures for AST nodes

## Testing Strategy

The parser includes a comprehensive test suite with:

### Unit Tests
- Individual parser functions
- Edge cases and error conditions
- Operator precedence and associativity

### Integration Tests
- Complete source files
- End-to-end parsing
- Error recovery scenarios

### Property Tests
- Round-trip parsing and pretty-printing
- Parser invariants
- Fuzzing with random inputs

## Future Improvements

1. **Incremental Parsing**: Support for re-parsing only changed parts of the source
2. **Better Error Recovery**: More sophisticated error recovery strategies
3. **Parallel Parsing**: Leverage multiple cores for large files
4. **Syntax Highlighting**: Integration with editor tooling
5. **Language Server Protocol**: Support for IDEs and editors

## Conclusion

The Medi parser is designed to be maintainable, efficient, and extensible. Its modular architecture allows for easy addition of new language features while maintaining good performance and helpful error messages.
