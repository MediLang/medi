# Parsing Approach (Recursive Descent)

This document outlines the structure and strategy of the Medi parser.

## Goals
- Clear, hand-written recursive descent for readability and precise diagnostics.
- Full operator precedence/associativity implemented via precedence-climbing (or layered RD functions).
- Memory-efficient on large inputs (multi-MB), avoiding unnecessary allocations.

## High-Level Architecture
- Token source: streaming/chunked lexer feeds `TokenSlice` views to the parser.
- Entry points:
  - `parse_program()` → `Vec<StatementNode>`
  - `parse_statement()` → `StatementNode`
  - `parse_expression(precedence)` → `ExpressionNode`
- Error handling:
  - Early error return with span-rich diagnostics.
  - Basic recovery using synchronizing tokens (e.g., `;`, `}`) to continue.

## Declarations
- Functions: `fn ident (params) [-> type] block`
  - Params: `ident [: type]` comma-separated
  - Body: `{ ... }` parsed by `parse_block()`
- Other decls: `let/const`, type/struct/enum/trait/impl (as supported in v0.1)

## Statements
- Block, return, if/else, while/for, loop/break/continue, match, expr-stmt.

## Expressions and Precedence (low → high)
1. Assignment: `=, +=, -=, *=, /=, %=` (right-associative)
2. Logical OR: `||`
3. Logical AND: `&&`
4. Equality: `==, !=`
5. Comparison: `<, <=, >, >=`
6. Additive: `+, -`
7. Multiplicative: `*, /, %`
8. Unary prefix: `+, -, !`
9. Postfix/calls/index/field: `()`, `[]`, `.`
10. Primary: literals, identifiers, grouped `(expr)`

Implementation is either:
- Precedence-climbing with a loop on binary operators and recursive calls for RHS with tighter precedence; or
- Layered RD functions (e.g., `parse_or()`, `parse_and()`, ...), each delegating down.

## Healthcare-Specific Constructs
- `fhir_query { ... }`: block-like construct with domain-specific clauses.
- `regulate { ... }`: scoped compliance block.
- Keywords: `federated`, `scope`, `real_time` integrated where grammar specifies.
- Medical literals: `pid("..."), icd10("..."), ...` parsed as primary expressions with typed literal nodes.

## Diagnostics
- Map parser errors to clinician-friendly hints (token confusion, unmatched braces, etc.).
- Include source spans for messages and potential quick-fixes.

## Large-Input Handling
- Work on slices into a token buffer; avoid cloning token vectors.
- Minimize backtracking; prefer single-pass with small lookahead.

## Testing Strategy
- Unit tests per construct.
- Precedence/associativity matrix tests.
- Deeply nested structures.
- PRD-derived healthcare samples for `fhir_query` and `regulate`.

See `docs/parser_acceptance_checklist.md` to track implementation and tests.
