# Parser Acceptance Checklist

This checklist maps LANG_SPEC.md grammar constructs to implementation, tests, and docs. Use it to track completion for Task 3: Implement Recursive Descent Parser.

Legend: [ ] pending, [~] partial, [x] complete

## Core Declarations
- [ ] Module declarations (mod/import)
- [~] Function declarations `fn name(params) [-> type] { ... }`
  - Params: identifiers with optional type annotations
  - Return type: optional after `->`
  - Body: block
- [ ] Let/const declarations
- [ ] Type/struct/enum/trait/impl declarations (as specced for v0.1)
- [ ] Visibility modifiers: `pub`, `priv`

## Statements
- [ ] Expression statement (with/without trailing `;` as per spec)
- [ ] Return statement
- [ ] If/else
- [ ] While / loop / break / continue
- [ ] For-in
- [ ] Match
- [ ] Block `{ ... }`

## Expressions
- [ ] Literals (int/float/string/bool/datetime/medical literals)
- [ ] Identifiers and paths (`::`)
- [ ] Call, indexing, field access
- [ ] Unary ops: `+ - !`
- [ ] Multiplicative: `* / %`
- [ ] Additive: `+ -`
- [ ] Comparisons: `== != < <= > >=`
- [ ] Logical: `&& ||`
- [ ] Assignment family: `= += -= *= /= %=`
- [ ] Ranges: `.. ..= ...` as per spec availability
- [ ] Ternary / conditional if supported (spec)

## Healthcare Constructs
- [ ] `fhir_query { ... }` parse rules
- [ ] `regulate { ... }` blocks
- [ ] `federated`, `scope`, `real_time` keywords where applicable
- [ ] Medical literals: `pid("..."), icd10("..."), ...`

## Precedence and Associativity
- [ ] Full precedence table implemented in Pratt/precedence-climbing or RD hierarchy
- [ ] Associativity validated with tests

## Error Handling / Diagnostics
- [ ] Descriptive parser errors with spans
- [ ] Recovery where possible to continue after errors
- [ ] Diagnostics integrated with clinician-friendly messages

## Performance & Memory
- [ ] Parser handles large inputs (>= 8MB) without excessive allocations
- [ ] Benchmarks pass with silenced stdout/stderr

## Tests
- [ ] Unit tests for each grammar construct
- [ ] Precedence tests
- [ ] Nested constructs tests
- [ ] Healthcare construct samples from PRD

## Documentation
- [ ] Parsing approach and precedence documentation
- [ ] Examples for each construct

---

Maintenance:
- Update this file when implementing a construct, adding tests, or documenting behavior.
- Link test file names and example snippets as they are added.
