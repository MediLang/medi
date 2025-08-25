# Common Errors and Solutions

This page lists frequent issues seen in Medi code and how to resolve them, with examples of the annotated diagnostics you will see.

## Using '=' instead of '=='

- Symptom: assignment operator used where a comparison is intended.
- Solution: use `==` for equality comparison; keep `=` for assignment.

Example diagnostic:
```
error: Unexpected token: Equal
 --> line N, col M
  |
 N | if x = 3 { ... }
  |      ^  help: If you meant to compare two values, use '=='
```

## Unmatched brackets

- Symptom: `)`, `]`, or `}` appears without a matching opening bracket.
- Solution: add the corresponding opening bracket earlier in the line/block.

```
error: Unexpected token: RightBracket
 --> line N, col M
  |
 N | list]
  |     ^  help: Did you forget a matching '[' earlier?
```

## Stray or unrecognized text

- Symptom: Lexer error token appears (e.g., invalid symbol or garbled text).
- Solution: remove the text or replace it with a valid keyword/symbol.

```
error: Unrecognized token: '??='
 --> line N, col M
  |
 N | a ??= b
  |   ~~  help: This text is not valid Medi syntax. Remove it or replace with a valid symbol/keyword.
```

## Misusing clinical operators (e.g., 'per', 'of')

- Symptom: Operator is in the wrong context or missing units.
- Solution: ensure expressions follow domain usage, e.g., `mg per kg`, `2 of doses`.

```
warning: Value did not meet expected form
 --> line N, col M
  |
 N | dose per
  |      ^  help: 'per' expresses rates (e.g., 'mg per kg'). Ensure units are valid
```

## Incomplete input

- Symptom: File ends while a statement/block is unfinished.
- Solution: complete the statement or close the block.

```
error: Incomplete input
 --> line 1, col 1
  |
  |  help: The parser expected more input. Did the file end unexpectedly?
```

## Tips

- Caret (^) highlights single-character issues; tildes (~) highlight multi-character spans.
- Non-fatal parse errors surfaced during recovery are shown as warnings.
- Most diagnostics include a short `help:` hint with a suggested fix.
