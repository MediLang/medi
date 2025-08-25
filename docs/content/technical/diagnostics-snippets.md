# Interpreting Annotated Diagnostics

Medi shows clinician‑friendly diagnostic messages with an annotated snippet of your source code to help you quickly understand and fix issues.

Each diagnostic includes:

- Severity and message.
- Location (line and column).
- The source line with a gutter.
- An underline marking the exact span.
- Optional help guidance.

## Example

```
error: Unexpected token: RightBracket
 --> line 4, col 10
  |
 4 | let meds = list]
  |           ^  help: Did you forget a matching '[' earlier?
```

- "error" is the severity. Others include "warning", "info", and "note".
- The caret (^) or tildes (~) underline the offending span.
  - Single‑character spans use a caret.
  - Multi‑character spans use tildes.
- A contextual "help" hint may appear after the underline.

## Reading the underline

- Caret under the first column: the issue is at the start of the line.
- Caret at the last column: the issue is at the end of the line.
- Multiple tildes: the entire operator/identifier is highlighted (e.g., `==`).

## Multi‑line files

If your code spans multiple lines, the snippet will show the specific line and column for the diagnostic. Only the relevant line is shown to keep focus clear.

## Informational messages

The "info" severity communicates non‑actionable guidance (e.g., tips or context) and uses the same snippet format so you can locate the referenced code quickly.

## Common guidance

Medi offers domain‑aware help messages, for example:

- Confusing `=` with `==` in comparisons.
- Unmatched brackets `)`, `]`, or `}`.
- Misplaced clinical operators such as `per` or `of`.

These hints appear as `help: ...` after the underline.
