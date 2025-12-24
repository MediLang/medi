# Medi Language Reference

## Reference Index

This section links to the authoritative language and tooling references.

- **Language Specification**: [Language Specification](language-spec.md)
- **Standard Library (overview)**: [Standard Library](standard-library.md)
- **Tooling / CLI**: [Tooling](../technical/tooling.md)
- **Lexer**: [Lexer](lexer.md)
- **Compatibility Matrix**: [Compatibility Matrix](compatibility-matrix.md)
- **Benchmarks**: [Benchmarks](../technical/benchmarks.md)

## Minimal v0.1 example

```medi
fn is_hypertensive(systolic: int, diastolic: int) -> bool {
  systolic >= 140 || diastolic >= 90
}

record Patient {
  name: string,
  systolic: int,
  diastolic: int
}

let p = Patient{name: "Patient A", systolic: 145, diastolic: 95};
let flag = is_hypertensive(p.systolic, p.diastolic);
```
