# Tooling

Medi's current compiler/CLI tool is `medic`.

## Building

From the repository root:

```bash
cargo build -p medic
```

## Quick check

Typecheck a file and print diagnostics:

```bash
cargo run -p medic -- check path/to/file.medi
```

## JSON output

Emit machine-readable diagnostics and type information:

```bash
cargo run -p medic -- json path/to/file.medi
```

You can also provide function type metadata via `--types-json`:

```bash
cargo run -p medic -- check --types-json path/to/types.json path/to/file.medi
```

## REPL

Interactive parsing/typechecking feedback:

```bash
cargo run -p medic -- repl
```

## Docs generator

Generate Markdown documentation from `///` / `//!` doc comments:

```bash
cargo run -p medic -- docs --out-dir out_docs path/to/file.medi
```

## Package manifest (experimental)

Initialize a minimal `medipack.toml` manifest:

```bash
cargo run -p medic -- pack init
```

List manifest contents:

```bash
cargo run -p medic -- pack list
```

## LLVM backend (optional)

Some targets and `--emit/--out` flags are available only when building with the `llvm-backend` feature (requires system LLVM 15.x).
