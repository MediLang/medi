# medic_codegen_llvm

Feature-gated LLVM backend integration for the Medi compiler using [Inkwell](https://github.com/TheDan64/inkwell).

## Prerequisites

- LLVM 15.x development libraries installed on your system (headers + libs)
- Rust stable toolchain

On Ubuntu/Debian you can install via `apt` or using upstream LLVM packages. For example (adjust to your distro):

```bash
# Example using apt. Ensure you get LLVM 15 packages.
sudo apt-get update
sudo apt-get install -y llvm-15 llvm-15-dev clang-15

# Make sure llvm-config points to LLVM 15 or export explicitly for the build
export LLVM_SYS_150_PREFIX=/usr/lib/llvm-15
```

On macOS with Homebrew:

```bash
brew install llvm@15
# Point environment so inkwell/llvm-sys finds the correct version
export LLVM_SYS_150_PREFIX=$(brew --prefix llvm@15)
```

## Building

This backend is disabled by default to keep the workspace buildable without system LLVM installed.
Enable it via the `llvm-backend` feature on the `medic` crate (which plumbs to this crate's
`llvm` feature):

```bash
# Build only the backend crate
cargo build -p medic -p medic_codegen_llvm --features llvm-backend

# Or build the compiler frontend with the backend enabled
cargo build -p medic --features llvm-backend
```

## Quantity IR (experimental)

A feature-flagged path is available to represent quantities explicitly in IR as a struct
`{ double value, i32 unit_id }`. This is disabled by default and intended for development
and experimentation.

Enable both the LLVM backend and Quantity IR when building or testing:

```bash
# Build the backend with Quantity IR enabled
cargo build -p medic_codegen_llvm --features "llvm,quantity_ir"

# Run IR tests that are guarded by the feature flags
cargo test -p tests --features "llvm,quantity_ir" -- --nocapture
```

Notes:

- With `quantity_ir` disabled (default), quantities lower to plain `f64` and unit metadata is
  tracked in side tables for type checking and diagnostics.
- With `quantity_ir` enabled, quantities lower to a `%Quantity` struct and unit conversion is
  applied either at compile-time (known pairs) or via a fallback runtime function
  `medi_convert_q(%Quantity, i32 to_unit_id)`.
- For now, arithmetic on quantities is conservative: Add/Sub require matching units; Mul/Div
  and comparisons are disallowed until dimensional analysis is introduced. Convert explicitly
  first using the `->` operator.

## What’s implemented

- Target initialization for x86_64, wasm32, and riscv32
- Thin wrappers for `Context`, `Module`, and `IRBuilder`
- High-level skeleton functions:
  - `generate_llvm_ir(&str)`
  - `optimize_module(level: u8)`
  - `generate_target_code(TargetKind)`

These are scaffolds to establish the build and linking pipeline. Future changes will translate
Medi AST into IR, wire optimization passes, and emit code for the supported targets.

## WebAssembly (wasm32-wasi) target

The LLVM backend can emit WebAssembly modules targeting WASI. When there are top-level Medi
statements, the compiler lowers them into a WASI-compliant `_start(): void` entrypoint.

Build the compiler with the backend and emit a wasm module:

```bash
cargo build -p medic --features llvm-backend

# From repo root
./target/debug/medic --emit=wasm32 --out=hello.wasm examples/wasm32-wasi/hello.medi
```

You can run the result with a WASI runtime such as `wasmtime` or `wasmer`:

```bash
wasmtime hello.wasm
# or
wasmer run hello.wasm
```

Notes:

- The current example performs arithmetic only; I/O intrinsics (e.g., printing) are not yet wired.
- Omit `--out` to print the LLVM IR for inspection instead of writing a `.wasm` binary.

### WASI memory model and calling conventions

- Data layout and ABI: The module’s data layout is taken from LLVM’s target machine for `wasm32-wasi`, ensuring sizes/alignments conform to the WASI ABI.
- Entry point: When there are top-level statements, they are lowered into a `_start(): void` function, as required by WASI.
- Integer/float sizes: `Int` lowers to `i64`, `Float` to `f64`, `Bool` to `i1`. Pointers are in address space 0 (linear memory).
- Strings: Currently represented as `i8*` (pointer into linear memory) with explicit byte lengths where needed by imports.
- Calling convention: LLVM’s default CC for wasm is used; no custom CC attributes are applied at this time.
- Imports: Minimal I/O is implemented via the WASI `fd_write` import from module `wasi_snapshot_preview1`.

### Minimal print intrinsic

- Medi-level `print("string literal")` is lowered to a single `fd_write(1, &iovec, 1, &nwritten)` call.
- The backend constructs a single `iovec { i8* buf; i32 len; }` on the stack and issues the call. Only string literals are supported at present.
- Example usage in `examples/wasm32-wasi/hello.medi` prints a line to stdout under a WASI runtime.
