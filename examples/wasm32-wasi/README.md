# Medi wasm32-wasi Example

This example demonstrates compiling a small Medi program to a WebAssembly module targeting WASI.

## Prerequisites

- Build Medi with the LLVM backend enabled
- A WASI runtime, e.g. `wasmtime` or `wasmer`

```bash
# Build the compiler with LLVM backend
cargo build -p medic --features llvm-backend

# Verify a WASI runtime is installed (example: wasmtime)
wasmtime --version
```

## Compile to wasm32-wasi

```bash
# From repo root
./target/debug/medic --emit=wasm32 --out=hello.wasm examples/wasm32-wasi/hello.tlvx
```

This will produce `hello.wasm`. The compiler lowers top-level statements into a `_start` entrypoint as required by WASI. The current example uses only arithmetic (no I/O) and therefore does not print.

## Run with a WASI runtime

```bash
# Using wasmtime
wasmtime hello.wasm

# Using wasmer
wasmer run hello.wasm
```

Notes:

- I/O intrinsics (e.g., printing) have not been wired yet. Additions can be made by introducing WASI imports and corresponding Medi intrinsics.
- If you want to see the LLVM IR instead of producing a `.wasm`, omit `--out`:

```bash
./target/debug/medic --emit=wasm32 examples/wasm32-wasi/hello.tlvx
```
