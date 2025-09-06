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
cargo build -p medic_codegen_llvm --features llvm

# Or build the compiler frontend with the backend enabled
cargo build -p medic --features llvm-backend
```

## What’s implemented

- Target initialization for x86_64, wasm32, and riscv32
- Thin wrappers for `Context`, `Module`, and `IRBuilder`
- High-level skeleton functions:
  - `generate_llvm_ir(&str)`
  - `optimize_module(level: u8)`
  - `generate_target_code(TargetKind)`

These are scaffolds to establish the build and linking pipeline. Future changes will translate
Medi AST into IR, wire optimization passes, and emit code for the supported targets.
