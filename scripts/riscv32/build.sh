#!/usr/bin/env bash
set -euo pipefail

# Build a riscv32 object from the example Medi program.
# Requirements:
# - medi compiler built with --features llvm-backend
#
# Optional tools for inspection (if present):
# - llvm-size or riscv32-unknown-elf-size
# - llvm-objdump or riscv32-unknown-elf-objdump

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
EXAMPLE_DIR="$ROOT_DIR/examples/riscv32"
OUT_DIR="$ROOT_DIR/out/riscv32"
mkdir -p "$OUT_DIR"

EXAMPLE_SRC="$EXAMPLE_DIR/simple_add.medi"
OBJ_OUT="$OUT_DIR/simple_add.o"

# Allow overrides
OPT_LEVEL=${MEDI_LLVM_OPT:-2}
CPU=${MEDI_LLVM_CPU:-generic}
FEATURES=${MEDI_LLVM_FEATURES:-}

# Build object
"$ROOT_DIR/target/debug/medic" --emit=riscv32 --opt="$OPT_LEVEL" --cpu="$CPU" --features="$FEATURES" --out="$OBJ_OUT" "$EXAMPLE_SRC"

BYTES=$(wc -c < "$OBJ_OUT")
echo "Built RISC-V object: $OBJ_OUT (${BYTES} bytes)"

# Footprint report via size tool (best-effort)
if command -v riscv32-unknown-elf-size >/dev/null 2>&1; then
  echo "\n[riscv32-unknown-elf-size]"
  riscv32-unknown-elf-size "$OBJ_OUT" || true
elif command -v llvm-size >/dev/null 2>&1; then
  echo "\n[llvm-size]"
  llvm-size "$OBJ_OUT" || true
else
  echo "\n(size tool not found; install llvm-size or riscv32-unknown-elf-size to see section sizes)"
fi

# Optional disassembly for inspection
if command -v riscv32-unknown-elf-objdump >/dev/null 2>&1; then
  echo "\n[riscv32-unknown-elf-objdump -d]"
  riscv32-unknown-elf-objdump -d "$OBJ_OUT" | sed -n '1,200p' || true
elif command -v llvm-objdump >/dev/null 2>&1; then
  echo "\n[llvm-objdump -d]"
  llvm-objdump -d "$OBJ_OUT" | sed -n '1,200p' || true
else
  echo "\n(objdump not found; install llvm-objdump or riscv32-unknown-elf-objdump for disassembly)"
fi
