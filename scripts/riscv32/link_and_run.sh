#!/usr/bin/env bash
set -euo pipefail

# Link the previously built RISC-V object into an executable and run it in QEMU (if available).
# Requires a RISC-V bare-metal toolchain (riscv32-unknown-elf-*) and qemu-riscv32.
# This uses the toolchain's default crt0/newlib startup when available.

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
OUT_DIR="$ROOT_DIR/out/riscv32"
OBJ_OUT="$OUT_DIR/simple_add.o"
ELF_OUT="$OUT_DIR/simple_add.elf"

if [ ! -f "$OBJ_OUT" ]; then
  echo "Object file not found: $OBJ_OUT"
  echo "Run ./scripts/riscv32/build.sh first."
  exit 1
fi

# Detect toolchain prefix (prefer 32-bit prefix, fallback to 64-bit)
TOOL_PREFIX=""
if command -v riscv32-unknown-elf-gcc >/dev/null 2>&1; then
  TOOL_PREFIX="riscv32-unknown-elf-"
elif command -v riscv64-unknown-elf-gcc >/dev/null 2>&1; then
  TOOL_PREFIX="riscv64-unknown-elf-"
else
  echo "No RISC-V GCC toolchain found (riscv32-unknown-elf-gcc or riscv64-unknown-elf-gcc)."
  echo "Install a RISC-V GNU toolchain and re-run this script."
  exit 0
fi

# Link with standard libraries (newlib). This assumes your toolchain provides crt0 and libs.
"${TOOL_PREFIX}gcc" -march=rv32imac -mabi=ilp32 \
  -Wl,-gc-sections -Os -ffunction-sections -fdata-sections \
  -o "$ELF_OUT" "$OBJ_OUT"

echo "Linked ELF: $ELF_OUT"

# Optional: show size (using matching prefix if available)
if command -v "${TOOL_PREFIX}size" >/dev/null 2>&1; then
  echo "\n[${TOOL_PREFIX}size]"
  "${TOOL_PREFIX}size" "$ELF_OUT" || true
fi

# Optional: disassembly
if command -v "${TOOL_PREFIX}objdump" >/dev/null 2>&1; then
  echo "\n[${TOOL_PREFIX}objdump -d]"
  "${TOOL_PREFIX}objdump" -d "$ELF_OUT" | sed -n '1,200p' || true
fi

# Optional: run under QEMU if available
if command -v qemu-riscv32 >/dev/null 2>&1; then
  echo "\n[Running under qemu-riscv32]"
  qemu-riscv32 "$ELF_OUT" || true
else
  echo "\nqemu-riscv32 not found; skipping run."
fi
