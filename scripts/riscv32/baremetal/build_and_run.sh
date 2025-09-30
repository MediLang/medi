#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
BK_DIR="$ROOT_DIR/scripts/riscv32/baremetal"
OUT_DIR="$ROOT_DIR/out/riscv32"
OBJ_OUT="$OUT_DIR/simple_add.o"
ELF_OUT="$OUT_DIR/simple_add_bare.elf"

mkdir -p "$OUT_DIR"

# Ensure object exists
if [ ! -f "$OBJ_OUT" ]; then
  echo "Object file not found: $OBJ_OUT"
  echo "Running scripts/riscv32/build.sh to generate it..."
  bash "$ROOT_DIR/scripts/riscv32/build.sh"
fi

# Detect toolchain prefix (32 -> 64 fallback)
TOOL_PREFIX=""
if command -v riscv32-unknown-elf-gcc >/dev/null 2>&1; then
  TOOL_PREFIX="riscv32-unknown-elf-"
elif command -v riscv64-unknown-elf-gcc >/dev/null 2>&1; then
  TOOL_PREFIX="riscv64-unknown-elf-"
else
  echo "No RISC-V GCC toolchain found (riscv32-unknown-elf-gcc or riscv64-unknown-elf-gcc)."
  exit 1
fi

# Assemble startup and link bare-metal without libc
"${TOOL_PREFIX}gcc" -c -march=rv32imac -mabi=ilp32 -nostdlib -Os -o "$OUT_DIR/start.o" "$BK_DIR/start.S"
"${TOOL_PREFIX}gcc" -nostartfiles -nostdlib -Wl,-gc-sections \
  -march=rv32imac -mabi=ilp32 -T"$BK_DIR/linker.ld" \
  -o "$ELF_OUT" "$OUT_DIR/start.o" "$OBJ_OUT"

echo "Linked bare-metal ELF: $ELF_OUT"

# Show size/objdump if available
if command -v "${TOOL_PREFIX}size" >/dev/null 2>&1; then
  echo "\n[${TOOL_PREFIX}size]"
  "${TOOL_PREFIX}size" "$ELF_OUT" || true
fi
if command -v "${TOOL_PREFIX}objdump" >/dev/null 2>&1; then
  echo "\n[${TOOL_PREFIX}objdump -d (head)]"
  "${TOOL_PREFIX}objdump" -d "$ELF_OUT" | sed -n '1,200p' || true
fi

# Run in QEMU system emulator
if command -v qemu-system-riscv32 >/dev/null 2>&1; then
  echo "\n[Running under qemu-system-riscv32 -machine virt -nographic]"
  qemu-system-riscv32 \
    -machine virt \
    -nographic \
    -bios none \
    -serial mon:stdio \
    -kernel "$ELF_OUT" || true
else
  echo "\nqemu-system-riscv32 not found; install qemu-system-riscv (Ubuntu) and re-run."
  exit 0
fi
