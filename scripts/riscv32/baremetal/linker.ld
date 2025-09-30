/* Minimal RV32 bare-metal linker script for QEMU virt */
OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY
{
  RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 128M
}

SECTIONS
{
  /* If the generated object exports `main.1`, alias it to `main` so startup can call it */
  PROVIDE(main = main.1);

  .text : {
    KEEP(*(.init))
    *(.text .text.*)
  } > RAM

  .rodata : {
    *(.rodata .rodata.*)
  } > RAM

  .data : ALIGN(4) {
    *(.data .data.*)
  } > RAM

  .bss : ALIGN(4) {
    *(.bss .bss.* COMMON)
  } > RAM
}
