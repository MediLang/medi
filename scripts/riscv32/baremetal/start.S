.section .init
.global _start

/* QEMU virt machine: RAM at 0x80000000, UART0 at 0x10000000 */
.equ RAM_BASE, 0x80000000
.equ RAM_SIZE, (128 * 1024 * 1024)
.equ STACK_TOP, (RAM_BASE + RAM_SIZE - 0x100)
.equ UART0_BASE, 0x10000000
/* QEMU Test Finisher (exit): address 0x100000 */
.equ TEST_FINISHER, 0x00100000

/* UART registers (16550-compatible) */
.equ UART_THR, 0x00 /* Transmit Holding Register */
.equ UART_LSR, 0x05 /* Line Status Register */
.equ LSR_THRE, 0x20 /* Transmit Holding Register Empty */

_start:
  /* Set up stack pointer */
  la sp, stack_top

  /* Print "OK\n" to UART0 for visibility */
  la a0, msg
  la a1, msg_end
1:
  beq a0, a1, 2f
  /* wait until THR empty */
3:
  li t0, UART0_BASE
  addi t1, t0, UART_LSR
  lbu t2, 0(t1)
  andi t2, t2, LSR_THRE
  beqz t2, 3b
  /* write byte */
  lbu t3, 0(a0)
  sb t3, UART_THR(t0)
  addi a0, a0, 1
  j 1b
2:
  /* Call main() if present */
  la t0, main
  jr t0

  /* If main returns, signal PASS to QEMU Test Finisher and exit emulator */
  li t0, TEST_FINISHER
  li t1, 0x5555          /* PASS code */
  sw t1, 0(t0)
  /* Also request SBI legacy shutdown (requires -bios default/OpenSBI) */
  li a7, 0x08            /* SBI legacy shutdown */
  ecall
  /* Fallback if finisher not present: hang */
4:
  wfi
  j 4b

/* BSS-less simple stack top symbol */
.section .bss
.balign 16
stack_top:
  .space 0

.section .rodata
msg:
  .ascii "OK\n"
msg_end:
