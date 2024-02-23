# Disassembler

## Introduction
To help with testing this lab, we have provided you with code from an [Open Source RISC-V Disassembler](https://github.com/michaeljclark/riscv-disassembler). The disassembler will translate hexadecimal instructions back to their written representation (0xFF410113 -> addi sp, sp, -12). This can help to make sense of what your output code is doing.

## How to Use It
Run `print-instructions.c` with hexadecimal instructions to disassemble as input.
- First, compile `print-instructions.c` by running your favourite C compiler. For example, on the commmand line execute `gcc print-instructions.c` or `clang print-instructions.c`.
- Next, create a text file of your hexadecimal instructions. Each instruction should appear on a newline. We have provided an `example.txt` file for how this should look like.
- Pass the text file to the executable (`./a.out example.txt`)

## Errors
Since this is an Open Source Disassembler, there are some bugs and quirks to be aware of:
- The disassembler does not translate `auipc` instructions correctly. The immediates that the disassembler prints for these instructions are incorrect. **Ignore the `auipc` instructions**. A solution to this lab **should not alter** the `auipc` instructions, just the `addi` instructions after.
- Remember that `la` translates to `auipc` then `addi`. If the immediate on the `addi` instruction is 0, the disassembler will think it's a `mv` instruction. For example, `addi t0, t0, 0` will be printed by the disassembler as `mv t0, t0`.
- The disassembler will not translate the sentinel value. This is as expected.