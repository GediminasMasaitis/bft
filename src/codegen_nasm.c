#include <stdio.h>

#include "codegen.h"

static const i32 use_shift_and_mask = 1;

static int get_shift(int n) {
  if (!use_shift_and_mask) {
    return -1;
  }

  if (n < 2) {
    return -1;
  }

  if ((n & (n - 1)) != 0) {
    return -1;
  }

  i32 shift = 0;
  while ((1 << shift) < n) {
    shift++;
  }

  return shift;
}

void codegen_nasm(const Program *program, FILE *output) {
  fprintf(output, "section .bss\n");
  fprintf(output, "    cells resb %d\n", CELL_COUNT);
  fprintf(output, "\n");

  fprintf(output, "section .text\n");
  fprintf(output, "global _start\n");
  fprintf(output, "\n");
  fprintf(output, "_start:\n");
  fprintf(output,
          "    mov rbx, cells      ; rbx = data pointer (callee-saved)\n");
  fprintf(output, "\n");

  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      if (instr->arg == 1) {
        fprintf(output, "    inc rbx\n");
      } else if (instr->arg == -1) {
        fprintf(output, "    dec rbx\n");
      } else if (instr->arg > 0) {
        fprintf(output, "    add rbx, %d\n", instr->arg);
      } else {
        fprintf(output, "    sub rbx, %d\n", -instr->arg);
      }
      break;

    case OP_INC:
      if (instr->offset == 0) {
        if (instr->arg == 1) {
          fprintf(output, "    inc byte [rbx]\n");
        } else if (instr->arg == -1) {
          fprintf(output, "    dec byte [rbx]\n");
        } else if (instr->arg > 0) {
          fprintf(output, "    add byte [rbx], %d\n", instr->arg);
        } else {
          fprintf(output, "    sub byte [rbx], %d\n", -instr->arg);
        }
      } else {
        if (instr->arg == 1) {
          fprintf(output, "    inc byte [rbx%+d]\n", instr->offset);
        } else if (instr->arg == -1) {
          fprintf(output, "    dec byte [rbx%+d]\n", instr->offset);
        } else if (instr->arg > 0) {
          fprintf(output, "    add byte [rbx%+d], %d\n", instr->offset,
                  instr->arg);
        } else {
          fprintf(output, "    sub byte [rbx%+d], %d\n", instr->offset,
                  -instr->arg);
        }
      }
      break;

    case OP_OUT:
      fprintf(output, "    mov rax, 1          ; write\n");
      fprintf(output, "    mov rdi, 1          ; stdout\n");
      if (instr->offset == 0) {
        fprintf(output, "    mov rsi, rbx        ; current cell\n");
      } else {
        fprintf(output, "    lea rsi, [rbx%+d]   ; cell at offset %d\n",
                instr->offset, instr->offset);
      }
      fprintf(output, "    mov rdx, 1          ; 1 byte\n");
      fprintf(output, "    syscall\n");
      break;

    case OP_IN:
      fprintf(output, "    mov rax, 0          ; read\n");
      fprintf(output, "    mov rdi, 0          ; stdin\n");
      if (instr->offset == 0) {
        fprintf(output, "    mov rsi, rbx        ; current cell\n");
      } else {
        fprintf(output, "    lea rsi, [rbx%+d]   ; cell at offset %d\n",
                instr->offset, instr->offset);
      }
      fprintf(output, "    mov rdx, 1          ; 1 byte\n");
      fprintf(output, "    syscall\n");
      break;

    case OP_LOOP:
      fprintf(output, ".loop%d_start:\n", i);
      if (instr->offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->offset);
      }
      fprintf(output, "    je .loop%d_end\n", i);
      break;

    case OP_END:
      if (instr->offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->offset);
      }
      fprintf(output, "    jne .loop%d_start\n", instr->arg);
      fprintf(output, ".loop%d_end:\n", instr->arg);
      break;

    case OP_SET:
      if (instr->arg2 <= 1) {
        if (instr->offset == 0) {
          fprintf(output, "    mov byte [rbx], %d\n", instr->arg);
        } else {
          fprintf(output, "    mov byte [rbx%+d], %d\n", instr->offset,
                  instr->arg);
        }
      } else if (instr->stride == 0 || instr->stride == 1) {
        if (instr->offset == 0) {
          fprintf(output, "    ; memset %d cells to %d\n", instr->arg2,
                  instr->arg);
          fprintf(output, "    mov rdi, rbx\n");
        } else {
          fprintf(output, "    lea rdi, [rbx%+d]\n", instr->offset);
        }
        fprintf(output, "    mov al, %d\n", instr->arg);
        fprintf(output, "    mov rcx, %d\n", instr->arg2);
        fprintf(output, "    rep stosb\n");
      } else {
        fprintf(output, "    ; strided set: %d cells, stride %d, value %d\n",
                instr->arg2, instr->stride, instr->arg);
        fprintf(output, "    mov rcx, %d\n", instr->arg2);
        if (instr->offset == 0) {
          fprintf(output, "    mov rdi, rbx\n");
        } else {
          fprintf(output, "    lea rdi, [rbx%+d]\n", instr->offset);
        }
        fprintf(output, ".strided_set_%d:\n", i);
        fprintf(output, "    mov byte [rdi], %d\n", instr->arg);
        fprintf(output, "    add rdi, %d\n", instr->stride);
        fprintf(output, "    dec rcx\n");
        fprintf(output, "    jnz .strided_set_%d\n", i);
      }
      break;

    case OP_SEEK_EMPTY:
      fprintf(output, ".find_empty_%d:\n", i);
      if (instr->offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->offset);
      }
      fprintf(output, "    je .find_empty_done_%d\n", i);
      if (instr->arg == 1) {
        fprintf(output, "    inc rbx\n");
      } else if (instr->arg == -1) {
        fprintf(output, "    dec rbx\n");
      } else if (instr->arg > 1) {
        fprintf(output, "    add rbx, %d\n", instr->arg);
      } else {
        fprintf(output, "    sub rbx, %d\n", -instr->arg);
      }
      fprintf(output, "    jmp .find_empty_%d\n", i);
      fprintf(output, ".find_empty_done_%d:\n", i);
      break;

    case OP_TRANSFER:
      if (instr->offset == 0) {
        fprintf(output, "    movzx eax, byte [rbx]\n");
      } else {
        fprintf(output, "    movzx eax, byte [rbx%+d]\n", instr->offset);
      }

      if (instr->arg2 == 1 && instr->arg == 1) {
        // Assignment mode
        i32 offset = instr->targets[0].offset;
        i32 factor = instr->targets[0].factor;
        i32 bias = instr->targets[0].bias;

        if (factor == 1) {
          if (bias != 0) {
            fprintf(output, "    add al, %d\n", bias);
          }
          fprintf(output, "    mov byte [rbx%+d], al\n", offset);
        } else if (factor == -1) {
          fprintf(output, "    neg al\n");
          if (bias != 0) {
            fprintf(output, "    add al, %d\n", bias);
          }
          fprintf(output, "    mov byte [rbx%+d], al\n", offset);
        } else if (factor > 0) {
          fprintf(output, "    mov cl, %d\n", factor);
          fprintf(output, "    imul cl\n");
          if (bias != 0) {
            fprintf(output, "    add al, %d\n", bias);
          }
          fprintf(output, "    mov byte [rbx%+d], al\n", offset);
        } else {
          fprintf(output, "    mov cl, %d\n", -factor);
          fprintf(output, "    imul cl\n");
          fprintf(output, "    neg al\n");
          if (bias != 0) {
            fprintf(output, "    add al, %d\n", bias);
          }
          fprintf(output, "    mov byte [rbx%+d], al\n", offset);
        }
      } else {
        for (int t = 0; t < instr->arg; t++) {
          i32 bias = instr->targets[t].bias;
          if (bias != 0) {
            i32 offset = instr->targets[t].offset;
            if (bias > 0) {
              fprintf(output, "    add byte [rbx%+d], %d\n", offset, bias);
            } else {
              fprintf(output, "    sub byte [rbx%+d], %d\n", offset, -bias);
            }
          }
        }

        fprintf(output, "    test al, al\n");
        fprintf(output, "    jz .transfer_done_%d\n", i);

        for (int t = 0; t < instr->arg; t++) {
          i32 offset = instr->targets[t].offset;
          i32 factor = instr->targets[t].factor;

          if (factor == 1) {
            fprintf(output, "    add byte [rbx%+d], al\n", offset);
          } else if (factor == -1) {
            fprintf(output, "    sub byte [rbx%+d], al\n", offset);
          } else if (factor > 0) {
            fprintf(output, "    mov cl, %d\n", factor);
            fprintf(output, "    imul cl\n");
            fprintf(output, "    add byte [rbx%+d], al\n", offset);
            if (t < instr->arg - 1) {
              if (instr->offset == 0) {
                fprintf(output, "    movzx eax, byte [rbx]\n");
              } else {
                fprintf(output, "    movzx eax, byte [rbx%+d]\n",
                        instr->offset);
              }
            }
          } else {
            fprintf(output, "    mov cl, %d\n", -factor);
            fprintf(output, "    imul cl\n");
            fprintf(output, "    sub byte [rbx%+d], al\n", offset);
            if (t < instr->arg - 1) {
              if (instr->offset == 0) {
                fprintf(output, "    movzx eax, byte [rbx]\n");
              } else {
                fprintf(output, "    movzx eax, byte [rbx%+d]\n",
                        instr->offset);
              }
            }
          }
        }

        fprintf(output, ".transfer_done_%d:\n", i);
      }
      break;

    case OP_DIV: {
      // dp[targets[0].offset] += dp[offset] / divisor
      i32 div_off = instr->offset;
      i32 quot_off = instr->targets[0].offset;
      i32 divisor = instr->arg;
      int shift = get_shift(divisor);

      fprintf(output, "    ; div by %d\n", divisor);
      fprintf(output, "    movzx eax, byte [rbx%+d]\n", div_off);

      if (shift > 0) {
        // Use bit shift for power of 2
        fprintf(output, "    shr al, %d\n", shift);
        fprintf(output, "    add byte [rbx%+d], al\n", quot_off);
      } else {
        fprintf(output, "    xor edx, edx\n");
        fprintf(output, "    mov cl, %d\n", divisor);
        fprintf(output, "    div cl\n");
        fprintf(output, "    add byte [rbx%+d], al\n", quot_off);
      }
      break;
    }

    case OP_MOD: {
      // dp[targets[0].offset] = dp[offset] % divisor
      i32 div_off = instr->offset;
      i32 rem_off = instr->targets[0].offset;
      i32 divisor = instr->arg;
      int shift = get_shift(divisor);

      fprintf(output, "    ; mod by %d\n", divisor);
      fprintf(output, "    movzx eax, byte [rbx%+d]\n", div_off);

      if (shift > 0) {
        // Use bit mask for power of 2
        fprintf(output, "    and al, %d\n", divisor - 1);
        fprintf(output, "    mov byte [rbx%+d], al\n", rem_off);
      } else {
        fprintf(output, "    xor edx, edx\n");
        fprintf(output, "    mov cl, %d\n", divisor);
        fprintf(output, "    div cl\n");
        fprintf(output, "    mov byte [rbx%+d], ah\n", rem_off);
      }
      break;
    }

    default:
      fprintf(output, "    ; UNKNOWN OP: %c\n", instr->op);
      break;
    }
  }

  fprintf(output, "\n");
  fprintf(output, "    mov rax, 60\n");
  fprintf(output, "    xor rdi, rdi\n");
  fprintf(output, "    syscall\n");
}
