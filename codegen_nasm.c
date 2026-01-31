#include <stdio.h>

#include "bft.h"

static const i32 use_shift_and_mask = 1;

static int get_shift(const int n) {
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
  fprintf(output, "    cells resb %d\n", CELL_COUNT * 2);
  fprintf(output, "\n");

  fprintf(output, "section .text\n");
  fprintf(output, "global _start\n");
  fprintf(output, "\n");
  fprintf(output, "_start:\n");
  fprintf(output,
          "    mov rbx, cells + %d  ; rbx = data pointer (center of tape)\n",
          CELL_COUNT);
  fprintf(output, "\n");

  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      if (instr->right.distance == 1) {
        fprintf(output, "    inc rbx\n");
      } else if (instr->right.distance == -1) {
        fprintf(output, "    dec rbx\n");
      } else if (instr->right.distance > 0) {
        fprintf(output, "    add rbx, %d\n", instr->right.distance);
      } else {
        fprintf(output, "    sub rbx, %d\n", -instr->right.distance);
      }
      break;

    case OP_INC:
      if (instr->inc.offset == 0) {
        if (instr->inc.count == 1) {
          fprintf(output, "    inc byte [rbx]\n");
        } else if (instr->inc.count == -1) {
          fprintf(output, "    dec byte [rbx]\n");
        } else if (instr->inc.count > 0) {
          fprintf(output, "    add byte [rbx], %d\n", instr->inc.count);
        } else {
          fprintf(output, "    sub byte [rbx], %d\n", -instr->inc.count);
        }
      } else {
        if (instr->inc.count == 1) {
          fprintf(output, "    inc byte [rbx%+d]\n", instr->inc.offset);
        } else if (instr->inc.count == -1) {
          fprintf(output, "    dec byte [rbx%+d]\n", instr->inc.offset);
        } else if (instr->inc.count > 0) {
          fprintf(output, "    add byte [rbx%+d], %d\n", instr->inc.offset,
                  instr->inc.count);
        } else {
          fprintf(output, "    sub byte [rbx%+d], %d\n", instr->inc.offset,
                  -instr->inc.count);
        }
      }
      break;

    case OP_OUT:
      fprintf(output, "    mov rax, 1          ; write\n");
      fprintf(output, "    mov rdi, 1          ; stdout\n");
      if (instr->out.offset == 0) {
        fprintf(output, "    mov rsi, rbx        ; current cell\n");
      } else {
        fprintf(output, "    lea rsi, [rbx%+d]   ; cell at offset %d\n",
                instr->out.offset, instr->out.offset);
      }
      fprintf(output, "    mov rdx, 1          ; 1 byte\n");
      fprintf(output, "    syscall\n");
      break;

    case OP_IN:
      fprintf(output, "    mov rax, 0          ; read\n");
      fprintf(output, "    mov rdi, 0          ; stdin\n");
      if (instr->in.offset == 0) {
        fprintf(output, "    mov rsi, rbx        ; current cell\n");
      } else {
        fprintf(output, "    lea rsi, [rbx%+d]   ; cell at offset %d\n",
                instr->in.offset, instr->in.offset);
      }
      fprintf(output, "    mov rdx, 1          ; 1 byte\n");
      fprintf(output, "    syscall\n");
      break;

    case OP_LOOP:
      fprintf(output, ".loop%d_start:\n", i);
      if (instr->loop.offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->loop.offset);
      }
      fprintf(output, "    je .loop%d_end\n", i);
      break;

    case OP_END:
      if (instr->end.offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->end.offset);
      }
      fprintf(output, "    jne .loop%d_start\n", instr->end.match_addr);
      fprintf(output, ".loop%d_end:\n", instr->end.match_addr);
      break;

    case OP_SET:
      if (instr->set.count <= 1) {
        if (instr->set.offset == 0) {
          fprintf(output, "    mov byte [rbx], %d\n", instr->set.value);
        } else {
          fprintf(output, "    mov byte [rbx%+d], %d\n", instr->set.offset,
                  instr->set.value);
        }
      } else if (instr->set.stride == 0 || instr->set.stride == 1) {
        if (instr->set.offset == 0) {
          fprintf(output, "    ; memset %d cells to %d\n", instr->set.count,
                  instr->set.value);
          fprintf(output, "    mov rdi, rbx\n");
        } else {
          fprintf(output, "    lea rdi, [rbx%+d]\n", instr->set.offset);
        }
        fprintf(output, "    mov al, %d\n", instr->set.value);
        fprintf(output, "    mov rcx, %d\n", instr->set.count);
        fprintf(output, "    rep stosb\n");
      } else {
        fprintf(output, "    ; strided set: %d cells, stride %d, value %d\n",
                instr->set.count, instr->set.stride, instr->set.value);
        fprintf(output, "    mov rcx, %d\n", instr->set.count);
        if (instr->set.offset == 0) {
          fprintf(output, "    mov rdi, rbx\n");
        } else {
          fprintf(output, "    lea rdi, [rbx%+d]\n", instr->set.offset);
        }
        fprintf(output, ".strided_set_%d:\n", i);
        fprintf(output, "    mov byte [rdi], %d\n", instr->set.value);
        fprintf(output, "    add rdi, %d\n", instr->set.stride);
        fprintf(output, "    dec rcx\n");
        fprintf(output, "    jnz .strided_set_%d\n", i);
      }
      break;

    case OP_SEEK_EMPTY:
      fprintf(output, ".find_empty_%d:\n", i);
      if (instr->seek.offset == 0) {
        fprintf(output, "    cmp byte [rbx], 0\n");
      } else {
        fprintf(output, "    cmp byte [rbx%+d], 0\n", instr->seek.offset);
      }
      fprintf(output, "    je .find_empty_done_%d\n", i);
      if (instr->seek.step == 1) {
        fprintf(output, "    inc rbx\n");
      } else if (instr->seek.step == -1) {
        fprintf(output, "    dec rbx\n");
      } else if (instr->seek.step > 1) {
        fprintf(output, "    add rbx, %d\n", instr->seek.step);
      } else {
        fprintf(output, "    sub rbx, %d\n", -instr->seek.step);
      }
      fprintf(output, "    jmp .find_empty_%d\n", i);
      fprintf(output, ".find_empty_done_%d:\n", i);
      break;

    case OP_TRANSFER:
      if (instr->transfer.src_offset == 0) {
        fprintf(output, "    movzx eax, byte [rbx]\n");
      } else {
        fprintf(output, "    movzx eax, byte [rbx%+d]\n",
                instr->transfer.src_offset);
      }

      if (instr->transfer.is_assignment && instr->transfer.target_count == 1) {
        // Assignment mode
        const i32 offset = instr->transfer.targets[0].offset;
        const i32 factor = instr->transfer.targets[0].factor;
        const i32 bias = instr->transfer.targets[0].bias;

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
        for (int t = 0; t < instr->transfer.target_count; t++) {
          const i32 bias = instr->transfer.targets[t].bias;
          if (bias != 0) {
            const i32 offset = instr->transfer.targets[t].offset;
            if (bias > 0) {
              fprintf(output, "    add byte [rbx%+d], %d\n", offset, bias);
            } else {
              fprintf(output, "    sub byte [rbx%+d], %d\n", offset, -bias);
            }
          }
        }

        fprintf(output, "    test al, al\n");
        fprintf(output, "    jz .transfer_done_%d\n", i);

        for (int t = 0; t < instr->transfer.target_count; t++) {
          const i32 offset = instr->transfer.targets[t].offset;
          const i32 factor = instr->transfer.targets[t].factor;

          if (factor == 1) {
            fprintf(output, "    add byte [rbx%+d], al\n", offset);
          } else if (factor == -1) {
            fprintf(output, "    sub byte [rbx%+d], al\n", offset);
          } else if (factor > 0) {
            fprintf(output, "    mov cl, %d\n", factor);
            fprintf(output, "    imul cl\n");
            fprintf(output, "    add byte [rbx%+d], al\n", offset);
            if (t < instr->transfer.target_count - 1) {
              if (instr->transfer.src_offset == 0) {
                fprintf(output, "    movzx eax, byte [rbx]\n");
              } else {
                fprintf(output, "    movzx eax, byte [rbx%+d]\n",
                        instr->transfer.src_offset);
              }
            }
          } else {
            fprintf(output, "    mov cl, %d\n", -factor);
            fprintf(output, "    imul cl\n");
            fprintf(output, "    sub byte [rbx%+d], al\n", offset);
            if (t < instr->transfer.target_count - 1) {
              if (instr->transfer.src_offset == 0) {
                fprintf(output, "    movzx eax, byte [rbx]\n");
              } else {
                fprintf(output, "    movzx eax, byte [rbx%+d]\n",
                        instr->transfer.src_offset);
              }
            }
          }
        }

        fprintf(output, ".transfer_done_%d:\n", i);
      }
      break;

    case OP_DIV: {
      const i32 div_off = instr->div.src_offset;
      const i32 quot_off = instr->div.dst_offset;
      const i32 divisor = instr->div.divisor;
      const int shift = get_shift(divisor);

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
      const i32 div_off = instr->mod.src_offset;
      const i32 rem_off = instr->mod.dst_offset;
      const i32 divisor = instr->mod.divisor;
      const int shift = get_shift(divisor);

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
