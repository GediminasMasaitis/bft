#include <stdio.h>
#include <stdlib.h>

#include "bft.h"

static void print_c_indent(FILE *output, int level) {
  for (int i = 0; i < level; i++) {
    fprintf(output, "  ");
  }
}

static const i32 use_shift_and_mask = 0;
static const int inc_always_use_offset = 1;
static const int set_always_use_offset = 1;

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

static void print_multiply_expr(FILE *output, const char *operand, int factor) {
  const int factor_abs = abs(factor);
  const int shift = get_shift(factor_abs);

  if (factor < 0) {
    fprintf(output, "-");
  }

  if (shift > 0) {
    fprintf(output, "(%s << %d)", operand, shift);
  } else if (factor_abs == 1) {
    fprintf(output, "%s", operand);
  } else {
    fprintf(output, "%s * %d", operand, factor_abs);
  }
}

void codegen_c(const Program *program, FILE *output) {
  int *skip = calloc(program->size, sizeof(int));
  if (!skip) {
    fprintf(stderr, "Error: Could not allocate memory for skip array\n");
    return;
  }

  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];
    if (instr->op == OP_LOOP) {
      const addr_t end_idx = instr->loop.match_addr;
      if (end_idx > 0 && program->instructions[end_idx - 1].op == OP_RIGHT) {
        skip[end_idx - 1] = 1;
        if (i > 0 && program->instructions[i - 1].op == OP_SET &&
            program->instructions[i - 1].set.count == 1 &&
            program->instructions[i - 1].set.offset == 0) {
          skip[i - 1] = 1;
        }
      }
    }
  }

  fprintf(output, "#include <stdio.h>\n");
  fprintf(output, "#include <string.h>\n");
  fprintf(output, "\n");

  fprintf(output, "int main(void) {\n");
  fprintf(output, "  unsigned char cells[%d] = {0};\n", CELL_COUNT * 2);
  fprintf(output, "  unsigned char *dp = cells + %d;\n", CELL_COUNT);
  fprintf(output, "\n");

  int indent_level = 1;

  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];

    if (skip[i]) {
      continue;
    }

    switch (instr->op) {
    case OP_RIGHT:
      print_c_indent(output, indent_level);
      if (instr->right.distance == 1) {
        fprintf(output, "dp++;\n");
      } else if (instr->right.distance == -1) {
        fprintf(output, "dp--;\n");
      } else if (instr->right.distance > 0) {
        fprintf(output, "dp += %d;\n", instr->right.distance);
      } else {
        fprintf(output, "dp -= %d;\n", -instr->right.distance);
      }
      break;

    case OP_INC:
      print_c_indent(output, indent_level);
      if (!inc_always_use_offset && instr->inc.offset == 0) {
        if (instr->inc.count == 1) {
          fprintf(output, "(*dp)++;\n");
        } else if (instr->inc.count == -1) {
          fprintf(output, "(*dp)--;\n");
        } else if (instr->inc.count > 0) {
          fprintf(output, "*dp += %d;\n", instr->inc.count);
        } else {
          fprintf(output, "*dp -= %d;\n", -instr->inc.count);
        }
      } else {
        if (instr->inc.count == 1) {
          fprintf(output, "dp[%d]++;\n", instr->inc.offset);
        } else if (instr->inc.count == -1) {
          fprintf(output, "dp[%d]--;\n", instr->inc.offset);
        } else if (instr->inc.count > 0) {
          fprintf(output, "dp[%d] += %d;\n", instr->inc.offset,
                  instr->inc.count);
        } else {
          fprintf(output, "dp[%d] -= %d;\n", instr->inc.offset,
                  -instr->inc.count);
        }
      }
      break;

    case OP_OUT:
      print_c_indent(output, indent_level);
      if (instr->out.offset == 0) {
        fprintf(output, "putchar(*dp);\n");
      } else {
        fprintf(output, "putchar(dp[%d]);\n", instr->out.offset);
      }
      break;

    case OP_IN:
      print_c_indent(output, indent_level);
      if (instr->in.offset == 0) {
        fprintf(output, "*dp = getchar();\n");
      } else {
        fprintf(output, "dp[%d] = getchar();\n", instr->in.offset);
      }
      break;

    case OP_LOOP:
      print_c_indent(output, indent_level);
      {
        const addr_t end_idx = instr->loop.match_addr;
        if (end_idx > 0 && skip[end_idx - 1] && instr->loop.offset == 0) {
          const Instruction *right_instr = &program->instructions[end_idx - 1];
          if (i > 0 && skip[i - 1] &&
              program->instructions[i - 1].set.offset == 0) {
            const Instruction *set_instr = &program->instructions[i - 1];
            fprintf(output, "for (*dp = %d; *dp != 0; ", set_instr->set.value);
          } else {
            fprintf(output, "for (; *dp != 0; ");
          }
          if (right_instr->right.distance == 1) {
            fprintf(output, "dp++");
          } else if (right_instr->right.distance == -1) {
            fprintf(output, "dp--");
          } else if (right_instr->right.distance > 0) {
            fprintf(output, "dp += %d", right_instr->right.distance);
          } else {
            fprintf(output, "dp -= %d", -right_instr->right.distance);
          }
          fprintf(output, ") {\n");
        } else if (instr->loop.offset == 0) {
          fprintf(output, "while (*dp != 0) {\n");
        } else {
          fprintf(output, "while (dp[%d] != 0) {\n", instr->loop.offset);
        }
      }
      indent_level++;
      break;

    case OP_END:
      indent_level--;
      print_c_indent(output, indent_level);
      fprintf(output, "}\n");
      break;

    case OP_SET:
      print_c_indent(output, indent_level);
      if (instr->set.count <= 1) {
        if (!set_always_use_offset && instr->set.offset == 0) {
          fprintf(output, "*dp = %d;\n", instr->set.value);
        } else {
          fprintf(output, "dp[%d] = %d;\n", instr->set.offset,
                  instr->set.value);
        }
      } else if (instr->set.stride == 0 || instr->set.stride == 1) {
        if (!set_always_use_offset && instr->set.offset == 0) {
          fprintf(output, "memset(dp, %d, %d);\n", instr->set.value,
                  instr->set.count);
        } else {
          const char sign = instr->set.offset >= 0 ? '+' : '-';
          const i32 offset_abs =
              instr->set.offset >= 0 ? instr->set.offset : -instr->set.offset;
          fprintf(output, "memset(dp %c %d, %d, %d);\n", sign, offset_abs,
                  instr->set.value, instr->set.count);
        }
      } else {
        fprintf(output, "for (int i = 0; i < %d; i++) dp[%d + i * %d] = %d;\n",
                instr->set.count, instr->set.offset, instr->set.stride,
                instr->set.value);
      }
      break;

    case OP_SEEK_EMPTY:
      print_c_indent(output, indent_level);
      const char sign = instr->seek.step >= 0 ? '+' : '-';
      const int step_abs = abs(instr->seek.step);
      if (instr->seek.offset == 0) {
        fprintf(output, "while (*dp != 0) dp %c= %d; // seek empty\n", sign,
                step_abs);
      } else {
        fprintf(output, "while (dp[%d] != 0) dp %c= %d; // seek empty\n",
                instr->seek.offset, sign, step_abs);
      }
      break;

    case OP_TRANSFER:
      print_c_indent(output, indent_level);
      if (instr->transfer.target_count == 1) {
        const int is_assignment = instr->transfer.is_assignment;
        const int factor = instr->transfer.targets[0].factor;
        const int factor_abs = abs(factor);
        const int bias = instr->transfer.targets[0].bias;
        const int shift = get_shift(factor_abs);

        char src_operand[32];
        snprintf(src_operand, sizeof(src_operand), "dp[%d]",
                 instr->transfer.src_offset);

        if (is_assignment) {
          fprintf(output, "dp[%d] = ", instr->transfer.targets[0].offset);
          if (bias != 0) {
            fprintf(output, "%d + ", bias);
          }
          print_multiply_expr(output, src_operand, factor);
          fprintf(output, ";\n");
        } else {
          if (bias != 0) {
            fprintf(output, "dp[%d] += ", instr->transfer.targets[0].offset);
            print_multiply_expr(output, src_operand, factor);
            if (bias > 0) {
              fprintf(output, " + %d", bias);
            } else {
              fprintf(output, " - %d", -bias);
            }
          } else {
            if (shift > 0) {
              const char op = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s << %d",
                      instr->transfer.targets[0].offset, op, src_operand,
                      shift);
            } else if (factor_abs == 1) {
              const char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s",
                      instr->transfer.targets[0].offset, factor_sign,
                      src_operand);
            } else {
              const char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s * %d",
                      instr->transfer.targets[0].offset, factor_sign,
                      src_operand, factor_abs);
            }
          }
          fprintf(output, ";\n");
        }
      } else {
        for (int t = 0; t < instr->transfer.target_count; t++) {
          if (t > 0) {
            print_c_indent(output, indent_level);
          }

          const int offset = instr->transfer.targets[t].offset;
          const int factor = instr->transfer.targets[t].factor;
          const int bias = instr->transfer.targets[t].bias;
          const int factor_abs = abs(factor);
          const int shift = get_shift(factor_abs);

          char src_operand[32];
          snprintf(src_operand, sizeof(src_operand), "dp[%d]",
                   instr->transfer.src_offset);

          if (bias != 0) {
            fprintf(output, "dp[%d] += ", offset);
            print_multiply_expr(output, src_operand, factor);
            if (bias > 0) {
              fprintf(output, " + %d", bias);
            } else {
              fprintf(output, " - %d", -bias);
            }
          } else {
            if (shift > 0) {
              const char op = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s << %d", offset, op, src_operand,
                      shift);
            } else if (factor_abs == 1) {
              const char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s", offset, factor_sign,
                      src_operand);
            } else {
              const char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s * %d", offset, factor_sign,
                      src_operand, factor_abs);
            }
          }
          fprintf(output, ";\n");
        }
      }
      break;

    case OP_DIV: {
      const i32 div_off = instr->div.src_offset;
      const i32 quot_off = instr->div.dst_offset;
      const i32 divisor = instr->div.divisor;
      const int shift = get_shift(divisor);

      print_c_indent(output, indent_level);
      if (shift > 0) {
        fprintf(output, "dp[%d] += dp[%d] >> %d;\n", quot_off, div_off, shift);
      } else {
        fprintf(output, "dp[%d] += dp[%d] / %d;\n", quot_off, div_off, divisor);
      }
      break;
    }

    case OP_MOD: {
      const i32 div_off = instr->mod.src_offset;
      const i32 rem_off = instr->mod.dst_offset;
      const i32 divisor = instr->mod.divisor;
      const int shift = get_shift(divisor);

      print_c_indent(output, indent_level);
      if (shift > 0) {
        fprintf(output, "dp[%d] = dp[%d] & %d;\n", rem_off, div_off,
                divisor - 1);
      } else {
        fprintf(output, "dp[%d] = dp[%d] %% %d;\n", rem_off, div_off, divisor);
      }
      break;
    }

    default:
      print_c_indent(output, indent_level);
      fprintf(output, "/* unknown op: %c */\n", instr->op);
      break;
    }
  }

  fprintf(output, "\n");
  fprintf(output, "  return 0;\n");
  fprintf(output, "}\n");

  free(skip);
}
