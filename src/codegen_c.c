#include <stdio.h>
#include <stdlib.h>

#include "codegen.h"

static void print_c_indent(FILE *output, int level) {
  for (int i = 0; i < level; i++) {
    fprintf(output, "  ");
  }
}

static int get_shift(int n) {
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
  int factor_abs = abs(factor);
  int shift = get_shift(factor_abs);
  
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
      addr_t end_idx = instr->arg;
      if (end_idx > 0 && program->instructions[end_idx - 1].op == OP_RIGHT) {
        skip[end_idx - 1] = 1;
        if (i > 0 && program->instructions[i - 1].op == OP_SET &&
            program->instructions[i - 1].arg2 == 1 &&
            program->instructions[i - 1].offset == 0) {
          skip[i - 1] = 1;
        }
      }
    }
  }

  fprintf(output, "#include <stdio.h>\n");
  fprintf(output, "#include <string.h>\n");
  fprintf(output, "\n");

  fprintf(output, "int main(void) {\n");
  fprintf(output, "  unsigned char cells[%d] = {0};\n", CELL_COUNT);
  fprintf(output, "  unsigned char *dp = cells;\n");
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
      if (instr->arg == 1) {
        fprintf(output, "dp++;\n");
      } else if (instr->arg == -1) {
        fprintf(output, "dp--;\n");
      } else if (instr->arg > 0) {
        fprintf(output, "dp += %d;\n", instr->arg);
      } else {
        fprintf(output, "dp -= %d;\n", -instr->arg);
      }
      break;

    case OP_INC:
      print_c_indent(output, indent_level);
      if (instr->offset == 0) {
        if (instr->arg == 1) {
          fprintf(output, "(*dp)++;\n");
        } else if (instr->arg == -1) {
          fprintf(output, "(*dp)--;\n");
        } else if (instr->arg > 0) {
          fprintf(output, "*dp += %d;\n", instr->arg);
        } else {
          fprintf(output, "*dp -= %d;\n", -instr->arg);
        }
      } else {
        if (instr->arg == 1) {
          fprintf(output, "dp[%d]++;\n", instr->offset);
        } else if (instr->arg == -1) {
          fprintf(output, "dp[%d]--;\n", instr->offset);
        } else if (instr->arg > 0) {
          fprintf(output, "dp[%d] += %d;\n", instr->offset, instr->arg);
        } else {
          fprintf(output, "dp[%d] -= %d;\n", instr->offset, -instr->arg);
        }
      }
      break;

    case OP_OUT:
      print_c_indent(output, indent_level);
      if (instr->offset == 0) {
        fprintf(output, "putchar(*dp);\n");
      } else {
        fprintf(output, "putchar(dp[%d]);\n", instr->offset);
      }
      break;

    case OP_IN:
      print_c_indent(output, indent_level);
      if (instr->offset == 0) {
        fprintf(output, "*dp = getchar();\n");
      } else {
        fprintf(output, "dp[%d] = getchar();\n", instr->offset);
      }
      break;

    case OP_LOOP:
      print_c_indent(output, indent_level);
      {
        addr_t end_idx = instr->arg;
        if (end_idx > 0 && skip[end_idx - 1] && instr->offset == 0) {
          const Instruction *right_instr = &program->instructions[end_idx - 1];
          if (i > 0 && skip[i - 1] &&
              program->instructions[i - 1].offset == 0) {
            const Instruction *set_instr = &program->instructions[i - 1];
            fprintf(output, "for (*dp = %d; *dp != 0; ", set_instr->arg);
          } else {
            fprintf(output, "for (; *dp != 0; ");
          }
          if (right_instr->arg == 1) {
            fprintf(output, "dp++");
          } else if (right_instr->arg == -1) {
            fprintf(output, "dp--");
          } else if (right_instr->arg > 0) {
            fprintf(output, "dp += %d", right_instr->arg);
          } else {
            fprintf(output, "dp -= %d", -right_instr->arg);
          }
          fprintf(output, ") {\n");
        } else if (instr->offset == 0) {
          fprintf(output, "while (*dp != 0) {\n");
        } else {
          fprintf(output, "while (dp[%d] != 0) {\n", instr->offset);
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
      if (instr->offset == 0) {
        if (instr->arg2 <= 1) {
          fprintf(output, "*dp = %d;\n", instr->arg);
        } else {
          fprintf(output, "memset(dp, %d, %d);\n", instr->arg, instr->arg2);
        }
      } else {
        const char sign = instr->offset >= 0 ? '+' : '-';
        const i32 offset_abs =
            instr->offset >= 0 ? instr->offset : -instr->offset;
        if (instr->arg2 <= 1) {
          fprintf(output, "dp[%d] = %d;\n", instr->offset, instr->arg);
        } else {
          fprintf(output, "memset(dp %c %d, %d, %d);\n", sign, offset_abs,
                  instr->arg, instr->arg2);
        }
      }
      break;

    case OP_SEEK_EMPTY:
      print_c_indent(output, indent_level);
      char sign = instr->arg >= 0 ? '+' : '-';
      int step_abs = abs(instr->arg);
      if (instr->offset == 0) {
        fprintf(output, "while (*dp != 0) dp %c= %d; // seek empty\n", sign,
                step_abs);
      } else {
        fprintf(output, "while (dp[%d] != 0) dp %c= %d; // seek empty\n",
                instr->offset, sign, step_abs);
      }
      break;

    case OP_TRANSFER:
      print_c_indent(output, indent_level);
      if (instr->arg == 1) {
        int is_assignment = instr->arg2 == 1;
        int factor = instr->targets[0].factor;
        int factor_abs = abs(factor);
        int bias = instr->targets[0].bias;
        int shift = get_shift(factor_abs);

        char src_operand[32];
        snprintf(src_operand, sizeof(src_operand), "dp[%d]", instr->offset);

        if (is_assignment) {
          fprintf(output, "dp[%d] = ", instr->targets[0].offset);
          if (bias != 0) {
            fprintf(output, "%d + ", bias);
          }
          print_multiply_expr(output, src_operand, factor);
          fprintf(output, ";\n");
        } else {
          if (bias != 0) {
            fprintf(output, "dp[%d] += ", instr->targets[0].offset);
            print_multiply_expr(output, src_operand, factor);
            if (bias > 0) {
              fprintf(output, " + %d", bias);
            } else {
              fprintf(output, " - %d", -bias);
            }
          } else {
            if (shift > 0) {
              char op = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s << %d", 
                      instr->targets[0].offset, op, src_operand, shift);
            } else if (factor_abs == 1) {
              char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s", instr->targets[0].offset,
                      factor_sign, src_operand);
            } else {
              char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s * %d", instr->targets[0].offset,
                      factor_sign, src_operand, factor_abs);
            }
          }
          fprintf(output, ";\n");
        }
      } else {
        for (int t = 0; t < instr->arg; t++) {
          if (t > 0) {
            print_c_indent(output, indent_level);
          }

          int offset = instr->targets[t].offset;
          int factor = instr->targets[t].factor;
          int bias = instr->targets[t].bias;
          int factor_abs = abs(factor);
          int shift = get_shift(factor_abs);

          char src_operand[32];
          snprintf(src_operand, sizeof(src_operand), "dp[%d]", instr->offset);

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
              char op = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s << %d", offset, op, src_operand, shift);
            } else if (factor_abs == 1) {
              char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s", offset, factor_sign, src_operand);
            } else {
              char factor_sign = factor >= 0 ? '+' : '-';
              fprintf(output, "dp[%d] %c= %s * %d", offset, factor_sign, src_operand, factor_abs);
            }
          }
          fprintf(output, ";\n");
        }
      }
      break;

    case OP_DIV: {
      i32 div_off = instr->offset;
      i32 quot_off = instr->targets[0].offset;
      i32 divisor = instr->arg;
      int shift = get_shift(divisor);

      print_c_indent(output, indent_level);
      if (shift > 0) {
        fprintf(output, "dp[%d] += dp[%d] >> %d;\n", quot_off, div_off, shift);
      } else {
        fprintf(output, "dp[%d] += dp[%d] / %d;\n", quot_off, div_off, divisor);
      }
      break;
    }

    case OP_MOD: {
      i32 div_off = instr->offset;
      i32 rem_off = instr->targets[0].offset;
      i32 divisor = instr->arg;
      int shift = get_shift(divisor);

      print_c_indent(output, indent_level);
      if (shift > 0) {
        fprintf(output, "dp[%d] = dp[%d] & %d;\n", rem_off, div_off, divisor - 1);
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
