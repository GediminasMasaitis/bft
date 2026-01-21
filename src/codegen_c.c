#include <stdio.h>
#include <stdlib.h>

#include "codegen.h"

static void print_c_indent(FILE *output, int level) {
  for (int i = 0; i < level; i++) {
    fprintf(output, "  ");
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

  int needs_transfer_multiple = 0;

  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];
    if (instr->op == OP_TRANSFER && instr->arg > 1) {
      needs_transfer_multiple = 1;
    }
  }

  fprintf(output, "#include <stdio.h>\n");
  fprintf(output, "#include <string.h>\n");
  fprintf(output, "\n");
  fprintf(output, "#define CELL_COUNT %d\n", CELL_COUNT);
  fprintf(output, "\n");
  fprintf(output, "static unsigned char cells[CELL_COUNT];\n");
  fprintf(output, "\n");

  if (needs_transfer_multiple) {
    fprintf(output, "static void copy_multiple(unsigned char *dp, const "
                    "int src_offset, const int count, const int dst_offsets[], "
                    "const int factors[]) {\n");
    fprintf(output, "  for (int i = 0; i < count; i++) {\n");
    fprintf(output, "    dp[dst_offsets[i]] += dp[src_offset] * factors[i];\n");
    fprintf(output, "  }\n");
    fprintf(output, "}\n");
    fprintf(output, "\n");
  }

  fprintf(output, "int main(void) {\n");
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

        if (is_assignment) {
          fprintf(output, "dp[%d] = ", instr->targets[0].offset);
          if (factor == -1) {
            fprintf(output, "-dp[%d]", instr->offset);
          } else if (factor_abs == 1) {
            fprintf(output, "dp[%d]", instr->offset);
          } else if (factor < 0) {
            fprintf(output, "-dp[%d] * %d", instr->offset, factor_abs);
          } else {
            fprintf(output, "dp[%d] * %d", instr->offset, factor);
          }
          fprintf(output, ";\n");
        } else {
          char factor_sign = factor >= 0 ? '+' : '-';
          fprintf(output, "dp[%d] %c= dp[%d]", instr->targets[0].offset,
                  factor_sign, instr->offset);
          if (factor_abs != 1) {
            fprintf(output, " * %d", factor_abs);
          }
          fprintf(output, ";\n");
        }
      } else {
        fprintf(output, "copy_multiple(dp, %d, %d, (int[]){", instr->offset,
                instr->arg);
        for (int transfer_index = 0; transfer_index < instr->arg;
             transfer_index++) {
          fprintf(output, "%d", instr->targets[transfer_index].offset);
          if (transfer_index < instr->arg - 1)
            fprintf(output, ", ");
        }
        fprintf(output, "}, (int[]){");
        for (int transfer_index = 0; transfer_index < instr->arg;
             transfer_index++) {
          fprintf(output, "%d", instr->targets[transfer_index].factor);
          if (transfer_index < instr->arg - 1)
            fprintf(output, ", ");
        }
        fprintf(output, "});\n");
      }
      break;

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
