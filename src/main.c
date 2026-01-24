#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "machine.h"
#include "optimizer.h"
#include "types.h"

static void print_usage(const char *prog_name) {
  fprintf(stderr, "Usage: %s [options] <file.bf>\n", prog_name);
  fprintf(stderr, "\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -r, --run         Run the program (default)\n");
  fprintf(stderr, "  -c, --emit-c      Generate C code\n");
  fprintf(stderr, "  -s, --emit-asm    Generate NASM assembly\n");
  fprintf(stderr,
          "  -o, --output FILE Write output to FILE instead of stdout\n");
  fprintf(stderr, "  -d, --dump        Dump optimized instructions\n");
  fprintf(stderr, "  -h, --help        Show this help message\n");
}

static void dump_instructions(const Program *program) {
  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];

    printf("Instruction %d: %c %d %d", i, instr->op, instr->arg, instr->arg2);
    if (instr->offset != 0) {
      printf(" @%d", instr->offset);
    }
    if (instr->op == OP_TRANSFER) {
      for (int t = 0; t < instr->arg; t++) {
        printf(" (off=%d, fac=%d", instr->targets[t].offset,
               instr->targets[t].factor);
        if (instr->targets[t].bias != 0) {
          printf(", bias=%d", instr->targets[t].bias);
        }
        printf(")");
      }
    } else if (instr->op == OP_DIV) {
      printf(" ; div: dp[%d] += dp[%d] / %d", instr->targets[0].offset,
             instr->offset, instr->arg);
    } else if (instr->op == OP_MOD) {
      printf(" ; mod: dp[%d] = dp[%d] %% %d", instr->targets[0].offset,
             instr->offset, instr->arg);
    }
    putchar('\n');
  }
  putchar('\n');
}

enum { MODE_RUN, MODE_EMIT_C, MODE_EMIT_ASM } mode;

int main(int argc, char **argv) {
  mode = MODE_RUN;
  const char *output_path = NULL;
  int dump = 0;

  static struct option long_options[] = {{"run", no_argument, 0, 'r'},
                                         {"emit-c", no_argument, 0, 'c'},
                                         {"emit-asm", no_argument, 0, 's'},
                                         {"output", required_argument, 0, 'o'},
                                         {"dump", no_argument, 0, 'd'},
                                         {"help", no_argument, 0, 'h'},
                                         {0, 0, 0, 0}};

  int opt;
  while ((opt = getopt_long(argc, argv, "rcso:dh", long_options, NULL)) != -1) {
    switch (opt) {
    case 'r':
      mode = MODE_RUN;
      break;
    case 'c':
      mode = MODE_EMIT_C;
      break;
    case 's':
      mode = MODE_EMIT_ASM;
      break;
    case 'o':
      output_path = optarg;
      break;
    case 'd':
      dump = 1;
      break;
    case 'h':
      print_usage(argv[0]);
      return 0;
    default:
      print_usage(argv[0]);
      return 1;
    }
  }

  if (optind >= argc) {
    fprintf(stderr, "Error: No input file specified\n");
    print_usage(argv[0]);
    return 1;
  }

  const char *input_path = argv[optind];

  op_t code[MAX_CODE_SIZE] = {0};
  FILE *file = fopen(input_path, "r");
  if (!file) {
    fprintf(stderr, "Error: Could not open file %s\n", input_path);
    return 1;
  }
  size_t read_size = fread((void *)code, 1, MAX_CODE_SIZE - 1, file);
  (void)read_size;
  fclose(file);

  SimpleMachine simple_machine;
  const status_t simple_load_status =
      simple_machine_load(&simple_machine, code);
  if (simple_load_status != STATUS_OK) {
    return simple_load_status;
  }

  Machine machine = {0};
  const status_t load_status =
      simple_machine_to_program(&machine.program, &simple_machine);
  if (load_status != STATUS_OK) {
    return load_status;
  }

  optimize_program(&machine.program);

  if (dump) {
    dump_instructions(&machine.program);
  }

  FILE *output = stdout;
  if (output_path) {
    output = fopen(output_path, "w");
    if (!output) {
      fprintf(stderr, "Error: Could not open %s for writing\n", output_path);
      return 1;
    }
  }

  status_t status = STATUS_OK;
  switch (mode) {
  case MODE_RUN:
    status = machine_run(&machine);
    break;
  case MODE_EMIT_C:
    codegen_c(&machine.program, output);
    break;
  case MODE_EMIT_ASM:
    codegen_nasm(&machine.program, output);
    break;
  }

  if (output_path && output != stdout) {
    fclose(output);
  }

  return status;
}
