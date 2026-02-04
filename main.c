#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bft.h"

static void print_usage(const char *prog_name) {
  fprintf(stderr, "Usage: %s [options] <file.bf>\n", prog_name);
  fprintf(stderr, "\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -r, --run         Run the program (default)\n");
  fprintf(stderr, "  -c, --emit-c      Generate C code\n");
  fprintf(stderr, "  -s, --emit-asm    Generate NASM assembly\n");
  fprintf(stderr, "  -l, --emit-llvm   Generate LLVM IR\n");
  fprintf(stderr, "  -o, --output FILE Write output to FILE instead of stdout\n");
  fprintf(stderr, "  -d, --dump        Dump optimized instructions\n");
  fprintf(stderr, "  -h, --help        Show this help message\n");
}

static void dump_instructions(const Program *program) {
  for (addr_t i = 0; i < program->size; i++) {
    const Instruction *instr = &program->instructions[i];

    printf("Instruction %d: %c", i, instr->op);

    switch (instr->op) {
    case OP_RIGHT:
      printf(" %d", instr->right.distance);
      break;
    case OP_INC:
      printf(" %d @%d", instr->inc.count, instr->inc.offset);
      break;
    case OP_SET:
      printf(" %d %d", instr->set.value, instr->set.count);
      if (instr->set.offset != 0) {
        printf(" @%d", instr->set.offset);
      }
      if (instr->set.count > 1 && instr->set.stride > 1) {
        printf(" stride=%d", instr->set.stride);
      }
      break;
    case OP_SEEK_EMPTY:
      printf(" %d", instr->seek.step);
      if (instr->seek.offset != 0) {
        printf(" @%d", instr->seek.offset);
      }
      break;
    case OP_LOOP:
      printf(" %d", instr->loop.match_addr);
      if (instr->loop.offset != 0) {
        printf(" @%d", instr->loop.offset);
      }
      break;
    case OP_END:
      printf(" %d", instr->end.match_addr);
      if (instr->end.offset != 0) {
        printf(" @%d", instr->end.offset);
      }
      break;
    case OP_OUT:
      if (instr->out.offset != 0) {
        printf(" @%d", instr->out.offset);
      }
      break;
    case OP_IN:
      if (instr->in.offset != 0) {
        printf(" @%d", instr->in.offset);
      }
      break;
    case OP_TRANSFER:
      printf(" %d %d", instr->transfer.target_count,
             instr->transfer.is_assignment);
      if (instr->transfer.src_offset != 0) {
        printf(" @%d", instr->transfer.src_offset);
      }
      for (int t = 0; t < instr->transfer.target_count; t++) {
        printf(" (off=%d, fac=%d", instr->transfer.targets[t].offset,
               instr->transfer.targets[t].factor);
        if (instr->transfer.targets[t].bias != 0) {
          printf(", bias=%d", instr->transfer.targets[t].bias);
        }
        printf(")");
      }
      break;
    case OP_DIV:
      printf(" %d @%d", instr->div.divisor, instr->div.src_offset);
      printf(" ; div: dp[%d] += dp[%d] / %d", instr->div.dst_offset,
             instr->div.src_offset, instr->div.divisor);
      break;
    case OP_MOD:
      printf(" %d @%d", instr->mod.divisor, instr->mod.src_offset);
      printf(" ; mod: dp[%d] = dp[%d] %% %d", instr->mod.dst_offset,
             instr->mod.src_offset, instr->mod.divisor);
      break;
    default:
      printf(" (unknown)");
      break;
    }
    putchar('\n');
  }
  putchar('\n');
}

enum { MODE_RUN, MODE_EMIT_C, MODE_EMIT_ASM, MODE_EMIT_LLVM };

int main(int argc, char **argv) {
  int mode = MODE_RUN;
  const char *output_path = NULL;
  int dump = 0;
  status_t status = STATUS_OK;

  static const struct option long_options[] = {
      {"run", no_argument, 0, 'r'},
      {"emit-c", no_argument, 0, 'c'},
      {"emit-asm", no_argument, 0, 's'},
      {"emit-llvm", no_argument, 0, 'l'},
      {"output", required_argument, 0, 'o'},
      {"dump", no_argument, 0, 'd'},
      {"help", no_argument, 0, 'h'},
      {0, 0, 0, 0}};

  int opt;
  while ((opt = getopt_long(argc, argv, "rcslo:dh", long_options, NULL)) != -1) {
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
    case 'l':
      mode = MODE_EMIT_LLVM;
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

  op_t *code = calloc(MAX_CODE_SIZE, sizeof(op_t));
  Machine *machine = calloc(1, sizeof(Machine));

  if (!code || !machine) {
    fprintf(stderr, "Error: Out of memory\n");
    status = 1;
    goto cleanup;
  }

  FILE *file = fopen(input_path, "r");
  if (!file) {
    fprintf(stderr, "Error: Could not open file %s\n", input_path);
    status = 1;
    goto cleanup;
  }
  size_t read_size = fread(code, 1, MAX_CODE_SIZE - 1, file);
  if (read_size == 0 && ferror(file)) {
    fprintf(stderr, "Error: Failed to read file %s\n", input_path);
    fclose(file);
    status = 1;
    goto cleanup;
  }
  fclose(file);

  status = program_parse(&machine->program, code);
  if (status != STATUS_OK) {
    goto cleanup;
  }

  optimize_program(&machine->program);

  if (dump) {
    dump_instructions(&machine->program);
  }

  FILE *output = stdout;
  if (output_path) {
    output = fopen(output_path, "w");
    if (!output) {
      fprintf(stderr, "Error: Could not open %s for writing\n", output_path);
      status = 1;
      goto cleanup;
    }
  }

  switch (mode) {
  case MODE_RUN:
    status = machine_run(machine);
    break;
  case MODE_EMIT_C:
    codegen_c(&machine->program, output);
    break;
  case MODE_EMIT_ASM:
    codegen_nasm(&machine->program, output);
    break;
  case MODE_EMIT_LLVM:
    codegen_llvm(&machine->program, output);
    break;
  }

  if (output_path && output != stdout) {
    fclose(output);
  }

cleanup:
  free(code);
  free(machine);
  return status;
}
