#include <stdio.h>
#include <string.h>

#include "machine.h"

status_t program_calculate_loops(Program *program) {
  addr_t stack[MAX_CODE_SIZE];
  addr_t stack_size = 0;

  for (addr_t i = 0; i < program->size; i++) {
    if (program->instructions[i].op == OP_LOOP) {
      stack[stack_size] = i;
      stack_size++;
    } else if (program->instructions[i].op == OP_END) {
      if (stack_size == 0) {
        fprintf(stderr, "Error: Unmatched ']' at instruction %d\n", i);
        return STATUS_UNMATCHED_END;
      }
      stack_size--;
      addr_t open = stack[stack_size];
      program->instructions[open].arg = i;
      program->instructions[i].arg = open;
    }
  }

  if (stack_size != 0) {
    fprintf(stderr, "Error: Unmatched '['\n");
    return STATUS_UNMATCHED_LOOP;
  }

  return STATUS_OK;
}

status_t simple_machine_to_program(Program *program,
                                   const SimpleMachine *machine) {
  memset(program, 0, sizeof(*program));

  for (addr_t i = 0; i < machine->code_size; i++) {
    op_t op = machine->code[i];

    switch (op) {
    case OP_RIGHT:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_RIGHT;
      program->instructions[program->size].arg = 1;
      program->size++;
      break;

    case OP_LEFT:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_RIGHT;
      program->instructions[program->size].arg = -1;
      program->size++;
      break;

    case OP_INC:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_INC;
      program->instructions[program->size].arg = 1;
      program->size++;
      break;

    case OP_DEC:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_INC;
      program->instructions[program->size].arg = -1;
      program->size++;
      break;

    case OP_OUT:
    case OP_IN:
    case OP_LOOP:
    case OP_END:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = op;
      program->instructions[program->size].arg = 1;
      program->size++;
      break;

    default:
      // skip comments
      break;
    }
  }

  return program_calculate_loops(program);
}

status_t machine_run(Machine *machine) {
  while (machine->ip < machine->program.size) {
    Instruction *instr = &machine->program.instructions[machine->ip];
    cell_t *cell = &machine->cells[machine->dp];

    switch (instr->op) {
    case OP_RIGHT:
#ifndef UNSAFE
    {
      i32 new_dp = machine->dp + instr->arg;
      if (new_dp < 0) {
        fprintf(
            stderr,
            "Error: Data pointer out of bounds (low) at instruction %d: %c\n",
            machine->ip, instr->op);
        return STATUS_OOB_LOW;
      }
      if (new_dp >= CELL_COUNT) {
        fprintf(
            stderr,
            "Error: Data pointer out of bounds (high) at instruction %d: %c\n",
            machine->ip, instr->op);
        return STATUS_OOB_HIGH;
      }
    }
#endif
      machine->dp += instr->arg;
      break;

    case OP_INC:
      machine->cells[machine->dp + instr->offset] += instr->arg;
      break;

    case OP_OUT:
#ifndef SILENT
      putchar(machine->cells[machine->dp + instr->offset]);
#endif
      break;

    case OP_IN:
      machine->cells[machine->dp + instr->offset] = getchar();
      break;

    case OP_LOOP:
      if (machine->cells[machine->dp + instr->offset] == 0) {
        machine->ip = instr->arg;
      }
      break;

    case OP_END:
      if (machine->cells[machine->dp + instr->offset] != 0) {
        machine->ip = instr->arg;
      }
      break;

    case OP_SET:
      if (instr->arg2 <= 1) {
        machine->cells[machine->dp + instr->offset] = instr->arg;
      } else {
        memset(&machine->cells[machine->dp + instr->offset], instr->arg,
               instr->arg2);
      }
      break;

    case OP_SEEK_EMPTY:
      while (machine->cells[machine->dp + instr->offset] != 0) {
        machine->dp += instr->arg;
      }
      break;

    case OP_TRANSFER: {
      cell_t *src = &machine->cells[machine->dp + instr->offset];
      cell_t v = *src;
      const i32 is_assignment = (instr->arg2 == 1);
      for (i32 t = 0; t < instr->arg; t++) {
        const i32 offset = instr->targets[t].offset;
        const i32 factor = instr->targets[t].factor;
        const i32 bias = instr->targets[t].bias;
        cell_t *dst = &machine->cells[machine->dp + offset];
        if (is_assignment) {
          *dst = (cell_t)(bias + (cell_t)(v * (cell_t)factor));
        } else {
          *dst = (cell_t)(*dst + bias + (cell_t)(v * (cell_t)factor));
        }
      }
      break;
    }

    case OP_DIV: {
      // dp[targets[0].offset] += dp[offset] / arg
      cell_t *dividend = &machine->cells[machine->dp + instr->offset];
      cell_t divisor = (cell_t)instr->arg;
      cell_t *quotient =
          &machine->cells[machine->dp + instr->targets[0].offset];
      *quotient += *dividend / divisor;
      break;
    }

    case OP_MOD: {
      // dp[targets[0].offset] = dp[offset] % arg
      cell_t *dividend = &machine->cells[machine->dp + instr->offset];
      cell_t divisor = (cell_t)instr->arg;
      cell_t *remainder =
          &machine->cells[machine->dp + instr->targets[0].offset];
      *remainder = *dividend % divisor;
      break;
    }

    default:
      break;
    }
    machine->ip++;
  }
  return STATUS_OK;
}
