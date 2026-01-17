#include <stdio.h>
#include <string.h>

#include "machine.h"

status_t simple_machine_step(SimpleMachine *machine) {
  op_t instruction = machine->code[machine->ip];
  cell_t *cell = &machine->cells[machine->dp];

  switch (instruction) {
  case OP_RIGHT:
    if (machine->dp + 1 >= CELL_COUNT) {
      fprintf(
          stderr,
          "Error: Data pointer out of bounds (high) at instruction %d: %c\n",
          machine->ip, machine->code[machine->ip]);
      return STATUS_OOB_HIGH;
    }
    machine->dp++;
    break;

  case OP_LEFT:
    if (machine->dp <= 0) {
      fprintf(stderr,
              "Error: Data pointer out of bounds (low) at instruction %d: %c\n",
              machine->ip, machine->code[machine->ip]);
      return STATUS_OOB_LOW;
    }
    machine->dp--;
    break;

  case OP_INC:
    (*cell)++;
    break;

  case OP_DEC:
    (*cell)--;
    break;

  case OP_OUT:
#ifndef SILENT
    putchar(*cell);
#endif
    break;

  case OP_IN:
    *cell = getchar();
    break;

  case OP_LOOP:
    if (*cell == 0) {
      machine->ip = machine->loops[machine->ip];
    }
    break;

  case OP_END:
    if (*cell != 0) {
      machine->ip = machine->loops[machine->ip];
    }
    break;

  default:
    break;
  }
  machine->ip++;
  return STATUS_OK;
}

status_t simple_machine_run(SimpleMachine *machine) {
  while (machine->ip < machine->code_size) {
    const status_t status = simple_machine_step(machine);
    if (status != STATUS_OK) {
      return status;
    }
  }
  return STATUS_OK;
}

status_t simple_machine_load(SimpleMachine *machine, const op_t *code) {
  memset(machine, 0, sizeof(*machine));

  while (machine->code_size < MAX_CODE_SIZE &&
         code[machine->code_size] != OP_DONE) {
    machine->code_size++;
  }

  if (code[machine->code_size] != OP_DONE) {
    fprintf(stderr, "Error: Code size %d exceeds maximum of %d\n",
            machine->code_size, MAX_CODE_SIZE);
    return STATUS_CODE_TOO_LARGE;
  }

  memcpy(machine->code, code, machine->code_size);

  addr_t stack[MAX_CODE_SIZE];
  addr_t stack_size = 0;

  for (addr_t i = 0; i < machine->code_size; i++) {
    if (machine->code[i] == OP_LOOP) {
      stack[stack_size] = i;
      stack_size++;
    } else if (machine->code[i] == OP_END) {
      if (stack_size == 0) {
        fprintf(stderr, "Error: Unmatched ']' at instruction %d\n", i);
        return STATUS_UNMATCHED_END;
      }
      stack_size--;
      addr_t open = stack[stack_size];
      machine->loops[open] = i;
      machine->loops[i] = open;
    }
  }

  if (stack_size != 0) {
    fprintf(stderr, "Error: Unmatched '['\n");
    return STATUS_UNMATCHED_LOOP;
  }

  return STATUS_OK;
}
