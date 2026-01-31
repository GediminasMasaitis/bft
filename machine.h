#pragma once

#include "types.h"

typedef struct {
  op_t code[MAX_CODE_SIZE];
  addr_t code_size;

  cell_t cells[CELL_COUNT];
  addr_t loops[MAX_CODE_SIZE];
  addr_t ip;
  addr_t dp;
} SimpleMachine;

typedef struct {
  Program program;

  cell_t cells[CELL_COUNT];
  addr_t ip;
  addr_t dp;
} Machine;

status_t simple_machine_load(SimpleMachine *machine, const op_t *code);
status_t simple_machine_step(SimpleMachine *machine);
status_t simple_machine_run(SimpleMachine *machine);

status_t simple_machine_to_program(Program *program,
                                   const SimpleMachine *machine);
status_t machine_run(Machine *machine);
status_t program_calculate_loops(Program *program);