#include <stdio.h>
#include <string.h>

#include "bft.h"

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
      program->instructions[open].loop.match_addr = i;
      program->instructions[i].end.match_addr = open;
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
      program->instructions[program->size].right.distance = 1;
      program->size++;
      break;

    case OP_LEFT:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_RIGHT;
      program->instructions[program->size].right.distance = -1;
      program->size++;
      break;

    case OP_INC:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_INC;
      program->instructions[program->size].inc.amount = 1;
      program->size++;
      break;

    case OP_DEC:
      if (program->size >= MAX_CODE_SIZE) {
        fprintf(stderr, "Error: Program too large after filtering\n");
        return STATUS_CODE_TOO_LARGE;
      }
      program->instructions[program->size].op = OP_INC;
      program->instructions[program->size].inc.amount = -1;
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
      /* These instructions don't need initialization - offsets default to 0 */
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
      i32 new_dp = machine->dp + instr->right.distance;
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
      machine->dp += instr->right.distance;
      break;

    case OP_INC:
      machine->cells[machine->dp + instr->inc.offset] += instr->inc.amount;
      break;

    case OP_OUT:
#ifndef SILENT
      putchar(machine->cells[machine->dp + instr->out.offset]);
#endif
      break;

    case OP_IN:
      machine->cells[machine->dp + instr->in.offset] = getchar();
      break;

    case OP_LOOP:
      if (machine->cells[machine->dp + instr->loop.offset] == 0) {
        machine->ip = instr->loop.match_addr;
      }
      break;

    case OP_END:
      if (machine->cells[machine->dp + instr->end.offset] != 0) {
        machine->ip = instr->end.match_addr;
      }
      break;

    case OP_SET:
      if (instr->set.count <= 1) {
        machine->cells[machine->dp + instr->set.offset] = instr->set.value;
      } else if (instr->set.stride == 0 || instr->set.stride == 1) {
        memset(&machine->cells[machine->dp + instr->set.offset],
               instr->set.value, instr->set.count);
      } else {
        for (i32 i = 0; i < instr->set.count; i++) {
          machine
              ->cells[machine->dp + instr->set.offset + i * instr->set.stride] =
              instr->set.value;
        }
      }
      break;

    case OP_SEEK_EMPTY:
      while (machine->cells[machine->dp + instr->seek.offset] != 0) {
        machine->dp += instr->seek.step;
      }
      break;

    case OP_TRANSFER: {
      cell_t *src = &machine->cells[machine->dp + instr->transfer.src_offset];
      cell_t v = *src;
      const i32 is_assignment = instr->transfer.is_assignment;
      for (i32 t = 0; t < instr->transfer.target_count; t++) {
        const i32 offset = instr->transfer.targets[t].offset;
        const i32 factor = instr->transfer.targets[t].factor;
        const i32 bias = instr->transfer.targets[t].bias;
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
      cell_t *dividend = &machine->cells[machine->dp + instr->div.src_offset];
      cell_t divisor = (cell_t)instr->div.divisor;
      cell_t *quotient = &machine->cells[machine->dp + instr->div.dst_offset];
      *quotient += *dividend / divisor;
      break;
    }

    case OP_MOD: {
      cell_t *dividend = &machine->cells[machine->dp + instr->mod.src_offset];
      cell_t divisor = (cell_t)instr->mod.divisor;
      cell_t *remainder = &machine->cells[machine->dp + instr->mod.dst_offset];
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
