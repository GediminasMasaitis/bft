#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "machine.h"
#include "optimizer.h"

void merge_consecutive_right_inc(Program *output, const Program *input,
                                 const op_t op) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];
    if (instr.op == op) {
      i32 count = instr.arg;
      i32 offset = instr.offset;
      while (in_index + 1 < input->size) {
        const Instruction *next = &input->instructions[in_index + 1];

        if (next->op != instr.op || next->offset != offset) {
          break;
        }

        count += next->arg;
        in_index++;
      }

      output->instructions[out_index].op = instr.op;
      output->instructions[out_index].arg = count;
      output->instructions[out_index].offset = offset;
      out_index++;
    } else {
      output->instructions[out_index] = instr;
      out_index++;
    }
  }

  output->size = out_index;
  program_calculate_loops(output);
}

// Check for pattern [-] or [+]:
//
// Scan for:
// LOOP
// INC 1 or INC -1
// END
//
// Replace with:
// SET 0 1
void create_zeroing_sets(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];
    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_INC &&
        (input->instructions[in_index + 1].arg == 1 ||
         input->instructions[in_index + 1].arg == -1) &&
        input->instructions[in_index + 2].op == OP_END) {
      output->instructions[out_index].op = OP_SET;
      output->instructions[out_index].arg = 0;  // value
      output->instructions[out_index].arg2 = 1; // count
      in_index += 2;
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_memset(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    if (instr.op == OP_SET) {
      const i32 in_set_val = instr.arg;
      i32 count = 1;

      addr_t j = in_index;
      while (j + 2 < input->size) {
        const Instruction *right = &input->instructions[j + 1];
        const Instruction *next_set = &input->instructions[j + 2];

        if (right->op == OP_RIGHT && right->arg == 1 &&
            next_set->op == OP_SET && next_set->arg == in_set_val) {
          count++;
          j += 2; // Move past RIGHT and SET
        } else {
          break;
        }
      }

      if (count >= 2) {
        // Emit the memset SET with count
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = in_set_val;
        output->instructions[out_index].arg2 = count;
        out_index++;

        // Emit the pointer movement: we set 'count' cells, so move count-1 to
        // end at last cell
        output->instructions[out_index].op = OP_RIGHT;
        output->instructions[out_index].arg = count - 1;
        out_index++;

        // Skip past all the instructions we consumed
        in_index = j;
        continue;
      }
    }

    output->instructions[out_index] = instr;
    out_index++;
  }
  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_seek_empty(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_RIGHT &&
        input->instructions[in_index + 2].op == OP_END) {

      output->instructions[out_index].op = OP_SEEK_EMPTY;
      output->instructions[out_index].arg =
          input->instructions[in_index + 1].arg; // stride
      in_index += 2;
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }
  output->size = out_index;
  program_calculate_loops(output);
}

static addr_t get_loop_length(const Program *output, addr_t input) {
  int depth = 0;
  addr_t i = input;

  while (i < output->size) {
    if (output->instructions[i].op == OP_LOOP) {
      depth++;
    } else if (output->instructions[i].op == OP_END) {
      depth--;
      if (depth == 0) {
        return i - input + 1;
      }
    }
    i++;
  }
  return 0; /* Unmatched loop */
}

static int analyze_multi_transfer(const Program *program, addr_t loop_start,
                                  TransferTarget *targets) {
  addr_t i = loop_start;

  if (program->instructions[i].op != OP_LOOP) {
    return 0;
  }
  i++;

  i32 current_offset = 0;

  i32 offsets[MAX_TRANSFER_TARGETS + 1]; // +1 for source
  i32 factors[MAX_TRANSFER_TARGETS + 1];
  int num_entries = 0;

  while (i < program->size && program->instructions[i].op != OP_END) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      current_offset += instr->arg;
      break;

    case OP_INC: {
      int found = 0;
      for (int j = 0; j < num_entries; j++) {
        if (offsets[j] == current_offset) {
          factors[j] += instr->arg;
          found = 1;
          break;
        }
      }
      if (!found) {
        if (num_entries >= MAX_TRANSFER_TARGETS + 1)
          return 0;
        offsets[num_entries] = current_offset;
        factors[num_entries] = instr->arg;
        num_entries++;
      }
    } break;

    case OP_LOOP:
    case OP_END:
    case OP_IN:
    case OP_OUT:
    case OP_SET:
    case OP_SEEK_EMPTY:
    case OP_TRANSFER:
      return 0;

    default:
      return 0;
    }
    i++;
  }

  if (i >= program->size || program->instructions[i].op != OP_END) {
    return 0;
  }

  if (current_offset != 0) {
    return 0;
  }

  int source_change = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] == 0) {
      source_change = factors[j];
      break;
    }
  }

  if (source_change != -1) {
    return 0;
  }

  int num_targets = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] != 0 && factors[j] != 0) {
      targets[num_targets].offset = offsets[j];
      targets[num_targets].factor = factors[j];
      num_targets++;
    }
  }

  return num_targets;
}

void optimize_multi_transfer(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;

  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    if (input->instructions[in_index].op == OP_LOOP) {
      TransferTarget targets[MAX_TRANSFER_TARGETS];
      int num_targets = analyze_multi_transfer(input, in_index, targets);

      if (num_targets > 0) {
        output->instructions[out_index].op = OP_TRANSFER;
        output->instructions[out_index].arg = num_targets;

        for (int t = 0; t < num_targets; t++) {
          output->instructions[out_index].targets[t] = targets[t];
        }
        out_index++;

        // Emit separate SET instruction to zero the source cell
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0;  // value
        output->instructions[out_index].arg2 = 1; // count
        output->instructions[out_index].offset = 0;
        out_index++;

        addr_t loop_len = get_loop_length(input, in_index);
        in_index += loop_len - 1;

        continue;
      }
    }

    output->instructions[out_index] = input->instructions[in_index];
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_set_inc_merge(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  addr_t i = 0;
  while (i < original->size) {
    const Instruction *curr = &original->instructions[i];

    if (curr->op == OP_SET && curr->arg2 == 1) {
      i32 value = curr->arg;
      const i32 target_offset = curr->offset;

      addr_t j = i + 1;
      while (j < original->size) {
        const Instruction *next = &original->instructions[j];

        if (next->op == OP_INC && next->offset == target_offset) {
          value += next->arg;
          j++;
          continue;
        }

        if (next->op == OP_SET && next->offset == target_offset) {
          break;
        }

        if (next->op == OP_LOOP || next->op == OP_END || next->op == OP_IN ||
            next->op == OP_OUT || next->op == OP_RIGHT ||
            next->op == OP_SEEK_EMPTY || next->op == OP_TRANSFER) {
          break;
        }

        if ((next->op == OP_INC || next->op == OP_SET) &&
            next->offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        // unknown op
        break;
      }

      output->instructions[out_index].op = OP_SET;
      output->instructions[out_index].arg = value;
      output->instructions[out_index].arg2 = 1;
      output->instructions[out_index].offset = target_offset;
      out_index++;

      i = j;
      continue;
    }

    output->instructions[out_index++] = *curr;
    i++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

static int analyze_loop_balance(const Program *program, addr_t loop_start, i32 *net_movement) {
  if (program->instructions[loop_start].op != OP_LOOP) {
    return 0;
  }

  i32 movement = 0;
  i32 depth = 1;
  addr_t i = loop_start + 1;

  while (i < program->size && depth > 0) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      movement += instr->arg;
      break;

    case OP_LOOP:
      depth++;
      i32 child_move;
      const i32 child_movable = analyze_loop_balance(program, i, &child_move);
      if (!child_movable) {
        return 0;
      }

      if (child_move != 0) {
        return 0;
      }
      break;

    case OP_END:
      depth--;
      break;

    case OP_INC:
    case OP_SET:
    case OP_OUT:
    case OP_IN:
    case OP_TRANSFER:
      // Donesn't affect pointer position
      break;

    case OP_SEEK_EMPTY:
      // Moves pointer unpredictably
      return 0;

    default:
      return 0;
    }
    i++;
  }

  if (depth != 0) {
    fprintf(stderr, "Unmatched loop at instruction %u\n", loop_start);
    exit(1);
    return 0;
  }

  *net_movement = movement;
  return 1;
}

void optimize_offsets(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  i32 virtual_offset = 0;

  i32 loop_entry_offsets[MAX_CODE_SIZE];
  int loop_stack_size = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *instr = &original->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      virtual_offset += instr->arg;
      break;

    case OP_INC:
    case OP_SET:
    case OP_OUT:
    case OP_IN:
    case OP_TRANSFER:
    case OP_SEEK_EMPTY:
      output->instructions[out_index] = *instr;
      output->instructions[out_index].offset = instr->offset + virtual_offset;

      if (instr->op == OP_TRANSFER) {
        for (int t = 0; t < instr->arg; t++) {
          output->instructions[out_index].targets[t].offset += virtual_offset;
        }
      }

      out_index++;
      break;

    case OP_LOOP: {
      i32 net_movement;
      int is_balanced =
          analyze_loop_balance(original, i, &net_movement) && net_movement == 0;

      if (is_balanced) {
        i32 combined_offset = instr->offset + virtual_offset;
        loop_entry_offsets[loop_stack_size++] = virtual_offset;

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = combined_offset;
        out_index++;
      } else {
        if (virtual_offset != 0) {
          output->instructions[out_index].op = OP_RIGHT;
          output->instructions[out_index].arg = virtual_offset;
          output->instructions[out_index].offset = 0;
          out_index++;
          virtual_offset = 0;
        }

        loop_entry_offsets[loop_stack_size++] = -999999;

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = instr->offset;
        out_index++;
      }
      break;
    }

    case OP_END: {
      i32 entry_virtual_offset = loop_entry_offsets[--loop_stack_size];

      if (entry_virtual_offset == -999999) {
        if (virtual_offset != 0) {
          output->instructions[out_index].op = OP_RIGHT;
          output->instructions[out_index].arg = virtual_offset;
          output->instructions[out_index].offset = 0;
          out_index++;
          virtual_offset = 0;
        }

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = instr->offset;
        out_index++;
      } else {
        i32 combined_offset = instr->offset + entry_virtual_offset;
        virtual_offset = entry_virtual_offset;

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = combined_offset;
        out_index++;
      }
      break;
    }

    default:
      perror("optimize_offsets: unknown instruction");
      exit(1);
      break;
    }
  }

  if (virtual_offset != 0) {
    output->instructions[out_index].op = OP_RIGHT;
    output->instructions[out_index].arg = virtual_offset;
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void eliminate_dead_stores(Program* output, const Program* input) {
    memset(output, 0, sizeof(*output));
    addr_t out_index = 0;

    for (addr_t i = 0; i < input->size; i++) {
        const Instruction* curr = &input->instructions[i];

        if ((curr->op == OP_INC || curr->op == OP_SET) &&
            i + 1 < input->size) {
            const Instruction* next = &input->instructions[i + 1];
            if (next->op == OP_SET &&
                next->offset == curr->offset &&
                next->arg2 == 1) {
                continue;
            }
        }

        output->instructions[out_index++] = *curr;
    }

    output->size = out_index;
    program_calculate_loops(output);
}

void optimize_program(Program *program) {
  Program *optimized = malloc(sizeof(Program));
  Program *before_pass = malloc(sizeof(Program));

  for (int iteration = 0; iteration < 10; iteration++) {
    memcpy(before_pass, program, sizeof(Program));

    merge_consecutive_right_inc(optimized, program, OP_RIGHT);
    *program = *optimized;

    merge_consecutive_right_inc(optimized, program, OP_INC);
    *program = *optimized;

    create_zeroing_sets(optimized, program);
    *program = *optimized;

    optimize_memset(optimized, program);
    *program = *optimized;

    optimize_seek_empty(optimized, program);
    *program = *optimized;

    optimize_multi_transfer(optimized, program);
    *program = *optimized;

    optimize_set_inc_merge(optimized, program);
    *program = *optimized;

    optimize_offsets(optimized, program);
    *program = *optimized;

    eliminate_dead_stores(optimized, program);
    *program = *optimized;

    const int changed = memcmp(before_pass, program, sizeof(Program)) != 0;
    if (!changed) {
      break;
    }
  }

  free(optimized);
  free(before_pass);
}
