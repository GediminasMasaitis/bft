#include <assert.h>
#include <string.h>

#include "machine.h"
#include "optimizer.h"

void merge_consecutive_right_inc(Program *output, const Program *input, const op_t target_op) {
  assert(target_op == OP_RIGHT || target_op == OP_INC);

  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    Instruction instr = input->instructions[in_index];
    if (instr.op == target_op) {
      i32 count = instr.arg;
      while (in_index + 1 < input->size) {
        if (input->instructions[in_index + 1].op != instr.op) {
          break;
        }

        count += input->instructions[in_index + 1].arg;
        in_index++;
      }

      output->instructions[out_index].op = instr.op;
      output->instructions[out_index].arg = count;
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
    Instruction instr = input->instructions[in_index];
    if (instr.op == OP_LOOP &&
      in_index + 2 < input->size &&
          input->instructions[in_index + 1].op == OP_INC &&
          (input->instructions[in_index + 1].arg == 1 ||
           input->instructions[in_index + 1].arg == -1) &&
          input->instructions[in_index + 2].op == OP_END) {
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0; // value
        output->instructions[out_index].arg2 = 1; // count
        out_index += 2;
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void create_memset(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));

  addr_t opt_index = 0;
  for (addr_t i = 0; i < original->size; i++) {
    Instruction instr = original->instructions[i];

    /* Look for SET followed by RIGHT 1 pattern */
    if (instr.op == OP_SET) {
      i32 set_value = instr.arg;
      i32 count = 1;

      /* Count consecutive SET value, RIGHT 1 patterns */
      addr_t j = i;
      while (j + 2 < original->size) {
        const Instruction *right = &original->instructions[j + 1];
        const Instruction *next_set = &original->instructions[j + 2];

        /* Check for RIGHT 1 followed by SET with same value */
        if (right->op == OP_RIGHT && right->arg == 1 &&
            next_set->op == OP_SET && next_set->arg == set_value) {
          count++;
          j += 2; /* Move past RIGHT and SET */
        } else {
          break;
        }
      }

      if (count >= 2) {
        /* Emit the memset SET with count */
        optimized->instructions[opt_index].op = OP_SET;
        optimized->instructions[opt_index].arg = set_value;
        optimized->instructions[opt_index].arg2 = count;
        opt_index++;

        /* Emit the pointer movement: we set 'count' cells, so move count-1 to
         * end at last cell */
        optimized->instructions[opt_index].op = OP_RIGHT;
        optimized->instructions[opt_index].arg = count - 1;
        opt_index++;

        /* Skip past all the instructions we consumed */
        i = j;
        continue;
      }
    }

    /* Default: copy instruction (ensure arg2 is set for SET) */
    optimized->instructions[opt_index] = instr;
    if (instr.op == OP_SET && optimized->instructions[opt_index].arg2 == 0) {
      optimized->instructions[opt_index].arg2 = 1;
    }
    opt_index++;
  }
  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_seek_empty(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));

  addr_t opt_index = 0;
  for (addr_t i = 0; i < original->size; i++) {
    Instruction instr = original->instructions[i];
    if (instr.op == OP_LOOP) {
      /* Check for pattern [>] or [<] - now just RIGHT with positive or negative
       * arg */
      if (i + 2 < original->size &&
          original->instructions[i + 1].op == OP_RIGHT &&
          original->instructions[i + 2].op == OP_END) {
        optimized->instructions[opt_index].op = OP_SEEK_EMPTY;
        optimized->instructions[opt_index].arg =
            original->instructions[i + 1].arg;
        i += 2; /* Skip the next two instructions */
        opt_index++;
        continue;
      }
    }
    optimized->instructions[opt_index] = instr;
    opt_index++;
  }
  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_transfer(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));

  addr_t opt_index = 0;

  for (addr_t i = 0; i < original->size; i++) {
    if (original->instructions[i].op == OP_LOOP) {
      if (i + 5 < original->size) {
        const Instruction dec = original->instructions[i + 1];
        const Instruction right1 = original->instructions[i + 2];
        const Instruction inc = original->instructions[i + 3];
        const Instruction right2 = original->instructions[i + 4];
        const Instruction end = original->instructions[i + 5];

        const int match = dec.op == OP_INC && dec.arg == -1 &&
                          right1.op == OP_RIGHT && inc.op == OP_INC &&
                          right2.op == OP_RIGHT && end.op == OP_END &&
                          right1.arg + right2.arg == 0;

        if (match) {
          optimized->instructions[opt_index].op = OP_TRANSFER;
          optimized->instructions[opt_index].arg = 1;
          optimized->instructions[opt_index].targets[0] = (TransferTarget){
              .offset = right1.arg,
              .factor = inc.arg,
          };
          opt_index++;

          i += 5;
          continue;
        }
      }
    }

    optimized->instructions[opt_index] = original->instructions[i];
    opt_index++;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

static addr_t get_loop_length(const Program *program, addr_t loop_start) {
  int depth = 0;
  addr_t i = loop_start;

  while (i < program->size) {
    if (program->instructions[i].op == OP_LOOP) {
      depth++;
    } else if (program->instructions[i].op == OP_END) {
      depth--;
      if (depth == 0) {
        return i - loop_start + 1;
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

void optimize_multi_transfer(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));

  addr_t opt_index = 0;

  for (addr_t i = 0; i < original->size; i++) {
    if (original->instructions[i].op == OP_LOOP) {
      TransferTarget targets[MAX_TRANSFER_TARGETS];
      int num_targets = analyze_multi_transfer(original, i, targets);

      if (num_targets > 0) {
        optimized->instructions[opt_index].op = OP_TRANSFER;
        optimized->instructions[opt_index].arg = num_targets;

        for (int t = 0; t < num_targets; t++) {
          optimized->instructions[opt_index].targets[t] = targets[t];
        }

        addr_t loop_len = get_loop_length(original, i);
        i += loop_len - 1;

        opt_index++;
        continue;
      }
    }

    optimized->instructions[opt_index] = original->instructions[i];
    opt_index++;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_transfer_inc(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));
  addr_t opt_index = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *curr = &original->instructions[i];

    if (curr->op == OP_TRANSFER) {
      i32 final_value = curr->arg2;
      i32 src_offset = curr->offset; /* Transfer's source offset */
      addr_t j = i + 1;

      while (j < original->size) {
        const Instruction *next = &original->instructions[j];

        /* Only merge INC if it targets the same cell as the transfer's source
         */
        if (next->op == OP_INC && next->offset == src_offset) {
          final_value += next->arg;
          j++;
          continue;
        }

        // do explici srt merge pass?
        if (next->op == OP_SET && next->offset == src_offset) {
          break;
        }

        if (next->op == OP_LOOP || next->op == OP_END || next->op == OP_RIGHT ||
            next->op == OP_SEEK_EMPTY || next->op == OP_TRANSFER ||
            next->op == OP_IN || next->op == OP_OUT) {
          break;
        }

        if ((next->op == OP_INC || next->op == OP_SET) &&
            next->offset != src_offset) {
          optimized->instructions[opt_index++] = *next;
          j++;
          continue;
        }

        break;
      }

      optimized->instructions[opt_index] = *curr;
      optimized->instructions[opt_index].arg2 = final_value;
      opt_index++;

      i = j - 1;
      continue;
    }

    optimized->instructions[opt_index++] = *curr;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_offsets(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));

  addr_t opt_index = 0;
  i32 virtual_offset = 0;

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
      optimized->instructions[opt_index] = *instr;
      optimized->instructions[opt_index].offset = virtual_offset;
      opt_index++;
      break;

    case OP_LOOP:
    case OP_END:
    case OP_SEEK_EMPTY:
    case OP_TRANSFER:
      if (virtual_offset != 0) {
        optimized->instructions[opt_index].op = OP_RIGHT;
        optimized->instructions[opt_index].arg = virtual_offset;
        optimized->instructions[opt_index].offset = 0;
        opt_index++;
        virtual_offset = 0;
      }

      optimized->instructions[opt_index] = *instr;
      optimized->instructions[opt_index].offset = 0;
      opt_index++;
      break;

    default:
      // copy unknowns
      optimized->instructions[opt_index] = *instr;
      opt_index++;
      break;
    }
  }

  if (virtual_offset != 0) {
    optimized->instructions[opt_index].op = OP_RIGHT;
    optimized->instructions[opt_index].arg = virtual_offset;
    opt_index++;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_dead_stores(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));
  addr_t opt_index = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *curr = &original->instructions[i];

    if (curr->op == OP_INC && i + 1 < original->size) {
      const Instruction *next = &original->instructions[i + 1];
      if (next->op == OP_SET && curr->offset == next->offset) {
        continue;
      }
    }

    optimized->instructions[opt_index++] = *curr;
  }
  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_set_inc_merge(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));
  addr_t opt_index = 0;

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
          optimized->instructions[opt_index++] = *next;
          j++;
          continue;
        }

        // unknown op
        break;
      }

      optimized->instructions[opt_index].op = OP_SET;
      optimized->instructions[opt_index].arg = value;
      optimized->instructions[opt_index].arg2 = 1;
      optimized->instructions[opt_index].offset = target_offset;
      opt_index++;

      i = j;
      continue;
    }

    optimized->instructions[opt_index++] = *curr;
    i++;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_transfer_offsets(Program *optimized, const Program *original) {
  memset(optimized, 0, sizeof(*optimized));
  addr_t opt_index = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *curr = &original->instructions[i];
    if (curr->op == OP_RIGHT) {
      i32 n = curr->arg;
      addr_t transfer_idx = 0;
      for (addr_t j = i + 1; j < original->size; j++) {
        if (original->instructions[j].op == OP_TRANSFER) {
          transfer_idx = j;
          break;
        }

        if (original->instructions[j].op == OP_RIGHT ||
            original->instructions[j].op == OP_LOOP ||
            original->instructions[j].op == OP_END ||
            original->instructions[j].op == OP_SEEK_EMPTY) {
          break;
        }
      }

      if (transfer_idx > 0) {
        addr_t right_idx = 0;
        for (addr_t j = transfer_idx + 1; j < original->size; j++) {
          if (original->instructions[j].op == OP_RIGHT) {
            right_idx = j;
            break;
          }

          if (original->instructions[j].op == OP_LOOP ||
              original->instructions[j].op == OP_END ||
              original->instructions[j].op == OP_TRANSFER ||
              original->instructions[j].op == OP_SEEK_EMPTY) {
            break;
          }
        }

        if (right_idx > 0) {
          i32 m = original->instructions[right_idx].arg;
          const Instruction *transfer = &original->instructions[transfer_idx];

          for (addr_t j = i + 1; j < transfer_idx; j++) {
            Instruction adjusted = original->instructions[j];
            adjusted.offset += n;
            optimized->instructions[opt_index++] = adjusted;
          }

          Instruction new_transfer = *transfer;
          new_transfer.offset = n;

          for (int t = 0; t < new_transfer.arg; t++) {
            new_transfer.targets[t].offset += n;
          }

          optimized->instructions[opt_index++] = new_transfer;

          for (addr_t j = transfer_idx + 1; j < right_idx; j++) {
            Instruction adjusted = original->instructions[j];
            adjusted.offset += n;
            optimized->instructions[opt_index++] = adjusted;
          }

          if (n + m != 0) {
            Instruction right;
            memset(&right, 0, sizeof(right));
            right.op = OP_RIGHT;
            right.arg = n + m;
            optimized->instructions[opt_index++] = right;
          }

          i = right_idx;
          continue;
        }
      }
    }

    optimized->instructions[opt_index++] = *curr;
  }

  optimized->size = opt_index;
  program_calculate_loops(optimized);
}

void optimize_program(Program *program) {
  Program optimized;

  while (1) {
    addr_t before_size = program->size;

    merge_consecutive_right_inc(&optimized, program, OP_RIGHT);
    *program = optimized;

    merge_consecutive_right_inc(&optimized, program, OP_INC);
    *program = optimized;

    create_zeroing_sets(&optimized, program);
    *program = optimized;

    optimize_memset(&optimized, program);
    *program = optimized;

    // optimize_seek_empty(&optimized, program);
    // *program = optimized;

    // optimize_multi_transfer(&optimized, program);
    // *program = optimized;

    // optimize_transfer_inc(&optimized, program);
    // *program = optimized;

    // optimize_dead_stores(&optimized, program);
    // *program = optimized;

    // optimize_set_inc_merge(&optimized, program);
    // *program = optimized;

    if (program->size == before_size) {
      break;
    }
  }

  // optimize_offsets(&optimized, program);
  // *program = optimized;

  // optimize_transfer_offsets(&optimized, program);
  // *program = optimized;
}
