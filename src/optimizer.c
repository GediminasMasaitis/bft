#include <assert.h>
#include <string.h>

#include "machine.h"
#include "optimizer.h"

void merge_consecutive_right_inc(Program *output, const Program *input, const op_t op) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];
    if (instr.op == op) {
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
    const Instruction instr = input->instructions[in_index];
    if (instr.op == OP_LOOP &&
      in_index + 2 < input->size &&
          input->instructions[in_index + 1].op == OP_INC &&
          (input->instructions[in_index + 1].arg == 1 ||
           input->instructions[in_index + 1].arg == -1) &&
          input->instructions[in_index + 2].op == OP_END) {
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0; // value
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

void optimize_seek_empty(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    if (instr.op == OP_LOOP &&
        in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_RIGHT &&
        input->instructions[in_index + 2].op == OP_END) {

      output->instructions[out_index].op = OP_SEEK_EMPTY;
      output->instructions[out_index].arg = input->instructions[in_index + 1].arg; // stride
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

  for (addr_t i = 0; i < input->size; i++) {
    if (input->instructions[i].op == OP_LOOP) {
      TransferTarget targets[MAX_TRANSFER_TARGETS];
      int num_targets = analyze_multi_transfer(input, i, targets);

      if (num_targets > 0) {
        output->instructions[out_index].op = OP_TRANSFER;
        output->instructions[out_index].arg = num_targets;

        for (int t = 0; t < num_targets; t++) {
          output->instructions[out_index].targets[t] = targets[t];
        }

        addr_t loop_len = get_loop_length(input, i);
        i += loop_len - 1;

        out_index++;
        continue;
      }
    }

    output->instructions[out_index] = input->instructions[i];
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_transfer_inc(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_TRANSFER) {
      i32 final_value = curr->arg2;
      i32 src_offset = curr->offset; /* Transfer's source offset */
      addr_t j = i + 1;

      while (j < input->size) {
        const Instruction *next = &input->instructions[j];

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
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        break;
      }

      output->instructions[out_index] = *curr;
      output->instructions[out_index].arg2 = final_value;
      out_index++;

      i = j - 1;
      continue;
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_offsets(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
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
      output->instructions[out_index] = *instr;
      output->instructions[out_index].offset = virtual_offset;
      out_index++;
      break;

    case OP_LOOP:
    case OP_END:
    case OP_SEEK_EMPTY:
    case OP_TRANSFER:
      if (virtual_offset != 0) {
        output->instructions[out_index].op = OP_RIGHT;
        output->instructions[out_index].arg = virtual_offset;
        output->instructions[out_index].offset = 0;
        out_index++;
        virtual_offset = 0;
      }

      output->instructions[out_index] = *instr;
      output->instructions[out_index].offset = 0;
      out_index++;
      break;

    default:
      // copy unknowns
      output->instructions[out_index] = *instr;
      out_index++;
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

void optimize_dead_stores(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_INC && i + 1 < input->size) {
      const Instruction *next = &input->instructions[i + 1];
      if (next->op == OP_SET && curr->offset == next->offset) {
        continue;
      }
    }

    output->instructions[out_index++] = *curr;
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

void optimize_transfer_offsets(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];
    if (curr->op == OP_RIGHT) {
      i32 n = curr->arg;
      addr_t transfer_idx = 0;
      for (addr_t j = i + 1; j < input->size; j++) {
        if (input->instructions[j].op == OP_TRANSFER) {
          transfer_idx = j;
          break;
        }

        if (input->instructions[j].op == OP_RIGHT ||
            input->instructions[j].op == OP_LOOP ||
            input->instructions[j].op == OP_END ||
            input->instructions[j].op == OP_SEEK_EMPTY) {
          break;
        }
      }

      if (transfer_idx > 0) {
        addr_t right_idx = 0;
        for (addr_t j = transfer_idx + 1; j < input->size; j++) {
          if (input->instructions[j].op == OP_RIGHT) {
            right_idx = j;
            break;
          }

          if (input->instructions[j].op == OP_LOOP ||
              input->instructions[j].op == OP_END ||
              input->instructions[j].op == OP_TRANSFER ||
              input->instructions[j].op == OP_SEEK_EMPTY) {
            break;
          }
        }

        if (right_idx > 0) {
          i32 m = input->instructions[right_idx].arg;
          const Instruction *transfer = &input->instructions[transfer_idx];

          for (addr_t j = i + 1; j < transfer_idx; j++) {
            Instruction adjusted = input->instructions[j];
            adjusted.offset += n;
            output->instructions[out_index++] = adjusted;
          }

          Instruction new_transfer = *transfer;
          new_transfer.offset = n;

          for (int t = 0; t < new_transfer.arg; t++) {
            new_transfer.targets[t].offset += n;
          }

          output->instructions[out_index++] = new_transfer;

          for (addr_t j = transfer_idx + 1; j < right_idx; j++) {
            Instruction adjusted = input->instructions[j];
            adjusted.offset += n;
            output->instructions[out_index++] = adjusted;
          }

          if (n + m != 0) {
            Instruction right;
            memset(&right, 0, sizeof(right));
            right.op = OP_RIGHT;
            right.arg = n + m;
            output->instructions[out_index++] = right;
          }

          i = right_idx;
          continue;
        }
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
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

    // optimize_memset(&optimized, program);
    // *program = optimized;

    optimize_seek_empty(&optimized, program);
    *program = optimized;

    //  optimize_multi_transfer(&optimized, program);
    //  *program = optimized;

    //  optimize_transfer_inc(&optimized, program);
    //  *program = optimized;

    //  optimize_dead_stores(&optimized, program);
    //  *program = optimized;

    //  optimize_set_inc_merge(&optimized, program);
    //  *program = optimized;

    if (program->size == before_size) {
      break;
    }
  }

  //  optimize_offsets(&optimized, program);
  //  *program = optimized;

  //  optimize_transfer_offsets(&optimized, program);
  //  *program = optimized;
}
