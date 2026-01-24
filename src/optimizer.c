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

void create_zeroing_sets(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];
    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_INC &&
        (input->instructions[in_index + 1].arg & 1) &&
        input->instructions[in_index + 2].op == OP_END) {
      output->instructions[out_index].op = OP_SET;
      output->instructions[out_index].arg = 0;
      output->instructions[out_index].arg2 = 1;
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
      i32 stride = 0;

      addr_t j = in_index;
      while (j + 2 < input->size) {
        const Instruction *right = &input->instructions[j + 1];
        const Instruction *next_set = &input->instructions[j + 2];

        if (right->op == OP_RIGHT && next_set->op == OP_SET &&
            next_set->arg == in_set_val) {
          if (stride == 0) {
            stride = right->arg;
          }
          if (right->arg == stride) {
            count++;
            j += 2;
          } else {
            break;
          }
        } else {
          break;
        }
      }

      if (count >= 2) {
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = in_set_val;
        output->instructions[out_index].arg2 = count;
        output->instructions[out_index].stride = stride;
        out_index++;

        output->instructions[out_index].op = OP_RIGHT;
        output->instructions[out_index].arg = (count - 1) * stride;
        out_index++;
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
      targets[num_targets].bias = 0;
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

static int analyze_loop_balance(const Program *program, addr_t loop_start,
                                i32 *net_movement) {
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
    case OP_DIV:
    case OP_MOD:
      break;

    case OP_SEEK_EMPTY:
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

    case OP_DIV:
    case OP_MOD:
      output->instructions[out_index] = *instr;
      output->instructions[out_index].offset = instr->offset + virtual_offset;
      output->instructions[out_index].targets[0].offset += virtual_offset;
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

static int is_cell_assignment(const Instruction *instr, i32 offset) {
  if (instr->op == OP_SET && instr->arg2 == 1 && instr->offset == offset) {
    return 1;
  }
  if (instr->op == OP_IN && instr->offset == offset) {
    return 1;
  }
  if (instr->op == OP_MOD && instr->targets[0].offset == offset) {
    return 1;
  }
  if (instr->op == OP_TRANSFER && instr->arg2 == 1 && instr->arg == 1 &&
      instr->targets[0].offset == offset) {
    return 1;
  }
  return 0;
}

static int instruction_uses_cell(const Instruction *instr, i32 offset) {
  switch (instr->op) {
  case OP_INC:
  case OP_OUT:
    return instr->offset == offset;

  case OP_SET:
    if (instr->arg2 > 1) {
      if (instr->stride <= 1) {
        return offset >= instr->offset && offset < instr->offset + instr->arg2;
      }
      if (offset < instr->offset) {
        return 0;
      }
      i32 diff = offset - instr->offset;
      if (diff % instr->stride != 0) {
        return 0;
      }
      return diff / instr->stride < instr->arg2;
    }
    return 0;

  case OP_IN:
    return 0;

  case OP_DIV:
    return instr->offset == offset;

  case OP_MOD:

    return instr->offset == offset;

  case OP_TRANSFER:
    if (instr->offset == offset)
      return 1;
    if (instr->arg2 != 1) {
      for (int t = 0; t < instr->arg; t++) {
        if (instr->targets[t].offset == offset)
          return 1;
      }
    }
    return 0;

  case OP_LOOP:
  case OP_END:
  case OP_RIGHT:
  case OP_SEEK_EMPTY:
    return 1;

  default:
    return 1;
  }
}

void eliminate_dead_stores(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if ((curr->op == OP_INC || (curr->op == OP_SET && curr->arg2 == 1))) {
      i32 target_offset = curr->offset;
      int is_dead = 0;

      for (addr_t j = i + 1; j < input->size; j++) {
        const Instruction *future = &input->instructions[j];

        if (is_cell_assignment(future, target_offset)) {
          is_dead = 1;
          break;
        }

        if (instruction_uses_cell(future, target_offset)) {
          break;
        }
      }

      if (is_dead) {
        continue;
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_set_transfer_merge(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_SET && curr->arg2 == 1 && i + 1 < input->size) {
      const Instruction *next = &input->instructions[i + 1];

      if (next->op == OP_TRANSFER) {
        int match_idx = -1;
        for (int t = 0; t < next->arg; t++) {
          if (next->targets[t].offset == curr->offset) {
            match_idx = t;
            break;
          }
        }

        if (match_idx >= 0) {
          output->instructions[out_index].op = OP_TRANSFER;
          output->instructions[out_index].arg = 1;
          output->instructions[out_index].arg2 = 1;
          output->instructions[out_index].offset = next->offset;
          output->instructions[out_index].targets[0] = next->targets[match_idx];
          output->instructions[out_index].targets[0].bias += curr->arg;
          out_index++;

          int remaining = next->arg - 1;
          if (remaining > 0) {
            output->instructions[out_index].op = OP_TRANSFER;
            output->instructions[out_index].arg = remaining;
            output->instructions[out_index].arg2 = 0;
            output->instructions[out_index].offset = next->offset;
            int t_out = 0;
            for (int t = 0; t < next->arg; t++) {
              if (t != match_idx) {
                output->instructions[out_index].targets[t_out++] =
                    next->targets[t];
              }
            }
            out_index++;
          }

          i++;
          continue;
        }
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

static int merge_inc_into_transfer(Instruction *transfer,
                                   const Instruction *inc) {
  int target_idx = -1;
  for (int t = 0; t < transfer->arg; t++) {
    if (transfer->targets[t].offset == inc->offset) {
      target_idx = t;
    }
  }

  if (target_idx >= 0) {
    transfer->targets[target_idx].bias += inc->arg;
    return 1;
  }

  if (inc->offset == transfer->offset) {
    for (int t = 0; t < transfer->arg; t++) {
      transfer->targets[t].bias += inc->arg * transfer->targets[t].factor;
    }
    return 1;
  }

  return 0;
}

void optimize_inc_transfer_merge(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_TRANSFER) {
      Instruction merged = *curr;

      int *skip = calloc(out_index, sizeof(int));
      for (int j = out_index - 1; j >= 0; j--) {
        if (output->instructions[j].op != OP_INC)
          break;

        if (merge_inc_into_transfer(&merged, &output->instructions[j])) {
          skip[j] = 1;
        } else {
          const Instruction *inc = &output->instructions[j];
          int interferes = (inc->offset == merged.offset);
          for (int t = 0; t < merged.arg && !interferes; t++) {
            if (inc->offset == merged.targets[t].offset)
              interferes = 1;
          }
          if (interferes)
            break;
        }
      }

      addr_t new_out = 0;
      for (addr_t j = 0; j < out_index; j++) {
        if (!skip[j]) {
          output->instructions[new_out++] = output->instructions[j];
        }
      }
      out_index = new_out;
      free(skip);

      while (i + 1 < input->size && input->instructions[i + 1].op == OP_INC) {
        if (merge_inc_into_transfer(&merged, &input->instructions[i + 1])) {
          i++;
        } else {
          const Instruction *inc = &input->instructions[i + 1];
          int interferes = (inc->offset == merged.offset);
          for (int t = 0; t < merged.arg && !interferes; t++) {
            if (inc->offset == merged.targets[t].offset)
              interferes = 1;
          }
          if (interferes) {
            break;
          } else {
            output->instructions[out_index++] = input->instructions[i + 1];
            i++;
          }
        }
      }

      output->instructions[out_index++] = merged;
      continue;
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

static int analyze_divmod_pattern(const Program *program, addr_t loop_start,
                                  i32 *dividend_off, i32 *divisor,
                                  i32 *quotient_off, i32 *remainder_off,
                                  i32 *temp_off) {
  if (loop_start + 5 >= program->size) {
    return 0;
  }

  const Instruction *loop_instr = &program->instructions[loop_start];
  const Instruction *inc_instr = &program->instructions[loop_start + 1];
  const Instruction *transfer1 = &program->instructions[loop_start + 2];
  const Instruction *transfer2 = &program->instructions[loop_start + 3];
  const Instruction *set_instr = &program->instructions[loop_start + 4];
  const Instruction *end_instr = &program->instructions[loop_start + 5];

  if (loop_instr->op != OP_LOOP) {
    return 0;
  }

  if (end_instr->op != OP_END || end_instr->arg != (i32)loop_start) {
    return 0;
  }

  if (end_instr->offset != loop_instr->offset) {
    return 0;
  }

  if (inc_instr->op != OP_INC || inc_instr->arg != -1 ||
      inc_instr->offset != loop_instr->offset) {
    return 0;
  }

  if (transfer1->op != OP_TRANSFER || transfer1->arg != 2 ||
      transfer1->arg2 != 0) {
    return 0;
  }

  if (transfer1->targets[0].factor != -1) {
    return 0;
  }

  if (transfer1->targets[1].factor != 1 || transfer1->targets[1].bias != 0) {
    return 0;
  }

  if (transfer2->op != OP_TRANSFER || transfer2->arg != 1 ||
      transfer2->arg2 != 1) {
    return 0;
  }

  if (transfer2->offset != transfer1->targets[0].offset) {
    return 0;
  }

  if (transfer2->targets[0].offset != transfer1->offset ||
      transfer2->targets[0].factor != 1) {
    return 0;
  }

  if (set_instr->op != OP_SET || set_instr->arg != 0 || set_instr->arg2 != 1) {
    return 0;
  }

  if (set_instr->offset != transfer1->targets[0].offset) {
    return 0;
  }

  *dividend_off = loop_instr->offset;
  *divisor = transfer1->targets[0].bias + 1;
  *quotient_off = transfer1->targets[1].offset;
  *remainder_off = transfer1->offset;
  *temp_off = transfer1->targets[0].offset;

  if (*divisor < 2) {
    return 0;
  }

  return 1;
}

void optimize_divmod(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction *instr = &input->instructions[in_index];

    if (instr->op == OP_LOOP) {
      i32 dividend_off, divisor, quotient_off, remainder_off, temp_off;

      if (analyze_divmod_pattern(input, in_index, &dividend_off, &divisor,
                                 &quotient_off, &remainder_off, &temp_off)) {
        output->instructions[out_index].op = OP_DIV;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = quotient_off;
        out_index++;

        output->instructions[out_index].op = OP_MOD;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = remainder_off;
        out_index++;

        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0;
        output->instructions[out_index].arg2 = 1;
        output->instructions[out_index].offset = dividend_off;
        out_index++;

        in_index += 5;
        continue;
      }
    }

    output->instructions[out_index] = *instr;
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

void optimize_eliminate_temp_cells(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    i32 temp_off;
    int is_candidate = 0;

    if (curr->op == OP_MOD || curr->op == OP_DIV) {
      temp_off = curr->targets[0].offset;
      is_candidate = 1;
    } else if (curr->op == OP_IN || (curr->op == OP_SET && curr->arg2 == 1)) {
      temp_off = curr->offset;
      is_candidate = 1;
    }

    if (is_candidate && i + 1 < input->size) {
      const Instruction *next = &input->instructions[i + 1];

      if (next->op == OP_TRANSFER && next->arg == 1 && next->arg2 == 1 &&
          next->offset == temp_off && next->targets[0].factor == 1 &&
          next->targets[0].bias == 0) {
        i32 final_off = next->targets[0].offset;

        addr_t set_idx = 0;
        int can_optimize = 0;

        for (addr_t j = i + 2; j < input->size; j++) {
          const Instruction *future = &input->instructions[j];

          if (future->op == OP_SET && future->arg == 0 && future->arg2 == 1 &&
              future->offset == temp_off) {
            set_idx = j;
            can_optimize = 1;
            break;
          }

          if ((future->op == OP_INC || future->op == OP_OUT ||
               future->op == OP_DIV || future->op == OP_MOD ||
               future->op == OP_IN) &&
              future->offset == temp_off) {
            break;
          }

          if (future->op == OP_TRANSFER && future->offset == temp_off) {
            break;
          }

          if (future->op == OP_LOOP || future->op == OP_END ||
              future->op == OP_RIGHT || future->op == OP_SEEK_EMPTY) {
            break;
          }
        }

        if (can_optimize) {
          output->instructions[out_index] = *curr;
          if (curr->op == OP_MOD || curr->op == OP_DIV) {
            output->instructions[out_index].targets[0].offset = final_off;
          } else {
            output->instructions[out_index].offset = final_off;
          }
          out_index++;

          for (addr_t j = i + 2; j < set_idx; j++) {
            output->instructions[out_index++] = input->instructions[j];
          }

          output->instructions[out_index++] = input->instructions[set_idx];
          i = set_idx;
          continue;
        }
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

static int instr_touches_offset_for_cancel(const Instruction *instr,
                                           const i32 offset) {
  switch (instr->op) {
  case OP_INC:
    return instr->offset == offset;
  case OP_SET:
    if (instr->arg2 == 1) {
      return instr->offset == offset;
    }
    if (instr->stride <= 1) {
      return offset >= instr->offset && offset < instr->offset + instr->arg2;
    }
    if (offset < instr->offset) {
      return 0;
    }
    i32 diff = offset - instr->offset;
    if (diff % instr->stride != 0) {
      return 0;
    }
    return diff / instr->stride < instr->arg2;
  case OP_OUT:
  case OP_IN:
    return instr->offset == offset;
  case OP_DIV:
  case OP_MOD:
    return instr->offset == offset || instr->targets[0].offset == offset;
  case OP_TRANSFER:
    if (instr->offset == offset)
      return 1;
    for (int t = 0; t < instr->arg; t++) {
      if (instr->targets[t].offset == offset)
        return 1;
    }
    return 0;
  case OP_LOOP:
  case OP_END:
  case OP_RIGHT:
  case OP_SEEK_EMPTY:
    return 1;
  default:
    return 1;
  }
}

void optimize_inc_cancellation(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  int *skip = calloc(input->size, sizeof(int));
  i32 *adjust = calloc(input->size, sizeof(i32));

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *instr = &input->instructions[i];
    if (instr->op != OP_INC) {
      continue;
    }

    i32 offset = instr->offset;

    for (addr_t j = i + 1; j < input->size; j++) {
      const Instruction *next = &input->instructions[j];

      if (next->op == OP_INC && next->offset == offset) {
        skip[i] = 1;
        adjust[j] += instr->arg;
        break;
      }

      if (instr_touches_offset_for_cancel(next, offset)) {
        break;
      }
    }
  }

  for (addr_t i = 0; i < input->size; i++) {
    if (skip[i]) {
      continue;
    }

    Instruction out_instr = input->instructions[i];
    if (out_instr.op == OP_INC) {
      out_instr.arg += adjust[i];
      if (out_instr.arg == 0) {
        continue;
      }
    }
    output->instructions[out_index++] = out_instr;
  }

  free(skip);
  free(adjust);
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

    optimize_divmod(optimized, program);
    *program = *optimized;

    eliminate_dead_stores(optimized, program);
    *program = *optimized;

    optimize_set_transfer_merge(optimized, program);
    *program = *optimized;

    optimize_inc_transfer_merge(optimized, program);
    *program = *optimized;

    optimize_eliminate_temp_cells(optimized, program);
    *program = *optimized;

    optimize_inc_cancellation(optimized, program);
    *program = *optimized;

    const int changed = memcmp(before_pass, program, sizeof(Program)) != 0;
    if (!changed) {
      break;
    }
  }

  free(optimized);
  free(before_pass);
}
