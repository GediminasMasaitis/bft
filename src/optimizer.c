#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "machine.h"
#include "optimizer.h"

/*******************************************************************************
 * PASS 1: INSTRUCTION FOLDING
 *
 * Merges consecutive identical operations (RIGHT or INC) at the same offset
 * into a single operation with accumulated count.
 *
 * Examples:
 *   > > > >     →  RIGHT 4
 *   + + + + +   →  INC 5
 *   - - -       →  INC -3  (decrements are negative increments)
 *   < <         →  RIGHT -2
 *
 * This is the most basic optimization and should be run first to simplify
 * the instruction stream for subsequent passes.
 ******************************************************************************/
void merge_consecutive_right_inc(Program *output, const Program *input,
                                 const op_t op) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    // Only process instructions matching the target operation type
    if (instr.op == op) {
      i32 count = instr.arg;
      i32 offset = instr.offset;

      // Accumulate consecutive operations with the same offset
      while (in_index + 1 < input->size) {
        const Instruction *next = &input->instructions[in_index + 1];

        // Stop if next instruction is different type or different offset
        if (next->op != instr.op || next->offset != offset) {
          break;
        }

        count += next->arg;
        in_index++;
      }

      // Emit single merged instruction
      output->instructions[out_index].op = instr.op;
      output->instructions[out_index].arg = count;
      output->instructions[out_index].offset = offset;
      out_index++;
    } else {
      // Pass through non-matching instructions unchanged
      output->instructions[out_index] = instr;
      out_index++;
    }
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS 2: ZEROING LOOP DETECTION
 *
 * Recognizes the common "[-]" idiom (and "[+]") which sets a cell to zero.
 *
 * Pattern: [ INC ±odd ]
 *
 * Why odd? In 8-bit arithmetic (0-255), adding an odd number repeatedly will
 * eventually hit zero. For example:
 *   - Start at 5, subtract 1: 5→4→3→2→1→0 ✓
 *   - Start at 5, subtract 3: 5→2→255→252→...→0 (eventually wraps to 0)
 *   - Start at 4, subtract 2: 4→2→0 ✓ or 4→2→254→252→...→2→0 (only works if even)
 *
 * Actually for even decrements, it only zeroes if the initial value is
 * divisible by gcd(decrement, 256). The optimizer conservatively requires
 * odd decrements which always terminate.
 *
 * Example: [-] (loop with single decrement)
 *   Before: LOOP, INC -1, END
 *   After:  SET 0
 ******************************************************************************/
void create_zeroing_sets(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    /* Check for pattern: [ INC(±odd) ] */
    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_INC &&
        (input->instructions[in_index + 1].arg & 1) && /* arg is odd */
        input->instructions[in_index + 2].op == OP_END) {

      /* Replace entire loop with SET 0 */
      output->instructions[out_index].op = OP_SET;
      output->instructions[out_index].arg = 0;   /* value to set */
      output->instructions[out_index].arg2 = 1;  /* count (single cell) */
      in_index += 2; /* Skip the INC and END */
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS 3: MEMSET OPTIMIZATION
 *
 * Combines sequences of SET instructions with intervening pointer moves into
 * a single memset-style operation.
 *
 * Pattern: SET v > SET v > SET v > ...
 * Result:  SET v (count=N, stride=S)
 *
 * This is common when initializing arrays in BF, e.g., clearing multiple cells.
 * The stride tracks non-contiguous patterns (e.g., clearing every 2nd cell).
 *
 * Example: [-] > [-] > [-] (after zeroing pass becomes SET 0 > SET 0 > SET 0)
 *   Before: SET 0, RIGHT 1, SET 0, RIGHT 1, SET 0
 *   After:  SET 0 (count=3, stride=1), RIGHT 2
 *
 * Note: We emit a final RIGHT to maintain correct pointer position.
 ******************************************************************************/
void optimize_memset(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    if (instr.op == OP_SET) {
      const i32 in_set_val = instr.arg;
      i32 count = 1;
      i32 stride = 0;

      /* Look ahead for pattern: RIGHT, SET(same value), RIGHT, SET, ... */
      addr_t j = in_index;
      while (j + 2 < input->size) {
        const Instruction *right = &input->instructions[j + 1];
        const Instruction *next_set = &input->instructions[j + 2];

        if (right->op == OP_RIGHT && next_set->op == OP_SET &&
            next_set->arg == in_set_val) {
          /* First iteration establishes the stride */
          if (stride == 0) {
            stride = right->arg;
          }
          /* Continue only if stride is consistent */
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

      /* Only optimize if we found at least 2 SETs */
      if (count >= 2) {
        /* Emit combined SET with count and stride */
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = in_set_val;
        output->instructions[out_index].arg2 = count;  /* number of cells */
        output->instructions[out_index].stride = stride;
        out_index++;

        /* Emit RIGHT to maintain correct final pointer position */
        /* (we're now at position (count-1)*stride relative to start) */
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

/*******************************************************************************
 * PASS 4: SEEK EMPTY CELL OPTIMIZATION
 *
 * Recognizes the "[>]" and "[<]" idioms which scan left or right until
 * finding a zero cell.
 *
 * Pattern: [ RIGHT ±n ]
 * Result:  SEEK_EMPTY (stride=±n)
 *
 * This is commonly used in BF to find the next available "slot" in a data
 * structure, or to skip past non-zero values.
 *
 * Example: [>] (scan right for zero)
 *   Before: LOOP, RIGHT 1, END
 *   After:  SEEK_EMPTY (stride=1)
 ******************************************************************************/
void optimize_seek_empty(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    /* Check for pattern: [ RIGHT ] */
    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_RIGHT &&
        input->instructions[in_index + 2].op == OP_END) {

      output->instructions[out_index].op = OP_SEEK_EMPTY;
      output->instructions[out_index].arg =
          input->instructions[in_index + 1].arg; /* stride/direction */
      in_index += 2;
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }
  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * HELPER: GET LOOP LENGTH
 *
 * Returns the number of instructions in a loop, including the opening and
 * closing brackets. Handles nested loops correctly.
 *
 * Used by transfer optimization to skip over processed loops.
 ******************************************************************************/
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
  return 0; /* Unmatched loop - should not happen in valid program */
}

/*******************************************************************************
 * PASS 5: MULTI-TARGET TRANSFER ANALYSIS
 *
 * This is one of the most important optimizations. It recognizes "transfer
 * loops" - loops that copy/multiply a value to one or more target cells
 * while decrementing the source to zero.
 *
 * Pattern Analysis:
 * A transfer loop has the form: [ movements and increments ]
 * Where:
 *   - The source cell (at offset 0) is decremented by an odd amount
 *   - Other cells may be incremented/decremented by any amount
 *   - Net pointer movement is zero (loop is "balanced")
 *
 * Example: [->+>++<<] (copy to dp+1, add 2x to dp+2)
 *   - Source (offset 0): decremented by 1 per iteration
 *   - Target at offset 1: incremented by 1 per iteration → factor = 1
 *   - Target at offset 2: incremented by 2 per iteration → factor = 2
 *
 * After N iterations (where N = initial value of source cell):
 *   - Source = 0
 *   - Target1 += N * 1
 *   - Target2 += N * 2
 *
 * This replaces an O(N) loop with O(1) multiplications.
 *
 * Return value: number of transfer targets (0 if not a valid transfer loop)
 ******************************************************************************/
static int analyze_multi_transfer(const Program *program, addr_t loop_start,
                                  TransferTarget *targets) {
  addr_t i = loop_start;

  if (program->instructions[i].op != OP_LOOP) {
    return 0;
  }
  i++;

  i32 current_offset = 0;

  /* Track all cells modified in the loop and their cumulative changes */
  i32 offsets[MAX_TRANSFER_TARGETS + 1]; /* +1 for source cell */
  i32 factors[MAX_TRANSFER_TARGETS + 1];
  int num_entries = 0;

  /* Scan loop body, tracking pointer position and cell modifications */
  while (i < program->size && program->instructions[i].op != OP_END) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      current_offset += instr->arg;
      break;

    case OP_INC: {
      /* Find or create entry for this offset */
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
          return 0; /* Too many targets */
        offsets[num_entries] = current_offset;
        factors[num_entries] = instr->arg;
        num_entries++;
      }
    } break;

    /* These instructions disqualify the loop from being a simple transfer */
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

  /* Verify loop ends properly */
  if (i >= program->size || program->instructions[i].op != OP_END) {
    return 0;
  }

  /* Loop must be balanced (pointer returns to start) */
  if (current_offset != 0) {
    return 0;
  }

  /* Find how source cell (offset 0) changes each iteration */
  int source_change = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] == 0) {
      source_change = factors[j];
      break;
    }
  }

  /* Source must decrease (negative change) to guarantee termination */
  if (source_change >= 0) {
    return 0;
  }

  /* Source decrement must be odd to guarantee hitting zero in 8-bit arithmetic */
  if ((source_change & 1) == 0) {
    return 0;
  }

  const i32 decrement_mag = -source_change;

  /* Build list of transfer targets (non-source cells with non-zero change) */
  int num_targets = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] != 0 && factors[j] != 0) {
      /*
       * For source decremented by D, target incremented by F:
       * After source/D iterations, target += (source/D) * F = source * (F/D)
       *
       * For this to work exactly, F must be divisible by D.
       */
      if (decrement_mag > 1 && (factors[j] % decrement_mag) != 0) {
        return 0; /* Can't evenly divide - not a clean transfer */
      }

      targets[num_targets].offset = offsets[j];
      targets[num_targets].factor = factors[j] / decrement_mag;
      targets[num_targets].bias = 0;
      num_targets++;
    }
  }

  return num_targets;
}

/*******************************************************************************
 * PASS 5 (MAIN): MULTI-TARGET TRANSFER OPTIMIZATION
 *
 * Uses analyze_multi_transfer to find transfer loops and replace them with
 * OP_TRANSFER instructions.
 *
 * The OP_TRANSFER instruction represents:
 *   for each target t:
 *     dp[t.offset] += dp[source] * t.factor + t.bias
 *
 * Followed by setting the source to zero.
 ******************************************************************************/
void optimize_multi_transfer(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;

  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    if (input->instructions[in_index].op == OP_LOOP) {
      TransferTarget targets[MAX_TRANSFER_TARGETS];
      int num_targets = analyze_multi_transfer(input, in_index, targets);

      if (num_targets > 0) {
        /* Emit TRANSFER instruction */
        output->instructions[out_index].op = OP_TRANSFER;
        output->instructions[out_index].arg = num_targets;

        for (int t = 0; t < num_targets; t++) {
          output->instructions[out_index].targets[t] = targets[t];
        }
        out_index++;

        /* Emit SET 0 to zero the source cell */
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0;  /* value */
        output->instructions[out_index].arg2 = 1; /* count */
        output->instructions[out_index].offset = 0;
        out_index++;

        /* Skip past the loop we just optimized */
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

/*******************************************************************************
 * PASS 6: SET + INC MERGE
 *
 * Combines a SET instruction with subsequent INC instructions on the same cell.
 * Also reorders non-interfering instructions to maximize merging opportunities.
 *
 * Pattern: SET v, INC n, INC m, ...
 * Result:  SET (v + n + m + ...)
 *
 * Example:
 *   SET 5, INC 3, INC -1 → SET 7
 *
 * The pass also moves independent instructions (operations on other cells)
 * across the SET to enable merging:
 *
 *   SET 5 @0, INC 1 @1, INC 3 @0 → INC 1 @1, SET 8 @0
 *
 * This increases opportunities for constant folding.
 ******************************************************************************/
void optimize_set_inc_merge(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  addr_t i = 0;
  while (i < original->size) {
    const Instruction *curr = &original->instructions[i];

    /* Look for single-cell SET instructions */
    if (curr->op == OP_SET && curr->arg2 == 1) {
      i32 value = curr->arg;
      const i32 target_offset = curr->offset;

      addr_t j = i + 1;
      while (j < original->size) {
        const Instruction *next = &original->instructions[j];

        /* Merge INC on same cell into the SET */
        if (next->op == OP_INC && next->offset == target_offset) {
          value += next->arg;
          j++;
          continue;
        }

        /* Stop if we see another SET to this cell (it would override ours) */
        if (next->op == OP_SET && next->offset == target_offset) {
          break;
        }

        /* Stop on control flow or position changes - can't reorder past these */
        if (next->op == OP_LOOP || next->op == OP_END || next->op == OP_IN ||
            next->op == OP_RIGHT || next->op == OP_SEEK_EMPTY) {
          break;
        }

        /* OUT on different cell can be moved before our SET */
        if (next->op == OP_OUT && next->offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* TRANSFER can be moved if it doesn't touch our target cell */
        if (next->op == OP_TRANSFER) {
          int dominated = (next->offset == target_offset);
          for (int t = 0; t < next->arg && !dominated; t++)
            dominated = (next->targets[t].offset == target_offset);
          if (!dominated) {
            output->instructions[out_index++] = *next;
            j++;
            continue;
          }
          break;
        }

        /* DIV/MOD can be moved if they don't touch our target */
        if ((next->op == OP_DIV || next->op == OP_MOD) &&
            next->offset != target_offset &&
            next->targets[0].offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* INC/SET/OUT on different cell can be moved */
        if ((next->op == OP_INC || next->op == OP_SET || next->op == OP_OUT) &&
            next->offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* Unknown op - stop to be safe */
        break;
      }

      /* Emit the merged SET */
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

/*******************************************************************************
 * HELPER: ANALYZE LOOP BALANCE
 *
 * Determines if a loop is "balanced" - meaning the data pointer returns to
 * its starting position at the end of each iteration.
 *
 * A balanced loop allows us to use offsets throughout instead of explicit
 * pointer movements, which enables the offset threading optimization.
 *
 * Returns 1 if balanced, 0 otherwise.
 * Sets *net_movement to the total pointer displacement per iteration.
 *
 * Recursively checks nested loops - they must also be balanced with zero
 * net movement for the outer loop to be optimizable.
 ******************************************************************************/
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
      /* Recursively check nested loop */
      i32 child_move;
      const i32 child_movable = analyze_loop_balance(program, i, &child_move);
      if (!child_movable) {
        return 0; /* Nested loop can't be analyzed */
      }

      if (child_move != 0) {
        return 0; /* Nested loop has non-zero net movement - can't predict */
      }
      break;

    case OP_END:
      depth--;
      break;

    /* These operations don't affect pointer position */
    case OP_INC:
    case OP_SET:
    case OP_OUT:
    case OP_IN:
    case OP_TRANSFER:
    case OP_DIV:
    case OP_MOD:
      break;

    /* SEEK_EMPTY has unpredictable movement - disqualifies the loop */
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

/*******************************************************************************
 * PASS 7: OFFSET THREADING
 *
 * This is a key optimization that eliminates most pointer movement instructions
 * by converting them into offsets on other operations.
 *
 * Instead of:  > > + + < < + (move right 2, increment, move back, increment)
 * We get:      INC 2 @2, INC 1 @0 (increment at offset 2, increment at offset 0)
 *
 * The optimization maintains a "virtual offset" - where the pointer would be
 * if we had executed all the RIGHT instructions. Operations use this virtual
 * offset, and actual RIGHT instructions are only emitted when necessary.
 *
 * For balanced loops, we track the virtual offset at loop entry and restore
 * it after the loop. For unbalanced loops (or loops with SEEK_EMPTY), we
 * must emit a RIGHT to materialize the virtual offset before the loop.
 *
 * This pass dramatically reduces instruction count for typical BF programs
 * which do lots of local pointer manipulation.
 ******************************************************************************/
void optimize_offsets(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  i32 virtual_offset = 0; /* Accumulated but not-yet-emitted pointer movement */

  /* Stack to track virtual offset at loop entry points */
  i32 loop_entry_offsets[MAX_CODE_SIZE];
  int loop_stack_size = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *instr = &original->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      /* Accumulate into virtual offset instead of emitting */
      virtual_offset += instr->arg;
      break;

    /* Operations that can use an offset - add virtual offset to their offset */
    case OP_INC:
    case OP_SET:
    case OP_OUT:
    case OP_IN:
    case OP_TRANSFER:
    case OP_SEEK_EMPTY:
      output->instructions[out_index] = *instr;
      output->instructions[out_index].offset = instr->offset + virtual_offset;

      /* TRANSFER targets also need offset adjustment */
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
        /*
         * Balanced loop: we can maintain virtual offset through the loop.
         * The loop condition and body all use offsets relative to current
         * virtual position.
         */
        i32 combined_offset = instr->offset + virtual_offset;
        loop_entry_offsets[loop_stack_size++] = virtual_offset;

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = combined_offset;
        out_index++;
      } else {
        /*
         * Unbalanced loop: we don't know where the pointer will be after
         * the loop, so we must materialize the virtual offset now.
         */
        if (virtual_offset != 0) {
          output->instructions[out_index].op = OP_RIGHT;
          output->instructions[out_index].arg = virtual_offset;
          output->instructions[out_index].offset = 0;
          out_index++;
          virtual_offset = 0;
        }

        /* Use sentinel value to indicate unbalanced loop */
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
        /* Ending unbalanced loop - materialize any virtual offset */
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
        /*
         * Ending balanced loop - restore virtual offset to what it was
         * at loop entry (since the loop is balanced, pointer position
         * is the same).
         */
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

  /* Emit any remaining virtual offset at program end */
  if (virtual_offset != 0) {
    output->instructions[out_index].op = OP_RIGHT;
    output->instructions[out_index].arg = virtual_offset;
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * HELPER: IS CELL ASSIGNMENT
 *
 * Returns true if the instruction completely overwrites a cell at the given
 * offset, making any previous value irrelevant.
 *
 * Used by dead store elimination to determine when a write is definitely dead.
 ******************************************************************************/
static int is_cell_assignment(const Instruction *instr, i32 offset) {
  /* SET with count=1 overwrites the cell */
  if (instr->op == OP_SET && instr->arg2 == 1 && instr->offset == offset) {
    return 1;
  }
  /* IN reads from input, completely replacing cell value */
  if (instr->op == OP_IN && instr->offset == offset) {
    return 1;
  }
  /* MOD writes remainder to target, overwriting it */
  if (instr->op == OP_MOD && instr->targets[0].offset == offset) {
    return 1;
  }
  /* Assignment-mode TRANSFER (arg2=1) overwrites rather than adds */
  if (instr->op == OP_TRANSFER && instr->arg2 == 1 && instr->arg == 1 &&
      instr->targets[0].offset == offset) {
    return 1;
  }
  return 0;
}

/*******************************************************************************
 * HELPER: INSTRUCTION USES CELL
 *
 * Returns true if the instruction reads from the cell at the given offset.
 * Used by dead store elimination - if a cell is read before being overwritten,
 * the previous write is not dead.
 *
 * Returns 1 (conservatively "uses") for control flow instructions since we
 * can't track through loops reliably.
 ******************************************************************************/
static int instruction_uses_cell(const Instruction *instr, i32 offset) {
  switch (instr->op) {
  case OP_INC:
  case OP_OUT:
    return instr->offset == offset;

  case OP_SET:
    /* Multi-cell SET might touch this offset */
    if (instr->arg2 > 1) {
      if (instr->stride <= 1) {
        /* Contiguous memset */
        return offset >= instr->offset && offset < instr->offset + instr->arg2;
      }
      /* Strided memset */
      if (offset < instr->offset) {
        return 0;
      }
      i32 diff = offset - instr->offset;
      if (diff % instr->stride != 0) {
        return 0;
      }
      return diff / instr->stride < instr->arg2;
    }
    return 0; /* Single-cell SET doesn't read, only writes */

  case OP_IN:
    return 0; /* IN only writes */

  case OP_DIV:
    return instr->offset == offset; /* DIV reads dividend */

  case OP_MOD:
    return instr->offset == offset; /* MOD reads dividend */

  case OP_TRANSFER:
    /* TRANSFER reads source cell */
    if (instr->offset == offset)
      return 1;
    /* Non-assignment TRANSFER also reads target cells (to add to them) */
    if (instr->arg2 != 1) {
      for (int t = 0; t < instr->arg; t++) {
        if (instr->targets[t].offset == offset)
          return 1;
      }
    }
    return 0;

  /* Control flow - conservatively return 1 (might use any cell) */
  case OP_LOOP:
  case OP_END:
  case OP_RIGHT:
  case OP_SEEK_EMPTY:
    return 1;

  default:
    return 1; /* Conservative default */
  }
}

/*******************************************************************************
 * PASS 8: DEAD STORE ELIMINATION
 *
 * Removes writes to cells that are overwritten before being read.
 *
 * Example:
 *   SET 5 @0, SET 10 @0 → SET 10 @0  (first SET is dead)
 *   INC 3 @0, SET 0 @0  → SET 0 @0   (INC is dead)
 *
 * This pass looks forward from each write to see if there's a later assignment
 * to the same cell with no intervening reads. If so, the earlier write is dead.
 *
 * Control flow (loops) conservatively stops the analysis since we can't
 * reliably track what happens inside loops.
 ******************************************************************************/
void eliminate_dead_stores(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    /* Only check writes (INC modifies, SET overwrites) */
    if ((curr->op == OP_INC || (curr->op == OP_SET && curr->arg2 == 1))) {
      i32 target_offset = curr->offset;
      int is_dead = 0;

      /* Look forward for overwrite or use */
      for (addr_t j = i + 1; j < input->size; j++) {
        const Instruction *future = &input->instructions[j];

        /* Found later assignment - this write is dead */
        if (is_cell_assignment(future, target_offset)) {
          is_dead = 1;
          break;
        }

        /* Found use - this write is live, stop looking */
        if (instruction_uses_cell(future, target_offset)) {
          break;
        }
      }

      if (is_dead) {
        continue; /* Skip this dead instruction */
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS 9: SET + TRANSFER MERGE
 *
 * When a SET is immediately followed by a TRANSFER that writes to the SET's
 * target cell, we can merge the SET value into the TRANSFER as a bias.
 *
 * Pattern: SET v @X, TRANSFER(..., target @X)
 * Result:  TRANSFER(..., target @X with bias += v)
 *
 * This converts the two-instruction sequence into a single operation that
 * sets the target to (source * factor + v) instead of (source * factor).
 *
 * Additionally, if the TRANSFER has multiple targets, we split it:
 * - An assignment TRANSFER for the target that matches the SET
 * - A regular TRANSFER for the remaining targets
 ******************************************************************************/
void optimize_set_transfer_merge(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_SET && curr->arg2 == 1 && i + 1 < input->size) {
      const Instruction *next = &input->instructions[i + 1];

      if (next->op == OP_TRANSFER) {
        /* Find which TRANSFER target matches the SET offset */
        int match_idx = -1;
        for (int t = 0; t < next->arg; t++) {
          if (next->targets[t].offset == curr->offset) {
            match_idx = t;
            break;
          }
        }

        if (match_idx >= 0) {
          /*
           * Emit assignment TRANSFER for the matching target.
           * arg2=1 means "assign" instead of "add".
           * bias includes the SET value.
           */
          output->instructions[out_index].op = OP_TRANSFER;
          output->instructions[out_index].arg = 1;
          output->instructions[out_index].arg2 = 1; /* assignment mode */
          output->instructions[out_index].offset = next->offset;
          output->instructions[out_index].targets[0] = next->targets[match_idx];
          output->instructions[out_index].targets[0].bias += curr->arg;
          out_index++;

          /* Emit regular TRANSFER for remaining targets (if any) */
          int remaining = next->arg - 1;
          if (remaining > 0) {
            output->instructions[out_index].op = OP_TRANSFER;
            output->instructions[out_index].arg = remaining;
            output->instructions[out_index].arg2 = 0; /* additive mode */
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

          i++; /* Skip the TRANSFER we just processed */
          continue;
        }
      }
    }

    output->instructions[out_index++] = *curr;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * HELPER: MERGE INC INTO TRANSFER
 *
 * Attempts to merge an INC instruction into a TRANSFER's bias.
 *
 * Two cases:
 * 1. INC targets one of TRANSFER's destination cells → add to that target's bias
 * 2. INC targets TRANSFER's source cell → scale and add to all targets' biases
 *
 * Returns 1 if merge was successful, 0 otherwise.
 ******************************************************************************/
static int merge_inc_into_transfer(Instruction *transfer,
                                   const Instruction *inc) {
  /* Case 1: INC targets a destination cell */
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

  /* Case 2: INC targets the source cell */
  if (inc->offset == transfer->offset) {
    /*
     * If source was going to be v, it's now v + inc.arg
     * Each target gets (v + inc.arg) * factor = v*factor + inc.arg*factor
     * So we add inc.arg * factor to each bias.
     */
    for (int t = 0; t < transfer->arg; t++) {
      transfer->targets[t].bias += inc->arg * transfer->targets[t].factor;
    }
    return 1;
  }

  return 0;
}

/*******************************************************************************
 * PASS 10: INC + TRANSFER MERGE
 *
 * Merges INC instructions immediately before or after a TRANSFER into the
 * TRANSFER's bias fields.
 *
 * This handles patterns like:
 *   INC 5 @X, TRANSFER(... target @X) → TRANSFER(... target @X, bias += 5)
 *   TRANSFER(... target @X), INC 5 @X → TRANSFER(... target @X, bias += 5)
 *
 * The pass scans backward through recently emitted INCs and forward through
 * upcoming INCs, merging all that can be merged.
 ******************************************************************************/
void optimize_inc_transfer_merge(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_TRANSFER) {
      Instruction merged = *curr;

      /* Scan backward through emitted instructions to find mergeable INCs */
      int *skip = calloc(out_index, sizeof(int));
      for (int j = out_index - 1; j >= 0; j--) {
        if (output->instructions[j].op != OP_INC)
          break; /* Stop at non-INC */

        if (merge_inc_into_transfer(&merged, &output->instructions[j])) {
          skip[j] = 1; /* Mark for removal */
        } else {
          /* Check if this INC interferes (touches source or target cells) */
          const Instruction *inc = &output->instructions[j];
          int interferes = (inc->offset == merged.offset);
          for (int t = 0; t < merged.arg && !interferes; t++) {
            if (inc->offset == merged.targets[t].offset)
              interferes = 1;
          }
          if (interferes)
            break; /* Can't reorder past interfering instruction */
        }
      }

      /* Remove skipped instructions by compacting output */
      addr_t new_out = 0;
      for (addr_t j = 0; j < out_index; j++) {
        if (!skip[j]) {
          output->instructions[new_out++] = output->instructions[j];
        }
      }
      out_index = new_out;
      free(skip);

      /* Scan forward through upcoming instructions to find mergeable INCs */
      while (i + 1 < input->size && input->instructions[i + 1].op == OP_INC) {
        if (merge_inc_into_transfer(&merged, &input->instructions[i + 1])) {
          i++; /* Consumed this INC */
        } else {
          const Instruction *inc = &input->instructions[i + 1];
          int interferes = (inc->offset == merged.offset);
          for (int t = 0; t < merged.arg && !interferes; t++) {
            if (inc->offset == merged.targets[t].offset)
              interferes = 1;
          }
          if (interferes) {
            break; /* Can't reorder past this */
          } else {
            /* Pass through this non-interfering INC */
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

/*******************************************************************************
 * PASS 11: DIVMOD PATTERN ANALYSIS
 *
 * Recognizes a specific loop pattern that implements integer division and
 * modulo operations using the following BF algorithm:
 *
 * The pattern structure is:
 *   [ -          ; loop while dividend != 0, decrement dividend
 *     TRANSFER   ; move value to temp (with bias = divisor-1), copy to quotient
 *     TRANSFER   ; when temp hits 0, restore to source (conditional)
 *     SET 0      ; clear temp
 *   ]
 *
 * This implements: quotient = dividend / divisor, remainder = dividend % divisor
 *
 * The trick is that temp counts down from divisor-1, and each time it hits 0,
 * quotient is incremented and temp is reset. The remainder is what's left in
 * the source position after all complete divisions.
 *
 * Returns 1 if pattern matches, 0 otherwise.
 * Sets output parameters to the relevant offsets.
 ******************************************************************************/
static int analyze_divmod_pattern(const Program *program, addr_t loop_start,
                                  i32 *dividend_off, i32 *divisor,
                                  i32 *quotient_off, i32 *remainder_off,
                                  i32 *temp_off) {
  /* Need exactly 6 instructions: LOOP, INC, TRANSFER, TRANSFER, SET, END */
  if (loop_start + 5 >= program->size) {
    return 0;
  }

  const Instruction *loop_instr = &program->instructions[loop_start];
  const Instruction *inc_instr = &program->instructions[loop_start + 1];
  const Instruction *transfer1 = &program->instructions[loop_start + 2];
  const Instruction *transfer2 = &program->instructions[loop_start + 3];
  const Instruction *set_instr = &program->instructions[loop_start + 4];
  const Instruction *end_instr = &program->instructions[loop_start + 5];

  /* Verify structure */
  if (loop_instr->op != OP_LOOP) {
    return 0;
  }

  if (end_instr->op != OP_END || end_instr->arg != (i32)loop_start) {
    return 0;
  }

  /* Loop must test same cell as END returns to */
  if (end_instr->offset != loop_instr->offset) {
    return 0;
  }

  /* Must decrement dividend by 1 each iteration */
  if (inc_instr->op != OP_INC || inc_instr->arg != -1 ||
      inc_instr->offset != loop_instr->offset) {
    return 0;
  }

  /* First transfer: source -> temp (with bias for countdown) and quotient */
  if (transfer1->op != OP_TRANSFER || transfer1->arg != 2 ||
      transfer1->arg2 != 0) {
    return 0;
  }

  /* Temp target has factor -1 (counts down) */
  if (transfer1->targets[0].factor != -1) {
    return 0;
  }

  /* Quotient target has factor 1 (counts up) */
  if (transfer1->targets[1].factor != 1 || transfer1->targets[1].bias != 0) {
    return 0;
  }

  /* Second transfer: assignment from temp back to source (when temp = 0) */
  if (transfer2->op != OP_TRANSFER || transfer2->arg != 1 ||
      transfer2->arg2 != 1) {
    return 0;
  }

  /* Source of second transfer is temp cell */
  if (transfer2->offset != transfer1->targets[0].offset) {
    return 0;
  }

  /* Destination is back to original source, factor 1 */
  if (transfer2->targets[0].offset != transfer1->offset ||
      transfer2->targets[0].factor != 1) {
    return 0;
  }

  /* SET clears temp cell */
  if (set_instr->op != OP_SET || set_instr->arg != 0 || set_instr->arg2 != 1) {
    return 0;
  }

  if (set_instr->offset != transfer1->targets[0].offset) {
    return 0;
  }

  /* Extract the parameters */
  *dividend_off = loop_instr->offset;
  *divisor = transfer1->targets[0].bias + 1; /* bias is divisor-1 */
  *quotient_off = transfer1->targets[1].offset;
  *remainder_off = transfer1->offset;
  *temp_off = transfer1->targets[0].offset;

  /* Divisor must be at least 2 for meaningful division */
  if (*divisor < 2) {
    return 0;
  }

  return 1;
}

/*******************************************************************************
 * PASS 11 (MAIN): DIVMOD OPTIMIZATION
 *
 * Replaces divmod loop patterns with OP_DIV and OP_MOD instructions.
 *
 * Before: Complex loop implementing division
 * After:  DIV (quotient += dividend / divisor)
 *         MOD (remainder = dividend % divisor)
 *         SET 0 (clear dividend)
 ******************************************************************************/
void optimize_divmod(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction *instr = &input->instructions[in_index];

    if (instr->op == OP_LOOP) {
      i32 dividend_off, divisor, quotient_off, remainder_off, temp_off;

      if (analyze_divmod_pattern(input, in_index, &dividend_off, &divisor,
                                 &quotient_off, &remainder_off, &temp_off)) {
        /* Emit DIV instruction */
        output->instructions[out_index].op = OP_DIV;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = quotient_off;
        out_index++;

        /* Emit MOD instruction */
        output->instructions[out_index].op = OP_MOD;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = remainder_off;
        out_index++;

        /* Clear the dividend cell */
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0;
        output->instructions[out_index].arg2 = 1;
        output->instructions[out_index].offset = dividend_off;
        out_index++;

        in_index += 5; /* Skip the 6 instructions of the pattern */
        continue;
      }
    }

    output->instructions[out_index] = *instr;
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS 12: TEMP CELL ELIMINATION
 *
 * Recognizes patterns where a value is written to a temporary cell, immediately
 * transferred to its final destination, and the temp is later zeroed.
 *
 * Pattern:
 *   WRITE temp_cell (MOD, DIV, IN, or SET)
 *   TRANSFER temp → final (with factor=1, bias=0)
 *   ... other instructions not using temp ...
 *   SET 0 @ temp
 *
 * Optimized to:
 *   WRITE final_cell
 *   ... other instructions ...
 *   SET 0 @ temp
 *
 * This eliminates the intermediate transfer, writing directly to the final
 * destination.
 ******************************************************************************/
void optimize_eliminate_temp_cells(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    i32 temp_off;
    int is_candidate = 0;

    /* Identify instructions that write to a potential temp cell */
    if (curr->op == OP_MOD || curr->op == OP_DIV) {
      temp_off = curr->targets[0].offset;
      is_candidate = 1;
    } else if (curr->op == OP_IN || (curr->op == OP_SET && curr->arg2 == 1)) {
      temp_off = curr->offset;
      is_candidate = 1;
    }

    if (is_candidate && i + 1 < input->size) {
      const Instruction *next = &input->instructions[i + 1];

      /* Check for identity transfer: temp → final with factor=1, bias=0 */
      if (next->op == OP_TRANSFER && next->arg == 1 && next->arg2 == 1 &&
          next->offset == temp_off && next->targets[0].factor == 1 &&
          next->targets[0].bias == 0) {
        i32 final_off = next->targets[0].offset;

        /* Look for terminating SET 0 */
        addr_t set_idx = 0;
        int can_optimize = 0;

        for (addr_t j = i + 2; j < input->size; j++) {
          const Instruction *future = &input->instructions[j];

          /* Found the terminating SET 0 */
          if (future->op == OP_SET && future->arg == 0 && future->arg2 == 1 &&
              future->offset == temp_off) {
            set_idx = j;
            can_optimize = 1;
            break;
          }

          /* Temp cell is used - can't eliminate */
          if ((future->op == OP_INC || future->op == OP_OUT ||
               future->op == OP_DIV || future->op == OP_MOD ||
               future->op == OP_IN) &&
              future->offset == temp_off) {
            break;
          }

          if (future->op == OP_TRANSFER && future->offset == temp_off) {
            break;
          }

          /* Control flow - stop analysis */
          if (future->op == OP_LOOP || future->op == OP_END ||
              future->op == OP_RIGHT || future->op == OP_SEEK_EMPTY) {
            break;
          }
        }

        if (can_optimize) {
          /* Emit original instruction but targeting final cell directly */
          output->instructions[out_index] = *curr;
          if (curr->op == OP_MOD || curr->op == OP_DIV) {
            output->instructions[out_index].targets[0].offset = final_off;
          } else {
            output->instructions[out_index].offset = final_off;
          }
          out_index++;

          /* Copy intervening instructions (between transfer and SET) */
          for (addr_t j = i + 2; j < set_idx; j++) {
            output->instructions[out_index++] = input->instructions[j];
          }

          /* Keep the SET 0 (temp cell still needs to be zeroed) */
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

/*******************************************************************************
 * PASS 13: TRANSFER CHAIN OPTIMIZATION
 *
 * This is a more sophisticated version of temp cell elimination that handles
 * chains of transfers through temporary cells.
 *
 * Pattern:
 *   TRANSFER source → temp (and possibly other targets)
 *   ... transfers from temp to other cells ...
 *   SET 0 @ temp
 *
 * The optimization composes the factors and biases to eliminate the temp cell,
 * transferring directly from source to final destinations.
 *
 * Example:
 *   TRANSFER @0 → @1 (factor=2), @2 (factor=3)    ; @1 is temp
 *   TRANSFER @1 → @3 (factor=4)
 *   SET 0 @ @1
 *
 * Becomes:
 *   TRANSFER @0 → @2 (factor=3), @3 (factor=2*4=8)
 *
 * The composed factor is source_to_temp_factor * temp_to_final_factor.
 * The composed bias includes both original biases appropriately scaled.
 ******************************************************************************/
void optimize_transfer_chain(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    /* Look for additive TRANSFER (arg2=0) */
    if (curr->op == OP_TRANSFER && curr->arg2 == 0) {
      i32 source_off = curr->offset;

      /* Try each target as a potential temp cell */
      for (int temp_idx = 0; temp_idx < curr->arg; temp_idx++) {
        i32 temp_off = curr->targets[temp_idx].offset;
        i32 F1 = curr->targets[temp_idx].factor;  /* source → temp factor */
        i32 B1 = curr->targets[temp_idx].bias;    /* source → temp bias */

        /* Collect all final targets (non-temp from curr + composed from later) */
        TransferTarget collected[MAX_TRANSFER_TARGETS];
        int num_collected = 0;
        int source_restored = 0;

        /* Add non-temp targets from curr */
        for (int t = 0; t < curr->arg; t++) {
          if (t != temp_idx && num_collected < MAX_TRANSFER_TARGETS) {
            collected[num_collected++] = curr->targets[t];
          }
        }

        addr_t set_idx = 0;
        int valid = 1;

        /* Scan forward for transfers from temp and terminating SET */
        for (addr_t j = i + 1; j < input->size && valid; j++) {
          const Instruction *future = &input->instructions[j];

          /* Found terminating SET 0 - we can optimize */
          if (future->op == OP_SET && future->arg == 0 && future->arg2 == 1 &&
              future->offset == temp_off) {
            set_idx = j;
            break;
          }

          /* Additive transfer FROM temp - collect with composed factors */
          if (future->op == OP_TRANSFER && future->offset == temp_off &&
              future->arg2 == 0) {
            for (int t = 0; t < future->arg; t++) {
              if (num_collected >= MAX_TRANSFER_TARGETS) {
                valid = 0;
                break;
              }
              /*
               * Composition: temp = src*F1 + B1
               * final += temp*Ft + Bt = src*(F1*Ft) + (B1*Ft + Bt)
               */
              collected[num_collected].offset = future->targets[t].offset;
              collected[num_collected].factor = F1 * future->targets[t].factor;
              collected[num_collected].bias =
                  B1 * future->targets[t].factor + future->targets[t].bias;

              /* Check if this restores the source (temp → source with factor 1) */
              if (future->targets[t].offset == source_off &&
                  collected[num_collected].factor == 1 &&
                  collected[num_collected].bias == 0) {
                source_restored = 1;
                num_collected++; /* Still collect, will filter if needed */
              } else {
                num_collected++;
              }
            }
            continue;
          }

          /* Assignment transfer FROM temp */
          if (future->op == OP_TRANSFER && future->offset == temp_off &&
              future->arg2 == 1 && future->arg == 1) {
            /* If it restores source exactly, that's OK (non-destructive pattern) */
            i32 F2 = future->targets[0].factor;
            i32 B2 = future->targets[0].bias;
            if (future->targets[0].offset == source_off && F1 * F2 == 1 &&
                B1 * F2 + B2 == 0) {
              source_restored = 1;
              continue;
            }

            /* Otherwise, can't handle assignment transfers */
            valid = 0;
            break;
          }

          /* Transfer TO temp invalidates our temp analysis */
          if (future->op == OP_TRANSFER) {
            for (int t = 0; t < future->arg; t++) {
              if (future->targets[t].offset == temp_off) {
                valid = 0;
                break;
              }
            }
            if (!valid)
              break;
            continue;
          }

          /* Other reads/writes to temp invalidate */
          if ((future->op == OP_INC || future->op == OP_OUT ||
               future->op == OP_IN) &&
              future->offset == temp_off) {
            valid = 0;
            break;
          }

          /* Control flow invalidates */
          if (future->op == OP_LOOP || future->op == OP_END ||
              future->op == OP_RIGHT || future->op == OP_SEEK_EMPTY) {
            valid = 0;
            break;
          }
        }

        /*
         * If source was restored, filter it out of collected targets.
         * (source += source*1 + 0 is a no-op we don't want to emit)
         */
        if (valid && set_idx > 0 && source_restored) {
          int new_count = 0;
          for (int t = 0; t < num_collected; t++) {
            if (collected[t].offset != source_off || collected[t].factor != 1 ||
                collected[t].bias != 0) {
              collected[new_count++] = collected[t];
            }
          }
          num_collected = new_count;
        }

        if (valid && set_idx > 0 && num_collected > 0) {
          /* Emit the optimized transfer */
          output->instructions[out_index].op = OP_TRANSFER;
          output->instructions[out_index].arg = num_collected;
          output->instructions[out_index].arg2 = 0;
          output->instructions[out_index].offset = source_off;
          for (int t = 0; t < num_collected; t++) {
            output->instructions[out_index].targets[t] = collected[t];
          }
          out_index++;

          /* Skip to after SET */
          i = set_idx;
          goto next_instruction;
        }
      }
    }

    output->instructions[out_index++] = *curr;
  next_instruction:;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * HELPER: INSTRUCTION TOUCHES OFFSET FOR CANCELLATION
 *
 * Returns true if the instruction might read or write the cell at the given
 * offset. Used by INC cancellation to determine when INC merging must stop.
 *
 * This is more conservative than instruction_uses_cell because we need to
 * stop if ANY interaction with the cell occurs, not just reads.
 ******************************************************************************/
static int instr_touches_offset_for_cancel(const Instruction *instr,
                                           const i32 offset) {
  switch (instr->op) {
  case OP_INC:
    return instr->offset == offset;
  case OP_SET:
    if (instr->arg2 == 1) {
      return instr->offset == offset;
    }
    /* Multi-cell SET - check if offset is in range */
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
  /* Control flow - conservative, touches everything */
  case OP_LOOP:
  case OP_END:
  case OP_RIGHT:
  case OP_SEEK_EMPTY:
    return 1;
  default:
    return 1;
  }
}

/*******************************************************************************
 * PASS 14: INC CANCELLATION
 *
 * Merges non-adjacent INC instructions on the same cell when no intervening
 * instructions interfere.
 *
 * Example:
 *   INC 3 @0, INC 2 @1, INC -3 @0 → INC 2 @1  (the @0 INCs cancel)
 *
 * The pass works by:
 * 1. For each INC, look forward for another INC on the same cell
 * 2. If found with no interference, mark the first for skipping and accumulate
 *    its value into the second
 * 3. Emit non-skipped instructions with accumulated adjustments
 *
 * INCs that completely cancel (sum to 0) are eliminated entirely.
 ******************************************************************************/
void optimize_inc_cancellation(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  int *skip = calloc(input->size, sizeof(int));
  i32 *adjust = calloc(input->size, sizeof(i32));

  /* First pass: identify which INCs can be merged forward */
  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *instr = &input->instructions[i];
    if (instr->op != OP_INC) {
      continue;
    }

    i32 offset = instr->offset;

    /* Look for next INC on same cell */
    for (addr_t j = i + 1; j < input->size; j++) {
      const Instruction *next = &input->instructions[j];

      if (next->op == OP_INC && next->offset == offset) {
        /* Found mergeable INC - mark current for skip, add to next */
        skip[i] = 1;
        adjust[j] += instr->arg;
        break;
      }

      /* Stop if instruction interferes with this cell */
      if (instr_touches_offset_for_cancel(next, offset)) {
        break;
      }
    }
  }

  /* Second pass: emit instructions with adjustments */
  for (addr_t i = 0; i < input->size; i++) {
    if (skip[i]) {
      continue;
    }

    Instruction out_instr = input->instructions[i];
    if (out_instr.op == OP_INC) {
      out_instr.arg += adjust[i];
      /* Skip if INC now has no effect */
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

/*******************************************************************************
 * MAIN OPTIMIZATION DRIVER
 *
 * Runs all optimization passes in a fixed order, iterating until the program
 * stops changing (fixed point) or 10 iterations are reached.
 *
 * Pass Order Rationale:
 * 1. Instruction folding first (simplifies everything else)
 * 2. Idiom recognition (zeroing, memset, seek) creates new instruction types
 * 3. Transfer optimization (major optimization, creates TRANSFER ops)
 * 4. SET+INC merge (constant folding after transfers)
 * 5. Offset threading (eliminates most RIGHT instructions)
 * 6. Divmod (specialized pattern recognition)
 * 7. Dead store elimination (cleanup after other passes)
 * 8. SET/INC + TRANSFER merge (algebraic simplification)
 * 9. Transfer chain optimization (eliminate temp cells)
 * 10. INC cancellation (final cleanup)
 *
 * Multiple iterations are needed because some passes enable others:
 * - Transfer optimization creates opportunities for SET+INC merge
 * - Dead store elimination after other passes removes more stores
 * - Transfer chain optimization may enable more dead store elimination
 ******************************************************************************/
void optimize_program(Program *program) {
  Program *optimized = malloc(sizeof(Program));
  Program *before_pass = malloc(sizeof(Program));

  for (int iteration = 0; iteration < 10; iteration++) {
    memcpy(before_pass, program, sizeof(Program));

    /* Pass 1a: Merge consecutive RIGHT instructions */
    merge_consecutive_right_inc(optimized, program, OP_RIGHT);
    *program = *optimized;

    /* Pass 1b: Merge consecutive INC instructions */
    merge_consecutive_right_inc(optimized, program, OP_INC);
    *program = *optimized;

    /* Pass 2: Recognize [-] as SET 0 */
    create_zeroing_sets(optimized, program);
    *program = *optimized;

    /* Pass 3: Combine SET sequences into memset */
    optimize_memset(optimized, program);
    *program = *optimized;

    /* Pass 4: Recognize [>] as seek empty */
    optimize_seek_empty(optimized, program);
    *program = *optimized;

    /* Pass 5: Recognize transfer loops */
    optimize_multi_transfer(optimized, program);
    *program = *optimized;

    /* Pass 6: Merge SET with following INCs */
    optimize_set_inc_merge(optimized, program);
    *program = *optimized;

    /* Pass 7: Convert pointer moves to offsets */
    optimize_offsets(optimized, program);
    *program = *optimized;

    /* Pass 11: Recognize divmod patterns */
    optimize_divmod(optimized, program);
    *program = *optimized;

    /* Pass 8: Remove dead stores */
    eliminate_dead_stores(optimized, program);
    *program = *optimized;

    /* Pass 9: Merge SET with TRANSFER */
    optimize_set_transfer_merge(optimized, program);
    *program = *optimized;

    /* Pass 10: Merge INC with TRANSFER */
    optimize_inc_transfer_merge(optimized, program);
    *program = *optimized;

    /* Pass 13: Optimize transfer chains */
    optimize_transfer_chain(optimized, program);
    *program = *optimized;

    /* Pass 12: Eliminate temp cells */
    optimize_eliminate_temp_cells(optimized, program);
    *program = *optimized;

    /* Pass 14: Cancel/merge non-adjacent INCs */
    optimize_inc_cancellation(optimized, program);
    *program = *optimized;

    /* Check if we've reached a fixed point */
    const int changed = memcmp(before_pass, program, sizeof(Program)) != 0;
    if (!changed) {
      break;
    }
  }

  free(optimized);
  free(before_pass);
}
