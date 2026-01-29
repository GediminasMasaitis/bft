/**
 * optimizer.c - Brainfuck Optimizer
 *
 * This file implements a multi-pass optimization pipeline for Brainfuck
 * programs. The optimizer transforms naive Brainfuck instructions into
 * higher-level operations that can be executed more efficiently or compiled to
 * better machine code.
 *
 * =============================================================================
 * INSTRUCTION FIELD REFERENCE
 * =============================================================================
 *
 * Each Instruction has: op, arg, arg2, offset, stride, targets[]
 *
 * OP_RIGHT: Move data pointer
 *   arg    = amount to move (positive=right, negative=left)
 *   arg2   = (unused)
 *   offset = (unused)
 *   Effect: dp += arg
 *
 * OP_INC: Increment/decrement cell
 *   arg    = amount to add (positive=increment, negative=decrement)
 *   arg2   = (unused)
 *   offset = cell offset from dp
 *   Effect: dp[offset] += arg
 *
 * OP_OUT: Output cell as character
 *   arg    = (unused)
 *   arg2   = (unused)
 *   offset = cell offset from dp
 *   Effect: putchar(dp[offset])
 *
 * OP_IN: Input character to cell
 *   arg    = (unused)
 *   arg2   = (unused)
 *   offset = cell offset from dp
 *   Effect: dp[offset] = getchar()
 *
 * OP_LOOP: Begin loop (jump to END if cell is zero)
 *   arg    = index of matching END instruction
 *   arg2   = (unused)
 *   offset = cell offset to test
 *   Effect: if (dp[offset] == 0) jump to arg
 *
 * OP_END: End loop (jump to LOOP if cell is non-zero)
 *   arg    = index of matching LOOP instruction
 *   arg2   = (unused)
 *   offset = cell offset to test
 *   Effect: if (dp[offset] != 0) jump to arg
 *
 * OP_SET: Set cell(s) to a value
 *   arg    = value to set
 *   arg2   = count of cells (1 for single cell)
 *   offset = starting cell offset from dp
 *   stride = distance between cells (for arg2 > 1)
 *   Effect: if (arg2 <= 1) dp[offset] = arg
 *           else for i in 0..arg2-1: dp[offset + i*stride] = arg
 *
 * OP_SEEK_EMPTY: Scan for zero cell
 *   arg    = stride (amount to move each step, positive or negative)
 *   arg2   = (unused)
 *   offset = cell offset to test
 *   Effect: while (dp[offset] != 0) dp += arg
 *
 * OP_TRANSFER: Transfer/multiply value to target cells
 *   arg    = number of targets
 *   arg2   = mode: 0=additive (+=), 1=assignment (=)
 *   offset = source cell offset
 *   targets[i].offset = target cell offset
 *   targets[i].factor = multiplication factor
 *   targets[i].bias   = constant to add
 *   Effect: src = dp[offset]
 *           for each target t:
 *             if (arg2 == 1): dp[t.offset] = src * t.factor + t.bias
 *             else:           dp[t.offset] += src * t.factor + t.bias
 *   NOTE: Source cell is READ but NOT MODIFIED
 *
 * OP_DIV: Integer division (ADDS to quotient)
 *   arg    = divisor
 *   arg2   = (unused)
 *   offset = dividend cell offset
 *   targets[0].offset = quotient cell offset
 *   Effect: dp[targets[0].offset] += dp[offset] / arg
 *
 * OP_MOD: Integer modulo (ASSIGNS to remainder)
 *   arg    = divisor
 *   arg2   = (unused)
 *   offset = dividend cell offset
 *   targets[0].offset = remainder cell offset
 *   Effect: dp[targets[0].offset] = dp[offset] % arg
 *
 * =============================================================================
 * OPTIMIZATION PASSES
 * =============================================================================
 *
 * 1. INSTRUCTION FOLDING: Consecutive identical operations are merged
 *    (e.g., "+++" becomes INC with arg=3)
 *
 * 2. IDIOM RECOGNITION: Common patterns are replaced with single instructions
 *    (e.g., "[-]" becomes SET 0, "[>]" becomes SEEK_EMPTY)
 *
 * 3. OFFSET THREADING: Pointer movements are converted to offsets on operations
 *    (e.g., ">+<" becomes INC at offset 1, eliminating the pointer moves)
 *
 * 4. LOOP ANALYSIS: Transfer loops like "[->+<]" are converted to arithmetic
 *    operations that don't require iteration
 *
 * 5. DEAD CODE ELIMINATION: Writes that are overwritten before being read are
 * removed
 *
 * 6. ALGEBRAIC SIMPLIFICATION: Operations are merged and simplified where
 * possible
 *
 * The optimizer runs multiple iterations until the program stops changing
 * (fixed point) or a maximum of 10 iterations is reached.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "machine.h"
#include "optimizer.h"

/*******************************************************************************
 * PASS: INSTRUCTION FOLDING (merge_consecutive_right_inc)
 *
 * Merges consecutive operations of the same type AND same offset into a single
 * operation with an accumulated count.
 *
 * This function is called twice per iteration:
 *   1. With op=OP_RIGHT: merges consecutive pointer movements
 *   2. With op=OP_INC: merges consecutive increments/decrements at same offset
 *
 * Examples (when called with OP_RIGHT):
 *   > > > >     →  RIGHT with arg=4
 *   < <         →  RIGHT with arg=-2
 *
 * Examples (when called with OP_INC):
 *   + + + + +   →  INC with arg=5 (at offset 0)
 *   - - -       →  INC with arg=-3 (at offset 0)
 *
 * Note: Only consecutive instructions with MATCHING OFFSETS are merged.
 * After the offset threading pass, INCs at different offsets won't merge
 * here - that's handled by optimize_inc_cancellation instead.
 ******************************************************************************/
void merge_consecutive_right_inc(Program *output, const Program *input,
                                 const op_t op) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    if (instr.op == op) {
      i32 count = instr.arg;
      i32 offset = instr.offset;

      /* Accumulate consecutive operations of same type AND same offset */
      while (in_index + 1 < input->size) {
        const Instruction *next = &input->instructions[in_index + 1];

        if (next->op != instr.op || next->offset != offset) {
          break;
        }

        count += next->arg;
        in_index++;
      }

      /* Skip emitting no-ops (e.g., when +5 and -5 cancel out) */
      if (count != 0) {
        output->instructions[out_index].op = instr.op;
        output->instructions[out_index].arg = count;
        output->instructions[out_index].offset = offset;
        out_index++;
      }
    } else {
      output->instructions[out_index] = instr;
      out_index++;
    }
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS: ZEROING LOOP DETECTION (create_zeroing_sets)
 *
 * Recognizes the "[-]" and "[+]" idioms which set a cell to zero.
 *
 * Pattern detected: [ INC(n) ] where n is ODD
 *
 * Why must n be odd?
 * In 8-bit unsigned arithmetic (0-255), repeatedly adding an odd number will
 * eventually hit zero because gcd(odd, 256) = 1, meaning the sequence cycles
 * through all 256 values. For even numbers, gcd(even, 256) > 1, so the
 * sequence may never reach 0 depending on the starting value.
 *
 * Examples:
 *   [-]     → INC arg=-1 (odd)  → SET 0  ✓
 *   [+]     → INC arg=1 (odd)   → SET 0  ✓
 *   [--]    → INC arg=-2 (even) → NOT optimized (might not terminate)
 *   [---]   → INC arg=-3 (odd)  → SET 0  ✓
 *
 * Output: OP_SET with arg=0 (value), arg2=1 (count of 1 cell)
 ******************************************************************************/
void create_zeroing_sets(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    /* Pattern: LOOP, INC(odd), END */
    if (instr.op == OP_LOOP && in_index + 2 < input->size &&
        input->instructions[in_index + 1].op == OP_INC &&
        (input->instructions[in_index + 1].arg & 1) && /* LSB=1 means odd */
        input->instructions[in_index + 2].op == OP_END) {

      output->instructions[out_index].op = OP_SET;
      output->instructions[out_index].arg = 0;  /* value to set */
      output->instructions[out_index].arg2 = 1; /* count = 1 cell */
      in_index += 2;                            /* Skip the INC and END */
    } else {
      output->instructions[out_index] = instr;
    }
    out_index++;
  }

  output->size = out_index;
  program_calculate_loops(output);
}

/*******************************************************************************
 * PASS: MEMSET OPTIMIZATION (optimize_memset)
 *
 * Combines sequences of SET instructions separated by constant pointer moves
 * into a single bulk SET operation.
 *
 * Pattern: SET(v), RIGHT(s), SET(v), RIGHT(s), SET(v), ...
 *          (all same value v, consistent stride s)
 *
 * This commonly occurs when initializing arrays, e.g., clearing multiple cells:
 *   [-]>[-]>[-]>[-]  →  (after zeroing pass) SET 0, >, SET 0, >, SET 0, >, SET
 *0 →  SET 0 with arg2=4, stride=1, then RIGHT 3
 *
 * A final RIGHT instruction is emitted to maintain correct pointer position.
 * In the original sequence, there are (count-1) RIGHTs, so pointer moves
 * by (count-1)*stride total.
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

      /* Look for pattern: RIGHT, SET(same value), RIGHT, SET(same value), ...
       */
      addr_t j = in_index;
      while (j + 2 < input->size) {
        const Instruction *right = &input->instructions[j + 1];
        const Instruction *next_set = &input->instructions[j + 2];

        if (right->op == OP_RIGHT && next_set->op == OP_SET &&
            next_set->arg == in_set_val) {
          /* First pair establishes the stride */
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

      /* Only emit combined SET if we found 2+ cells */
      if (count >= 2) {
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = in_set_val;
        output->instructions[out_index].arg2 = count;
        output->instructions[out_index].stride = stride;
        out_index++;

        /*
         * Emit RIGHT to position pointer correctly.
         * Original: SET, RIGHT, SET, RIGHT, ..., SET (no trailing RIGHT)
         * Net movement: (count-1) * stride
         */
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
 * PASS: SEEK EMPTY OPTIMIZATION (optimize_seek_empty)
 *
 * Recognizes scanning loops that search left or right for a zero cell.
 *
 * Pattern: [ RIGHT(n) ]
 *
 * This loop moves the pointer by n cells each iteration until finding a zero.
 * Common uses:
 *   - [>] scans right for next zero cell (stride = 1)
 *   - [<] scans left for previous zero cell (stride = -1)
 *   - [>>] scans right, skipping every other cell (stride = 2)
 *
 * Output: OP_SEEK_EMPTY with arg = stride (positive=right, negative=left)
 ******************************************************************************/
void optimize_seek_empty(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  for (addr_t in_index = 0; in_index < input->size; in_index++) {
    const Instruction instr = input->instructions[in_index];

    /* Pattern: LOOP, RIGHT, END */
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
 * Returns the number of instructions in a loop, including the opening LOOP
 * and closing END. Handles nested loops correctly using depth tracking.
 *
 * Used by optimize_multi_transfer to skip over processed loops.
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
 * PASS: TRANSFER LOOP ANALYSIS (analyze_multi_transfer)
 *
 * Analyzes a loop to determine if it's a "transfer loop" - a loop that
 * distributes the value of one cell to multiple target cells while
 * decrementing the source to zero.
 *
 * ALGORITHM:
 * 1. Walk through the loop body, tracking relative position (current_offset)
 * 2. For each INC instruction, record which offset it modifies and by how much
 * 3. Verify that:
 *    a) The loop is "balanced" - pointer returns to start (current_offset == 0)
 *    b) The source cell (offset 0) is decremented by an odd amount per
 *iteration c) Only RIGHT and INC operations are present (no I/O, nested loops,
 *etc.)
 *
 * EXAMPLE: [->+>++<<] (copy to dp+1, add 2x to dp+2)
 *   - Offset 0 (source): INC -1 per iteration (decrement by 1)
 *   - Offset 1: INC +1 per iteration → factor = 1
 *   - Offset 2: INC +2 per iteration → factor = 2
 *
 * FACTOR CALCULATION:
 * If the source decrements by D per iteration and a target increments by F:
 *   - The loop runs for (source_value / D) iterations (in modular arithmetic)
 *   - Target accumulates (source_value / D) * F = source_value * (F/D)
 *   - So the effective factor stored is F/D
 *
 * For this to work cleanly, F must be divisible by D when D > 1.
 *
 * WHY ODD DECREMENT?
 * An odd decrement guarantees termination because gcd(odd, 256) = 1.
 * This means the sequence source, source-D, source-2D, ... (mod 256)
 * will eventually hit 0 regardless of starting value.
 *
 * Returns: number of transfer targets (0 if not a valid transfer loop)
 ******************************************************************************/
static int analyze_multi_transfer(const Program *program, addr_t loop_start,
                                  TransferTarget *targets) {
  addr_t i = loop_start;

  if (program->instructions[i].op != OP_LOOP) {
    return 0;
  }
  i++;

  /* Track relative position within loop body */
  i32 current_offset = 0;

  /* Record net change to each offset touched in the loop */
  i32 offsets[MAX_TRANSFER_TARGETS + 1]; /* +1 for source cell at offset 0 */
  i32 factors[MAX_TRANSFER_TARGETS + 1]; /* net increment per iteration */
  int num_entries = 0;

  /* Scan loop body */
  while (i < program->size && program->instructions[i].op != OP_END) {
    const Instruction *instr = &program->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      current_offset += instr->arg;
      break;

    case OP_INC: {
      /* Find existing entry for this offset or create new one */
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
          return 0; /* Too many distinct offsets */
        offsets[num_entries] = current_offset;
        factors[num_entries] = instr->arg;
        num_entries++;
      }
    } break;

    /* These operations disqualify the loop from being a simple transfer */
    case OP_LOOP: /* Nested loops are too complex */
    case OP_END:
    case OP_IN: /* I/O has side effects */
    case OP_OUT:
    case OP_SET: /* Already-optimized ops shouldn't appear here */
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

  /* Loop must be balanced: pointer returns to starting position */
  if (current_offset != 0) {
    return 0;
  }

  /* Find change to source cell (offset 0) */
  int source_change = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] == 0) {
      source_change = factors[j];
      break;
    }
  }

  /* Source must be decremented (negative change) */
  if (source_change >= 0) {
    return 0;
  }

  /* Decrement must be odd to guarantee termination */
  if ((source_change & 1) == 0) {
    return 0;
  }

  const i32 decrement_mag = -source_change; /* Make positive */

  /* Build list of transfer targets (non-source cells with non-zero change) */
  int num_targets = 0;
  for (int j = 0; j < num_entries; j++) {
    if (offsets[j] != 0 && factors[j] != 0) {
      /*
       * When decrement > 1, the per-iteration increment must divide evenly.
       * E.g., if source decrements by 3 and target increments by 6,
       * then effective factor is 6/3 = 2.
       */
      if (decrement_mag > 1 && (factors[j] % decrement_mag) != 0) {
        return 0;
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
 * PASS: TRANSFER LOOP OPTIMIZATION (optimize_multi_transfer)
 *
 * Replaces transfer loops with OP_TRANSFER instructions.
 *
 * A transfer loop like [->+>++<<] becomes:
 *   TRANSFER with:
 *     - arg = 2 (two targets)
 *     - arg2 = 0 (additive mode, the default)
 *     - offset = 0 (source at dp+0)
 *     - targets[0] = {offset=1, factor=1, bias=0}
 *     - targets[1] = {offset=2, factor=2, bias=0}
 *   SET 0 at offset 0 (zero the source)
 *
 * IMPORTANT: OP_TRANSFER only READS the source cell - it does NOT modify it.
 * The separate SET 0 instruction zeros the source afterward.
 *
 * This converts an O(N) loop into O(1) arithmetic operations, where N is
 * the initial value of the source cell.
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
        output->instructions[out_index].arg =
            num_targets; /* number of targets */
        /* arg2 defaults to 0 (additive mode), offset defaults to 0 (source) */

        for (int t = 0; t < num_targets; t++) {
          output->instructions[out_index].targets[t] = targets[t];
        }
        out_index++;

        /* Emit SET 0 to zero the source cell (TRANSFER doesn't modify source)
         */
        output->instructions[out_index].op = OP_SET;
        output->instructions[out_index].arg = 0;  /* value */
        output->instructions[out_index].arg2 = 1; /* count = 1 cell */
        output->instructions[out_index].offset = 0;
        out_index++;

        /* Skip past the entire loop */
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
 * PASS: SET + INC MERGE (optimize_set_inc_merge)
 *
 * Folds INC instructions that follow a SET into the SET's value.
 * Also reorders independent instructions to maximize folding opportunities.
 *
 * Basic pattern: SET v @X, INC n @X → SET (v+n) @X
 *
 * Extended pattern with reordering:
 *   SET 5 @0, INC 2 @1, INC 3 @0
 *   → INC 2 @1, SET 8 @0  (the @0 operations are merged)
 *
 * Instructions that don't touch the SET's target cell can be moved before
 * the SET to enable more merging. This includes:
 *   - INC/SET/OUT on different offsets
 *   - TRANSFER that doesn't read/write the target offset
 *   - DIV/MOD that don't involve the target offset
 *
 * The pass stops merging when it encounters:
 *   - Another SET to the same cell (would override our value)
 *   - Control flow (LOOP, END)
 *   - Input (IN changes data unpredictably)
 *   - RIGHT/SEEK_EMPTY (changes pointer position)
 ******************************************************************************/
void optimize_set_inc_merge(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  addr_t i = 0;
  while (i < original->size) {
    const Instruction *curr = &original->instructions[i];

    /* Look for single-cell SET instructions (arg2 == 1) */
    if (curr->op == OP_SET && curr->arg2 == 1) {
      i32 value = curr->arg;
      const i32 target_offset = curr->offset;

      addr_t j = i + 1;
      while (j < original->size) {
        const Instruction *next = &original->instructions[j];

        /* INC on same cell: fold into SET value */
        if (next->op == OP_INC && next->offset == target_offset) {
          value += next->arg;
          j++;
          continue;
        }

        /* Another SET to same cell: stop (it would override us) */
        if (next->op == OP_SET && next->offset == target_offset) {
          break;
        }

        /* Control flow / position changes: stop */
        if (next->op == OP_LOOP || next->op == OP_END || next->op == OP_IN ||
            next->op == OP_RIGHT || next->op == OP_SEEK_EMPTY) {
          break;
        }

        /* OUT on different cell: can be moved before SET */
        if (next->op == OP_OUT && next->offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* TRANSFER: movable if it doesn't touch our target cell */
        if (next->op == OP_TRANSFER) {
          int touches_target = (next->offset == target_offset);
          for (int t = 0; t < next->arg && !touches_target; t++)
            touches_target = (next->targets[t].offset == target_offset);
          if (!touches_target) {
            output->instructions[out_index++] = *next;
            j++;
            continue;
          }
          break;
        }

        /* DIV/MOD: movable if they don't touch our target */
        if ((next->op == OP_DIV || next->op == OP_MOD) &&
            next->offset != target_offset &&
            next->targets[0].offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* INC/SET on different cell: can be moved before SET */
        /* (OUT @different is already handled above) */
        if ((next->op == OP_INC || next->op == OP_SET) &&
            next->offset != target_offset) {
          output->instructions[out_index++] = *next;
          j++;
          continue;
        }

        /* Unknown op or touches our cell: stop */
        break;
      }

      /* Emit the merged SET with accumulated value */
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
 * Determines if a loop has zero net pointer movement per iteration.
 *
 * A "balanced" loop allows the optimizer to use offsets throughout the loop
 * body without emitting actual pointer movements. This is key to offset
 * threading optimization.
 *
 * The function recursively checks nested loops - they must also be balanced
 * with zero net movement for the outer loop to be considered balanced.
 *
 * Returns: 1 if the loop can be analyzed, 0 otherwise
 * Sets *net_movement to the total pointer displacement per iteration
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
        return 0; /* Can't analyze nested loop */
      }
      if (child_move != 0) {
        return 0; /* Nested loop has unpredictable movement */
      }
      break;

    case OP_END:
      depth--;
      break;

    /* These don't affect pointer position */
    case OP_INC:
    case OP_SET:
    case OP_OUT:
    case OP_IN:
    case OP_TRANSFER:
    case OP_DIV:
    case OP_MOD:
      break;

    /* SEEK_EMPTY has data-dependent movement - can't analyze */
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
 * PASS: OFFSET THREADING (optimize_offsets)
 *
 * Eliminates most pointer movement (RIGHT) instructions by converting them
 * into offsets on other operations.
 *
 * CONCEPT:
 * Instead of physically moving the pointer, we track a "virtual offset" -
 * where the pointer WOULD be if we had executed all the RIGHTs. Operations
 * then use (original_offset + virtual_offset) to access the correct cell.
 *
 * EXAMPLE:
 *   > > + + < <      (move right 2, increment, move back)
 *   Becomes: INC 2 @+2  (increment at offset 2, no actual movement)
 *
 * LOOP HANDLING:
 * - Balanced loops (net movement = 0): We maintain the virtual offset through
 *   the loop. At loop exit, offset is the same as at entry.
 * - Unbalanced loops: We must "materialize" the virtual offset by emitting a
 *   RIGHT before the loop, then reset virtual offset to 0.
 *
 * The loop_entry_offsets stack tracks the virtual offset at each loop entry.
 * A sentinel value (-999999) marks unbalanced loops.
 ******************************************************************************/
void optimize_offsets(Program *output, const Program *original) {
  memset(output, 0, sizeof(*output));

  addr_t out_index = 0;
  i32 virtual_offset = 0;

  /* Stack to remember virtual offset at each loop entry */
  i32 loop_entry_offsets[MAX_CODE_SIZE];
  int loop_stack_size = 0;

  for (addr_t i = 0; i < original->size; i++) {
    const Instruction *instr = &original->instructions[i];

    switch (instr->op) {
    case OP_RIGHT:
      /* Accumulate into virtual offset instead of emitting */
      virtual_offset += instr->arg;
      break;

    /* Operations that can take an offset - add virtual offset */
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
        /* Balanced loop: maintain virtual offset through the loop */
        i32 combined_offset = instr->offset + virtual_offset;
        loop_entry_offsets[loop_stack_size++] = virtual_offset;

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = combined_offset;
        out_index++;
      } else {
        /* Unbalanced loop: must materialize offset before entering */
        if (virtual_offset != 0) {
          output->instructions[out_index].op = OP_RIGHT;
          output->instructions[out_index].arg = virtual_offset;
          output->instructions[out_index].offset = 0;
          out_index++;
          virtual_offset = 0;
        }

        loop_entry_offsets[loop_stack_size++] = -999999; /* Sentinel */

        output->instructions[out_index] = *instr;
        output->instructions[out_index].offset = instr->offset;
        out_index++;
      }
      break;
    }

    case OP_END: {
      i32 entry_virtual_offset = loop_entry_offsets[--loop_stack_size];

      if (entry_virtual_offset == -999999) {
        /* Exiting unbalanced loop: materialize any accumulated offset */
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
         * Exiting balanced loop: restore virtual offset to entry value.
         * Since loop is balanced, pointer is at same position as entry.
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
 * Returns true if the instruction completely overwrites the cell at the given
 * offset, making any previous value irrelevant (dead).
 *
 * Assignment operations:
 *   - SET (single-cell or multi-cell if offset is in range)
 *   - IN (reads from input, replacing cell value)
 *   - MOD (assigns remainder to target)
 *   - Assignment-mode TRANSFER with single target (arg2 == 1, arg == 1)
 ******************************************************************************/
static int is_cell_assignment(const Instruction *instr, i32 offset) {
  if (instr->op == OP_SET) {
    if (instr->arg2 == 1) {
      /* Single-cell SET */
      return instr->offset == offset;
    }
    /* Multi-cell SET: check if offset is in the range being set */
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

/*******************************************************************************
 * HELPER: INSTRUCTION USES CELL
 *
 * Returns true if the instruction reads from (uses) the cell at the given
 *offset.
 *
 * This is used by dead store elimination: if a cell is read between a write
 * and a later overwrite, the earlier write is NOT dead.
 *
 * Returns 1 (conservatively assumes usage) for control flow instructions
 * since we can't reliably track what happens inside loops.
 ******************************************************************************/
static int instruction_uses_cell(const Instruction *instr, i32 offset) {
  switch (instr->op) {
  case OP_INC:
  case OP_OUT:
    return instr->offset == offset;

  case OP_SET:
    return 0; /* SET only writes, never reads */

  case OP_IN:
    return 0; /* IN only writes */

  case OP_DIV:
  case OP_MOD:
    return instr->offset == offset; /* Reads dividend */

  case OP_TRANSFER:
    /* Reads source cell */
    if (instr->offset == offset)
      return 1;
    /* Additive TRANSFER (arg2 == 0) also reads target cells (to add to them) */
    if (instr->arg2 != 1) {
      for (int t = 0; t < instr->arg; t++) {
        if (instr->targets[t].offset == offset)
          return 1;
      }
    }
    return 0;

  /* Control flow - conservative, assume any cell might be used */
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
 * PASS: DEAD STORE ELIMINATION (eliminate_dead_stores)
 *
 * Removes writes to cells that are overwritten before being read.
 *
 * Examples:
 *   SET 5 @0, SET 10 @0 → SET 10 @0  (first SET is dead)
 *   INC 3 @0, SET 0 @0  → SET 0 @0   (INC is dead)
 *
 * For each write (INC or single-cell SET), we scan forward to see if there's
 * a later assignment to the same cell with no intervening reads. If so, the
 * earlier write is dead and can be removed.
 *
 * The scan stops at control flow since we can't track through loops reliably.
 ******************************************************************************/
void eliminate_dead_stores(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    /* Only check writes: INC (modifies) and single-cell SET (assigns) */
    if ((curr->op == OP_INC || (curr->op == OP_SET && curr->arg2 == 1))) {
      i32 target_offset = curr->offset;
      int is_dead = 0;

      for (addr_t j = i + 1; j < input->size; j++) {
        const Instruction *future = &input->instructions[j];

        /* Later assignment to same cell → current write is dead */
        if (is_cell_assignment(future, target_offset)) {
          is_dead = 1;
          break;
        }

        /* Cell is used → current write is NOT dead */
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
 * PASS: SET + TRANSFER MERGE (optimize_set_transfer_merge)
 *
 * When a SET is immediately followed by a TRANSFER that writes to the SET's
 * target cell, we can merge them into a single assignment TRANSFER with bias.
 *
 * Pattern: SET v @X, TRANSFER @src -> ... @X (factor F, bias B) ...
 *
 * Original semantics (with additive TRANSFER):
 *   X = v                    (SET)
 *   X += src * F + B         (TRANSFER additive)
 *   Final: X = v + src * F + B
 *
 * Optimized to assignment TRANSFER (arg2=1):
 *   X = src * F + (B + v)
 *
 * PRECONDITION: This pass assumes the original TRANSFER is additive (arg2=0).
 * If TRANSFER were assignment mode, the SET would be dead code and removed
 * by eliminate_dead_stores, which runs before this pass.
 *
 * If the TRANSFER has multiple targets, we split it:
 *   1. Assignment TRANSFER for the target that matches SET (arg2=1)
 *   2. Additive TRANSFER for remaining targets (arg2=0)
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
          /* Emit assignment TRANSFER for matching target */
          output->instructions[out_index].op = OP_TRANSFER;
          output->instructions[out_index].arg = 1;  /* one target */
          output->instructions[out_index].arg2 = 1; /* assignment mode */
          output->instructions[out_index].offset = next->offset;
          output->instructions[out_index].targets[0] = next->targets[match_idx];
          output->instructions[out_index].targets[0].bias +=
              curr->arg; /* merge SET value */
          out_index++;

          /* Emit additive TRANSFER for remaining targets (if any) */
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

          i++; /* Skip the TRANSFER */
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
 * Attempts to fold an INC instruction into a TRANSFER's bias.
 *
 * CASE 1: INC targets a TRANSFER destination cell
 *   Original: TRANSFER ... target @X (factor F, bias B) ..., then INC n @X
 *   Merged:   TRANSFER ... target @X (factor F, bias B+n) ...
 *   Correctness: target += src*F + B, then target += n
 *                equals target += src*F + (B+n) ✓
 *
 * CASE 2: INC targets the TRANSFER's source cell (INC comes BEFORE TRANSFER)
 *   Original: INC n @src, then TRANSFER @src -> targets (factor F, bias B)
 *   Merged:   TRANSFER @src -> targets (factor F, bias B + n*F)
 *   Correctness: src becomes src+n, targets += (src+n)*F + B = src*F + n*F + B
 *                Merged reads original src: targets += src*F + (B + n*F) ✓
 *
 *   IMPORTANT: The INC is removed, so the source cell is NOT incremented.
 *   This is safe because TRANSFERs from optimize_multi_transfer are always
 *   followed by SET 0 on the source, making the INC's effect irrelevant.
 *
 * Returns 1 if merge was successful, 0 otherwise.
 ******************************************************************************/
static int merge_inc_into_transfer(Instruction *transfer,
                                   const Instruction *inc) {
  /* Case 1: INC on a destination cell */
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

  /* Case 2: INC on source cell */
  if (inc->offset == transfer->offset) {
    for (int t = 0; t < transfer->arg; t++) {
      transfer->targets[t].bias += inc->arg * transfer->targets[t].factor;
    }
    return 1;
  }

  return 0;
}

/*******************************************************************************
 * PASS: INC + TRANSFER MERGE (optimize_inc_transfer_merge)
 *
 * Merges INC instructions adjacent to TRANSFER into the TRANSFER's biases.
 * Scans both backward (INCs before TRANSFER) and forward (INCs after).
 *
 * This handles patterns like:
 *   INC 5 @X, TRANSFER(...target @X) → TRANSFER(...target @X, bias += 5)
 *   TRANSFER(...target @X), INC 5 @X → TRANSFER(...target @X, bias += 5)
 *
 * Merged INCs are removed from the output. For INCs that can't be merged but
 * don't interfere (touch different cells), they pass through unchanged.
 * When an interfering INC is found, scanning in that direction stops.
 ******************************************************************************/
void optimize_inc_transfer_merge(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *curr = &input->instructions[i];

    if (curr->op == OP_TRANSFER) {
      Instruction merged = *curr;

      /* Scan backward through already-emitted INCs */
      int *skip = calloc(out_index, sizeof(int));
      for (int j = out_index - 1; j >= 0; j--) {
        if (output->instructions[j].op != OP_INC)
          break;

        if (merge_inc_into_transfer(&merged, &output->instructions[j])) {
          skip[j] = 1; /* Mark for removal */
        } else {
          /* Check if INC interferes with TRANSFER */
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

      /* Remove skipped INCs by compacting output */
      addr_t new_out = 0;
      for (addr_t j = 0; j < out_index; j++) {
        if (!skip[j]) {
          output->instructions[new_out++] = output->instructions[j];
        }
      }
      out_index = new_out;
      free(skip);

      /* Scan forward through upcoming INCs */
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
            break;
          } else {
            /* Non-interfering, non-mergeable INC: pass through */
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
 * PASS: DIVMOD PATTERN ANALYSIS (analyze_divmod_pattern)
 *
 * Recognizes a specific 6-instruction loop pattern that implements integer
 * division and modulo using a countdown technique.
 *
 * The exact pattern structure is:
 *   LOOP @dividend_off
 *   INC -1 @dividend_off
 *   TRANSFER @remainder_off -> @temp_off (factor=-1, bias=divisor-1),
 *                              @quotient_off (factor=1, bias=0)
 *   TRANSFER @temp_off -> @remainder_off (assignment, factor=1)
 *   SET 0 @temp_off
 *   END @dividend_off
 *
 * The divisor is encoded as (transfer1.targets[0].bias + 1).
 *
 * Returns 1 if pattern matches, 0 otherwise.
 * Sets output parameters to the relevant cell offsets and divisor value.
 ******************************************************************************/
static int analyze_divmod_pattern(const Program *program, addr_t loop_start,
                                  i32 *dividend_off, i32 *divisor,
                                  i32 *quotient_off, i32 *remainder_off,
                                  i32 *temp_off) {
  /* Need exactly 6 instructions */
  if (loop_start + 5 >= program->size) {
    return 0;
  }

  const Instruction *loop_instr = &program->instructions[loop_start];
  const Instruction *inc_instr = &program->instructions[loop_start + 1];
  const Instruction *transfer1 = &program->instructions[loop_start + 2];
  const Instruction *transfer2 = &program->instructions[loop_start + 3];
  const Instruction *set_instr = &program->instructions[loop_start + 4];
  const Instruction *end_instr = &program->instructions[loop_start + 5];

  /* Verify LOOP instruction */
  if (loop_instr->op != OP_LOOP) {
    return 0;
  }

  /* Verify END matches LOOP */
  if (end_instr->op != OP_END || end_instr->arg != (i32)loop_start) {
    return 0;
  }
  if (end_instr->offset != loop_instr->offset) {
    return 0;
  }

  /* Must decrement dividend by 1 */
  if (inc_instr->op != OP_INC || inc_instr->arg != -1 ||
      inc_instr->offset != loop_instr->offset) {
    return 0;
  }

  /* First TRANSFER: 2 targets, additive mode (arg2=0) */
  if (transfer1->op != OP_TRANSFER || transfer1->arg != 2 ||
      transfer1->arg2 != 0) {
    return 0;
  }

  /* First target is temp with factor -1 */
  if (transfer1->targets[0].factor != -1) {
    return 0;
  }

  /* Second target is quotient with factor 1, no bias */
  if (transfer1->targets[1].factor != 1 || transfer1->targets[1].bias != 0) {
    return 0;
  }

  /* Second TRANSFER: assignment (arg2=1) from temp to remainder */
  if (transfer2->op != OP_TRANSFER || transfer2->arg != 1 ||
      transfer2->arg2 != 1) {
    return 0;
  }

  /* Source of second TRANSFER must be temp cell */
  if (transfer2->offset != transfer1->targets[0].offset) {
    return 0;
  }

  /* Destination must be remainder (same as first TRANSFER's source), factor 1
   */
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

  /* Extract values */
  *dividend_off = loop_instr->offset;
  *divisor = transfer1->targets[0].bias + 1; /* divisor encoded as bias+1 */
  *quotient_off = transfer1->targets[1].offset;
  *remainder_off = transfer1->offset; /* source of first TRANSFER */
  *temp_off = transfer1->targets[0].offset;

  /* Divisor must be at least 2 */
  if (*divisor < 2) {
    return 0;
  }

  return 1;
}

/*******************************************************************************
 * PASS: DIVMOD OPTIMIZATION (optimize_divmod)
 *
 * Replaces the divmod loop pattern with OP_DIV and OP_MOD instructions.
 *
 * OP_DIV: dp[quotient_off] += dp[dividend_off] / divisor  (ADDS to quotient)
 * OP_MOD: dp[remainder_off] = dp[dividend_off] % divisor  (ASSIGNS remainder)
 *
 * Also emits SET 0 to clear the dividend (the original loop decremented it to
 *0).
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
        /* Emit DIV: quotient += dividend / divisor */
        output->instructions[out_index].op = OP_DIV;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = quotient_off;
        out_index++;

        /* Emit MOD: remainder = dividend % divisor */
        output->instructions[out_index].op = OP_MOD;
        output->instructions[out_index].offset = dividend_off;
        output->instructions[out_index].arg = divisor;
        output->instructions[out_index].targets[0].offset = remainder_off;
        out_index++;

        /* Clear dividend (loop would have decremented it to 0) */
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
 * PASS: TEMP CELL ELIMINATION (optimize_eliminate_temp_cells)
 *
 * Eliminates temporary cells when a value is written and immediately
 * transferred to its final destination with no other use of the temp.
 *
 * Pattern:
 *   WRITE @temp     (MOD, DIV, IN, or SET)
 *   TRANSFER @temp -> @final (arg=1, arg2=1, factor=1, bias=0)
 *   ... instructions not touching @temp ...
 *   SET 0 @temp
 *
 * Optimized to:
 *   WRITE @final    (directly to final destination)
 *   ... instructions ...
 *   SET 0 @temp     (still emitted to clear temp)
 *
 * This eliminates the intermediate TRANSFER.
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

      /* Check for identity TRANSFER: temp -> final, factor=1, bias=0,
       * assignment */
      if (next->op == OP_TRANSFER && next->arg == 1 && next->arg2 == 1 &&
          next->offset == temp_off && next->targets[0].factor == 1 &&
          next->targets[0].bias == 0) {
        i32 final_off = next->targets[0].offset;

        /* Look for terminating SET 0 @temp */
        addr_t set_idx = 0;
        int can_optimize = 0;

        for (addr_t j = i + 2; j < input->size; j++) {
          const Instruction *future = &input->instructions[j];

          /* Found SET 0 @temp - can optimize */
          if (future->op == OP_SET && future->arg == 0 && future->arg2 == 1 &&
              future->offset == temp_off) {
            set_idx = j;
            can_optimize = 1;
            break;
          }

          /* Temp is used - can't eliminate */
          if ((future->op == OP_INC || future->op == OP_OUT ||
               future->op == OP_DIV || future->op == OP_MOD ||
               future->op == OP_IN) &&
              future->offset == temp_off) {
            break;
          }

          if (future->op == OP_TRANSFER && future->offset == temp_off) {
            break;
          }

          /* Control flow - can't analyze further */
          if (future->op == OP_LOOP || future->op == OP_END ||
              future->op == OP_RIGHT || future->op == OP_SEEK_EMPTY) {
            break;
          }
        }

        if (can_optimize) {
          /* Emit original instruction targeting final cell directly */
          output->instructions[out_index] = *curr;
          if (curr->op == OP_MOD || curr->op == OP_DIV) {
            output->instructions[out_index].targets[0].offset = final_off;
          } else {
            output->instructions[out_index].offset = final_off;
          }
          out_index++;

          /* Copy intervening instructions */
          for (addr_t j = i + 2; j < set_idx; j++) {
            output->instructions[out_index++] = input->instructions[j];
          }

          /* Keep the SET 0 @temp */
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
 * PASS: TRANSFER CHAIN OPTIMIZATION (optimize_transfer_chain)
 *
 * Optimizes chains of transfers through temporary cells by composing the
 * factors and biases, eliminating the intermediate temp.
 *
 * Pattern:
 *   TRANSFER @source -> @temp (factor F1, bias B1), ...other targets
 *   TRANSFER @temp -> @final (factor F2, bias B2)
 *   SET 0 @temp
 *
 * Composition math (additive transfers):
 *   temp += source * F1 + B1
 *   final += temp * F2 + B2
 *
 * If temp starts at 0:
 *   final += (source * F1 + B1) * F2 + B2 = source * (F1*F2) + (B1*F2 + B2)
 *
 * Optimized to:
 *   TRANSFER @source -> @final (factor F1*F2, bias B1*F2+B2), ...other targets
 *
 * The optimization handles multiple transfers from temp and can detect when
 * the source is "restored" (transferred back with factor 1, bias 0),
 * in which case the restoration is filtered out as a no-op.
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
        i32 F1 = curr->targets[temp_idx].factor;
        i32 B1 = curr->targets[temp_idx].bias;

        /* Collect final targets: non-temp from curr + composed from later */
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

        /* Scan for transfers from temp and terminating SET */
        for (addr_t j = i + 1; j < input->size && valid; j++) {
          const Instruction *future = &input->instructions[j];

          /* Found terminating SET 0 @temp */
          if (future->op == OP_SET && future->arg == 0 && future->arg2 == 1 &&
              future->offset == temp_off) {
            set_idx = j;
            break;
          }

          /* Additive TRANSFER FROM temp - compose factors */
          if (future->op == OP_TRANSFER && future->offset == temp_off &&
              future->arg2 == 0) {
            for (int t = 0; t < future->arg; t++) {
              if (num_collected >= MAX_TRANSFER_TARGETS) {
                valid = 0;
                break;
              }
              /* Compose: final += temp*Ft + Bt = source*(F1*Ft) + (B1*Ft + Bt)
               */
              collected[num_collected].offset = future->targets[t].offset;
              collected[num_collected].factor = F1 * future->targets[t].factor;
              collected[num_collected].bias =
                  B1 * future->targets[t].factor + future->targets[t].bias;

              /* Check if this restores source (factor=1, bias=0 to source) */
              if (future->targets[t].offset == source_off &&
                  collected[num_collected].factor == 1 &&
                  collected[num_collected].bias == 0) {
                source_restored = 1;
              }
              num_collected++;
            }
            continue;
          }

          /* Assignment TRANSFER FROM temp */
          if (future->op == OP_TRANSFER && future->offset == temp_off &&
              future->arg2 == 1 && future->arg == 1) {
            /* If it restores source exactly, that's OK */
            i32 F2 = future->targets[0].factor;
            i32 B2 = future->targets[0].bias;
            if (future->targets[0].offset == source_off && F1 * F2 == 1 &&
                B1 * F2 + B2 == 0) {
              source_restored = 1;
              continue;
            }
            /* Can't handle other assignment transfers */
            valid = 0;
            break;
          }

          /* TRANSFER TO temp invalidates our analysis */
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

          /* Other temp uses invalidate */
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

        /* Filter out source restoration (it's a no-op: source += source is
         * wrong) */
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
          /* Emit optimized transfer */
          output->instructions[out_index].op = OP_TRANSFER;
          output->instructions[out_index].arg = num_collected;
          output->instructions[out_index].arg2 = 0;
          output->instructions[out_index].offset = source_off;
          for (int t = 0; t < num_collected; t++) {
            output->instructions[out_index].targets[t] = collected[t];
          }
          out_index++;

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
 * Returns true if the instruction interacts with the cell at the given offset
 * in any way (read or write). Used by INC cancellation to determine when
 * INC merging must stop.
 *
 * More conservative than instruction_uses_cell - we stop at ANY interaction.
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
  /* Control flow - conservatively touches everything */
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
 * PASS: INC CANCELLATION (optimize_inc_cancellation)
 *
 * Merges non-adjacent INC instructions on the same cell when no intervening
 * instructions interfere.
 *
 * Example:
 *   INC 3 @0, INC 2 @1, INC -3 @0 → INC 2 @1  (the @0 INCs cancel to 0)
 *
 * Algorithm:
 * 1. For each INC, scan forward for another INC on the same cell
 * 2. If found with no interference, mark the first INC for skipping and
 *    accumulate its value into the second
 * 3. Emit non-skipped instructions with accumulated adjustments
 *
 * INCs that sum to 0 are eliminated entirely.
 ******************************************************************************/
void optimize_inc_cancellation(Program *output, const Program *input) {
  memset(output, 0, sizeof(*output));
  addr_t out_index = 0;

  int *skip = calloc(input->size, sizeof(int));
  i32 *adjust = calloc(input->size, sizeof(i32));

  /* First pass: identify INCs that can be merged forward */
  for (addr_t i = 0; i < input->size; i++) {
    const Instruction *instr = &input->instructions[i];
    if (instr->op != OP_INC) {
      continue;
    }

    i32 offset = instr->offset;

    for (addr_t j = i + 1; j < input->size; j++) {
      const Instruction *next = &input->instructions[j];

      /* Found another INC on same cell - merge */
      if (next->op == OP_INC && next->offset == offset) {
        skip[i] = 1;
        adjust[j] += instr->arg;
        break;
      }

      /* Interference - can't merge past this */
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
      if (out_instr.arg == 0) {
        continue; /* INC 0 is a no-op */
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
 * MAIN OPTIMIZATION DRIVER (optimize_program)
 *
 * Runs all optimization passes in a fixed order, iterating until the program
 * stops changing (fixed point) or 10 iterations are reached.
 *
 * PASS ORDER RATIONALE:
 * - Instruction folding first (simplifies everything else)
 * - Idiom recognition (zeroing, memset, seek) creates new instruction types
 * - Transfer optimization (major: [->+<] → TRANSFER) - O(N) to O(1)
 * - SET+INC merge (constant folding after transfers)
 * - Offset threading (eliminates most RIGHT instructions)
 * - Divmod detection (specialized pattern, after offsets applied)
 * - Dead store elimination (cleanup, MUST run before SET+TRANSFER merge)
 * - Algebraic simplifications (SET/INC + TRANSFER merges)
 * - Chain/temp optimizations (eliminate intermediate cells)
 * - INC cancellation (final cleanup)
 *
 * Multiple iterations are needed because passes enable each other:
 * - Transfer creates SET 0 which may enable dead store elimination
 * - Dead store elimination may expose new merging opportunities
 * - Chain optimization may enable more dead store elimination
 ******************************************************************************/
void optimize_program(Program *program) {
  Program *optimized = malloc(sizeof(Program));
  Program *before_pass = malloc(sizeof(Program));

  for (int iteration = 0; iteration < 10; iteration++) {
    memcpy(before_pass, program, sizeof(Program));

    /* Instruction folding - merge consecutive RIGHT and INC */
    merge_consecutive_right_inc(optimized, program, OP_RIGHT);
    *program = *optimized;

    merge_consecutive_right_inc(optimized, program, OP_INC);
    *program = *optimized;

    /* Zeroing loop detection: [-] → SET 0 */
    create_zeroing_sets(optimized, program);
    *program = *optimized;

    /* Memset optimization: SET > SET > SET → SET(count) */
    optimize_memset(optimized, program);
    *program = *optimized;

    /* Seek empty optimization: [>] → SEEK_EMPTY */
    optimize_seek_empty(optimized, program);
    *program = *optimized;

    /* Transfer loop optimization: [->+<] → TRANSFER + SET 0 */
    optimize_multi_transfer(optimized, program);
    *program = *optimized;

    /* SET + INC merge: SET v, INC n → SET (v+n) */
    optimize_set_inc_merge(optimized, program);
    *program = *optimized;

    /* Offset threading: eliminate RIGHT by using offsets */
    optimize_offsets(optimized, program);
    *program = *optimized;

    /* Divmod pattern detection */
    optimize_divmod(optimized, program);
    *program = *optimized;

    /* Dead store elimination - MUST run before SET+TRANSFER merge */
    eliminate_dead_stores(optimized, program);
    *program = *optimized;

    /* SET + TRANSFER merge (requires dead stores eliminated first) */
    optimize_set_transfer_merge(optimized, program);
    *program = *optimized;

    /* INC + TRANSFER merge */
    optimize_inc_transfer_merge(optimized, program);
    *program = *optimized;

    /* Transfer chain optimization */
    optimize_transfer_chain(optimized, program);
    *program = *optimized;

    /* Temp cell elimination */
    optimize_eliminate_temp_cells(optimized, program);
    *program = *optimized;

    /* INC cancellation */
    optimize_inc_cancellation(optimized, program);
    *program = *optimized;

    /* Check for fixed point */
    const int changed = memcmp(before_pass, program, sizeof(Program)) != 0;
    if (!changed) {
      break;
    }
  }

  free(optimized);
  free(before_pass);
}
