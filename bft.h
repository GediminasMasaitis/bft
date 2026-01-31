#pragma once

#include <stdint.h>
#include <stdio.h>

typedef char i8;
typedef unsigned char u8;
typedef int32_t i32;
typedef uint32_t u32;

typedef enum {
  OP_RIGHT = '>',
  OP_LEFT = '<',
  OP_INC = '+',
  OP_DEC = '-',
  OP_OUT = '.',
  OP_IN = ',',
  OP_LOOP = '[',
  OP_END = ']',
  OP_DONE = '\0',

  OP_MOVE = OP_RIGHT,
  OP_ADD = OP_INC,
  OP_SET = '=',
  OP_SEEK_EMPTY = 'E',
  OP_TRANSFER = 'T',
  OP_DIV = '/',
  OP_MOD = '%',
} OpCode;

enum {
  CELL_COUNT = 1024 * 64,
  MAX_CODE_SIZE = 1024 * 64,
  MAX_TRANSFER_TARGETS = 4
};

typedef i8 op_t;
typedef u8 cell_t;
typedef i32 status_t;
typedef i32 addr_t;

typedef enum {
  STATUS_OK = 0,
  STATUS_OOB_LOW = 1,
  STATUS_OOB_HIGH = 2,
  STATUS_UNMATCHED_LOOP = 3,
  STATUS_UNMATCHED_END = 4,
  STATUS_CODE_TOO_LARGE = 5
} StatusCode;

typedef struct {
  i32 offset;
  i32 factor;
  i32 bias;
} TransferTarget;

typedef struct {
  op_t op;
  union {
    struct {
      i32 distance;
    } right;

    struct {
      i32 count;
      i32 offset;
    } inc;

    struct {
      i32 value;
      i32 count;
      i32 offset;
      i32 stride;
    } set;

    struct {
      i32 step;
      i32 offset;
    } seek;

    struct {
      i32 target_count;
      i32 is_assignment;
      i32 src_offset;
      TransferTarget targets[MAX_TRANSFER_TARGETS];
    } transfer;

    struct {
      i32 match_addr;
      i32 offset;
    } loop;

    struct {
      i32 match_addr;
      i32 offset;
    } end;

    struct {
      i32 divisor;
      i32 src_offset;
      i32 dst_offset;
    } div;

    struct {
      i32 divisor;
      i32 src_offset;
      i32 dst_offset;
    } mod;

    struct {
      i32 offset;
    } in;

    struct {
      i32 offset;
    } out;
  };
} Instruction;

typedef struct {
  Instruction instructions[MAX_CODE_SIZE];
  addr_t size;
} Program;

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

void optimize_program(Program *program);

void codegen_c(const Program *program, FILE *output);

void codegen_nasm(const Program *program, FILE *output);
