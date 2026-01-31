#pragma once
#include <stdint.h>

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

  // IR opcodes
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
      i32 arg1;
      i32 arg2;
      i32 offset;
      i32 arg4;
      TransferTarget targets[MAX_TRANSFER_TARGETS];
    };

    struct {
      i32 distance;
    } right;

    struct {
      i32 amount;
      i32 _inc_pad;
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
      i32 _seek_pad;
      i32 offset;
    } seek;

    struct {
      i32 target_count;
      i32 is_assignment;
      i32 src_offset;
      i32 _transfer_pad;
      TransferTarget targets[MAX_TRANSFER_TARGETS];
    } transfer;

    struct {
      i32 match_addr;
      i32 _loop_pad;
      i32 offset;
    } loop;

    struct {
      i32 divisor;
      i32 _div_pad;
      i32 src_offset;
      i32 _div_pad2;
      TransferTarget targets[MAX_TRANSFER_TARGETS];
    } div;

    struct {
      i32 divisor;
      i32 _mod_pad;
      i32 src_offset;
      i32 _mod_pad2;
      TransferTarget targets[MAX_TRANSFER_TARGETS];
    } mod;

    struct {
      i32 _in_pad1;
      i32 _in_pad2;
      i32 offset;
    } in;

    struct {
      i32 _out_pad1;
      i32 _out_pad2;
      i32 offset;
    } out;
  };
} Instruction;

typedef struct {
  Instruction instructions[MAX_CODE_SIZE];
  addr_t size;
} Program;
