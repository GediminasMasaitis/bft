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

  // Extended

  OP_SET = 'S',
  // arg1: value
  // arg2: count
  // offset: offset from data pointer 

  OP_SEEK_EMPTY = 'E',
  OP_TRANSFER = 'T'
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
} TransferTarget;

typedef struct {
  op_t op;
  i32 arg;
  i32 arg2;
  i32 offset;
  i32 stride;
  TransferTarget targets[MAX_TRANSFER_TARGETS];
} Instruction;

typedef struct {
  Instruction instructions[MAX_CODE_SIZE];
  addr_t size;
} Program;