#pragma once

#include "types.h"

void optimize_counts(Program *optimized, const Program *original);
void optimize_set(Program *optimized, const Program *original);
void optimize_memset(Program *optimized, const Program *original);
void optimize_seek_empty(Program *optimized, const Program *original);
void optimize_transfer(Program *optimized, const Program *original);
void optimize_multi_transfer(Program *optimized, const Program *original);
void optimize_dead_stores(Program *optimized, const Program *original);
void optimize_offsets(Program *optimized, const Program *original);
void optimize_set_inc_merge(Program *optimized, const Program *original);
void optimize_transfer_offsets(Program *optimized, const Program *original);
void optimize_program(Program *program);
