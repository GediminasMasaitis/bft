#pragma once

#include "types.h"
#include <stdio.h>

void codegen_nasm(const Program *program, FILE *output);
void codegen_c(const Program *program, FILE *output);