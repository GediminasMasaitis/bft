/**
 * test_harness.c - Comprehensive Test Suite for BFT Brainfuck Transpiler
 *
 * This test harness:
 * - Discovers all .b files in the programs directory
 * - Tests each program on all backends (interpreter, C codegen, NASM codegen)
 * - Measures generation time, compilation time, and execution time
 * - Verifies correctness by comparing output to the interpreter's output
 * - Reports line count and character count of generated files
 * - Prints a comprehensive summary with statistics
 */

#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "bft.h"

/*******************************************************************************
 * Configuration
 ******************************************************************************/

#define MAX_PROGRAMS 64
#define MAX_OUTPUT_SIZE (1024 * 1024) /* 1MB max output */
#define MAX_PATH_LEN 512
#define TIMEOUT_SECONDS 10

typedef enum {
  BACKEND_INTERPRETER,
  BACKEND_C,
  BACKEND_NASM,
  BACKEND_COUNT
} Backend;

static const char *backend_names[] = {"Interpreter", "C Codegen",
                                      "NASM Codegen"};

/*******************************************************************************
 * Timing utilities
 ******************************************************************************/

static double get_time_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
}

/*******************************************************************************
 * File utilities
 ******************************************************************************/

static int file_exists(const char *path) {
  struct stat st;
  return stat(path, &st) == 0;
}

static long file_size(const char *path) {
  struct stat st;
  if (stat(path, &st) != 0)
    return -1;
  return st.st_size;
}

static int count_lines(const char *path) {
  FILE *f = fopen(path, "r");
  if (!f)
    return -1;

  int lines = 0;
  int ch;
  int last_was_newline = 1;

  while ((ch = fgetc(f)) != EOF) {
    if (ch == '\n') {
      lines++;
      last_was_newline = 1;
    } else {
      last_was_newline = 0;
    }
  }

  /* Count last line if it doesn't end with newline */
  if (!last_was_newline) {
    lines++;
  }

  fclose(f);
  return lines;
}

static char *read_file(const char *path, size_t *out_size) {
  FILE *f = fopen(path, "rb");
  if (!f)
    return NULL;

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *buf = malloc(size + 1);
  if (!buf) {
    fclose(f);
    return NULL;
  }

  size_t read = fread(buf, 1, size, f);
  buf[read] = '\0';
  fclose(f);

  if (out_size)
    *out_size = read;
  return buf;
}

static int write_file(const char *path, const char *content, size_t size) {
  FILE *f = fopen(path, "wb");
  if (!f)
    return -1;
  fwrite(content, 1, size, f);
  fclose(f);
  return 0;
}

/*******************************************************************************
 * Command execution with timeout and output capture
 ******************************************************************************/

typedef struct {
  int exit_code;
  double elapsed_ms;
  char *output;
  size_t output_size;
  int timed_out;
} ExecResult;

static ExecResult exec_command(const char *cmd, const char *input,
                               int timeout_sec) {
  ExecResult result = {0};
  result.output = malloc(MAX_OUTPUT_SIZE);
  if (!result.output) {
    result.exit_code = -1;
    return result;
  }
  result.output[0] = '\0';

  /* Create temp files for output */
  char out_path[] = "/tmp/bft_test_XXXXXX";
  int out_fd = mkstemp(out_path);
  if (out_fd < 0) {
    result.exit_code = -1;
    return result;
  }
  close(out_fd);

  /* Build command with output redirection */
  char full_cmd[2048];
  if (input && strlen(input) > 0) {
    char in_path[] = "/tmp/bft_input_XXXXXX";
    int in_fd = mkstemp(in_path);
    write(in_fd, input, strlen(input));
    close(in_fd);
    snprintf(full_cmd, sizeof(full_cmd), "timeout %d %s < %s > %s 2>&1",
             timeout_sec, cmd, in_path, out_path);
  } else {
    snprintf(full_cmd, sizeof(full_cmd), "timeout %d %s > %s 2>&1", timeout_sec,
             cmd, out_path);
  }

  double start = get_time_ms();
  int status = system(full_cmd);
  result.elapsed_ms = get_time_ms() - start;

  if (WIFEXITED(status)) {
    result.exit_code = WEXITSTATUS(status);
    if (result.exit_code == 124) {
      result.timed_out = 1;
    }
  } else {
    result.exit_code = -1;
  }

  /* Read output */
  FILE *f = fopen(out_path, "rb");
  if (f) {
    result.output_size = fread(result.output, 1, MAX_OUTPUT_SIZE - 1, f);
    result.output[result.output_size] = '\0';
    fclose(f);
  }

  unlink(out_path);
  return result;
}

static void free_exec_result(ExecResult *r) {
  if (r->output) {
    free(r->output);
    r->output = NULL;
  }
}

/*******************************************************************************
 * Test result tracking
 ******************************************************************************/

typedef struct {
  char name[64];
  int passed;
  int outputs_match;
  double gen_time_ms;
  double compile_time_ms;
  double run_time_ms;
  int gen_lines;
  long gen_chars;
  int timed_out;
  char error_msg[256];
} BackendResult;

typedef struct {
  char filename[MAX_PATH_LEN];
  char basename[64];
  BackendResult backends[BACKEND_COUNT];
  char *reference_output;
  size_t reference_output_size;
} TestResult;

/*******************************************************************************
 * Run interpreter to get reference output
 ******************************************************************************/

static int run_interpreter(const char *bf_path, char **output,
                           size_t *output_size, double *elapsed_ms) {
  char cmd[1024];
  snprintf(cmd, sizeof(cmd), "./bft -r '%s'", bf_path);

  ExecResult result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  *elapsed_ms = result.elapsed_ms;

  if (result.timed_out) {
    free_exec_result(&result);
    return -2; /* timeout */
  }

  if (result.exit_code != 0) {
    free_exec_result(&result);
    return -1;
  }

  *output = result.output;
  *output_size = result.output_size;
  return 0;
}

/*******************************************************************************
 * Test C backend
 ******************************************************************************/

static void test_c_backend(const char *bf_path, const char *basename,
                           BackendResult *result, const char *expected_output,
                           size_t expected_size) {
  strcpy(result->name, "C Codegen");
  result->passed = 0;

  char c_path[MAX_PATH_LEN], exe_path[MAX_PATH_LEN];
  snprintf(c_path, sizeof(c_path), "/tmp/bft_test_%s.c", basename);
  snprintf(exe_path, sizeof(exe_path), "/tmp/bft_test_%s_c", basename);

  /* Generate C code */
  char cmd[1024];
  snprintf(cmd, sizeof(cmd), "./bft -c '%s' -o '%s'", bf_path, c_path);

  double start = get_time_ms();
  ExecResult gen_result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  result->gen_time_ms = get_time_ms() - start;

  if (gen_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Code generation failed: %s", gen_result.output);
    free_exec_result(&gen_result);
    return;
  }
  free_exec_result(&gen_result);

  /* Get file stats */
  result->gen_lines = count_lines(c_path);
  result->gen_chars = file_size(c_path);

  /* Compile C code */
  snprintf(cmd, sizeof(cmd), "gcc -O2 -o '%s' '%s'", exe_path, c_path);
  start = get_time_ms();
  ExecResult compile_result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  result->compile_time_ms = get_time_ms() - start;

  if (compile_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Compilation failed: %s", compile_result.output);
    free_exec_result(&compile_result);
    unlink(c_path);
    return;
  }
  free_exec_result(&compile_result);

  /* Run executable */
  ExecResult run_result = exec_command(exe_path, NULL, TIMEOUT_SECONDS);
  result->run_time_ms = run_result.elapsed_ms;

  if (run_result.timed_out) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Execution timed out");
    result->timed_out = 1;
    free_exec_result(&run_result);
    unlink(c_path);
    unlink(exe_path);
    return;
  }

  if (run_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Execution failed with code %d", run_result.exit_code);
    free_exec_result(&run_result);
    unlink(c_path);
    unlink(exe_path);
    return;
  }

  /* Compare output */
  result->outputs_match =
      (run_result.output_size == expected_size &&
       memcmp(run_result.output, expected_output, expected_size) == 0);

  if (!result->outputs_match) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Output mismatch: expected %zu bytes, got %zu bytes",
             expected_size, run_result.output_size);
  }

  result->passed = result->outputs_match;
  free_exec_result(&run_result);

  /* Cleanup */
  unlink(c_path);
  unlink(exe_path);
}

/*******************************************************************************
 * Test NASM backend
 ******************************************************************************/

static void test_nasm_backend(const char *bf_path, const char *basename,
                              BackendResult *result,
                              const char *expected_output,
                              size_t expected_size) {
  strcpy(result->name, "NASM Codegen");
  result->passed = 0;

  char asm_path[MAX_PATH_LEN], obj_path[MAX_PATH_LEN], exe_path[MAX_PATH_LEN];
  snprintf(asm_path, sizeof(asm_path), "/tmp/bft_test_%s.asm", basename);
  snprintf(obj_path, sizeof(obj_path), "/tmp/bft_test_%s.o", basename);
  snprintf(exe_path, sizeof(exe_path), "/tmp/bft_test_%s_asm", basename);

  /* Generate NASM code */
  char cmd[1024];
  snprintf(cmd, sizeof(cmd), "./bft -s '%s' -o '%s'", bf_path, asm_path);

  double start = get_time_ms();
  ExecResult gen_result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  result->gen_time_ms = get_time_ms() - start;

  if (gen_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Code generation failed: %s", gen_result.output);
    free_exec_result(&gen_result);
    return;
  }
  free_exec_result(&gen_result);

  /* Get file stats */
  result->gen_lines = count_lines(asm_path);
  result->gen_chars = file_size(asm_path);

  /* Assemble with NASM */
  snprintf(cmd, sizeof(cmd), "nasm -f elf64 -o '%s' '%s'", obj_path, asm_path);
  start = get_time_ms();
  ExecResult asm_result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  double asm_time = get_time_ms() - start;

  if (asm_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Assembly failed: %s", asm_result.output);
    free_exec_result(&asm_result);
    unlink(asm_path);
    return;
  }
  free_exec_result(&asm_result);

  /* Link with ld */
  snprintf(cmd, sizeof(cmd), "ld -o '%s' '%s'", exe_path, obj_path);
  ExecResult link_result = exec_command(cmd, NULL, TIMEOUT_SECONDS);
  result->compile_time_ms = asm_time + link_result.elapsed_ms;

  if (link_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg), "Linking failed: %s",
             link_result.output);
    free_exec_result(&link_result);
    unlink(asm_path);
    unlink(obj_path);
    return;
  }
  free_exec_result(&link_result);

  /* Run executable */
  ExecResult run_result = exec_command(exe_path, NULL, TIMEOUT_SECONDS);
  result->run_time_ms = run_result.elapsed_ms;

  if (run_result.timed_out) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Execution timed out");
    result->timed_out = 1;
    free_exec_result(&run_result);
    unlink(asm_path);
    unlink(obj_path);
    unlink(exe_path);
    return;
  }

  if (run_result.exit_code != 0) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Execution failed with code %d", run_result.exit_code);
    free_exec_result(&run_result);
    unlink(asm_path);
    unlink(obj_path);
    unlink(exe_path);
    return;
  }

  /* Compare output */
  result->outputs_match =
      (run_result.output_size == expected_size &&
       memcmp(run_result.output, expected_output, expected_size) == 0);

  if (!result->outputs_match) {
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Output mismatch: expected %zu bytes, got %zu bytes",
             expected_size, run_result.output_size);
  }

  result->passed = result->outputs_match;
  free_exec_result(&run_result);

  /* Cleanup */
  unlink(asm_path);
  unlink(obj_path);
  unlink(exe_path);
}

/*******************************************************************************
 * Discover test programs
 ******************************************************************************/

static int discover_programs(const char *dir, char programs[][MAX_PATH_LEN],
                             int max_programs) {
  DIR *d = opendir(dir);
  if (!d) {
    fprintf(stderr, "Error: Could not open directory %s\n", dir);
    return 0;
  }

  int count = 0;
  struct dirent *ent;
  while ((ent = readdir(d)) != NULL && count < max_programs) {
    const char *name = ent->d_name;
    size_t len = strlen(name);
    if (len > 2 && strcmp(name + len - 2, ".b") == 0) {
      snprintf(programs[count], MAX_PATH_LEN, "%s/%s", dir, name);
      count++;
    }
  }
  closedir(d);
  return count;
}

/*******************************************************************************
 * Extract basename without extension
 ******************************************************************************/

static void get_basename(const char *path, char *out, size_t out_size) {
  const char *last_slash = strrchr(path, '/');
  const char *name = last_slash ? last_slash + 1 : path;

  strncpy(out, name, out_size - 1);
  out[out_size - 1] = '\0';

  char *dot = strrchr(out, '.');
  if (dot)
    *dot = '\0';
}

/*******************************************************************************
 * Print utilities
 ******************************************************************************/

static void print_separator(int width) {
  for (int i = 0; i < width; i++)
    putchar('-');
  putchar('\n');
}

static void print_centered(const char *str, int width) {
  int len = strlen(str);
  int pad_left = (width - len) / 2;
  int pad_right = width - len - pad_left;
  printf("%*s%s%*s", pad_left, "", str, pad_right, "");
}

/*******************************************************************************
 * Main test runner
 ******************************************************************************/

int main(int argc, char **argv) {
  const char *programs_dir = "programs";

  if (argc > 1) {
    programs_dir = argv[1];
  }

  printf("\n");
  printf("╔════════════════════════════════════════════════════════════════════"
         "╗\n");
  printf("║           BFT - Brainfuck Transpiler Test Suite                    "
         "║\n");
  printf("╚════════════════════════════════════════════════════════════════════"
         "╝\n");
  printf("\n");

  /* Check for required tools */
  printf("Checking prerequisites...\n");

  if (!file_exists("./bft")) {
    fprintf(stderr, "Error: bft executable not found. Run 'make' first.\n");
    return 1;
  }
  printf("  ✓ bft executable found\n");

  ExecResult gcc_check = exec_command("gcc --version", NULL, 5);
  if (gcc_check.exit_code != 0) {
    fprintf(stderr, "Error: gcc not found\n");
    free_exec_result(&gcc_check);
    return 1;
  }
  free_exec_result(&gcc_check);
  printf("  ✓ gcc available\n");

  ExecResult nasm_check = exec_command("nasm --version", NULL, 5);
  int have_nasm = (nasm_check.exit_code == 0);
  free_exec_result(&nasm_check);
  if (have_nasm) {
    printf("  ✓ nasm available\n");
  } else {
    printf("  ✗ nasm not available (NASM tests will be skipped)\n");
  }

  printf("\n");

  /* Discover test programs */
  char programs[MAX_PROGRAMS][MAX_PATH_LEN];
  int program_count = discover_programs(programs_dir, programs, MAX_PROGRAMS);

  if (program_count == 0) {
    fprintf(stderr, "Error: No .b files found in %s\n", programs_dir);
    return 1;
  }

  printf("Found %d test program(s) in %s\n\n", program_count, programs_dir);

  /* Allocate results */
  TestResult *results = calloc(program_count, sizeof(TestResult));
  if (!results) {
    fprintf(stderr, "Error: Out of memory\n");
    return 1;
  }

  /* Run tests */
  int total_tests = 0;
  int passed_tests = 0;

  for (int i = 0; i < program_count; i++) {
    TestResult *tr = &results[i];
    strncpy(tr->filename, programs[i], MAX_PATH_LEN);
    get_basename(programs[i], tr->basename, sizeof(tr->basename));

    printf("Testing: %s\n", tr->basename);
    print_separator(60);

    /* Run interpreter to get reference output */
    printf("  [Interpreter] Running... ");
    fflush(stdout);

    double interp_time;
    int status = run_interpreter(programs[i], &tr->reference_output,
                                 &tr->reference_output_size, &interp_time);

    BackendResult *interp_result = &tr->backends[BACKEND_INTERPRETER];
    strcpy(interp_result->name, "Interpreter");
    interp_result->run_time_ms = interp_time;

    if (status == -2) {
      printf("TIMEOUT\n");
      interp_result->timed_out = 1;
      snprintf(interp_result->error_msg, sizeof(interp_result->error_msg),
               "Timed out");
    } else if (status != 0) {
      printf("FAILED\n");
      snprintf(interp_result->error_msg, sizeof(interp_result->error_msg),
               "Execution failed");
    } else {
      printf("OK (%.2f ms, %zu bytes output)\n", interp_time,
             tr->reference_output_size);
      interp_result->passed = 1;
      interp_result->outputs_match = 1;
      passed_tests++;
    }
    total_tests++;

    /* Skip other backends if interpreter failed */
    if (!interp_result->passed) {
      printf("  [C Codegen]   Skipped (no reference output)\n");
      printf("  [NASM Codegen] Skipped (no reference output)\n");
      printf("\n");
      continue;
    }

    /* Test C backend */
    printf("  [C Codegen]   ");
    fflush(stdout);

    test_c_backend(programs[i], tr->basename, &tr->backends[BACKEND_C],
                   tr->reference_output, tr->reference_output_size);

    BackendResult *c_result = &tr->backends[BACKEND_C];
    total_tests++;

    if (c_result->passed) {
      printf("PASS (gen: %.2f ms, compile: %.2f ms, run: %.2f ms)\n",
             c_result->gen_time_ms, c_result->compile_time_ms,
             c_result->run_time_ms);
      printf("              Generated: %d lines, %ld chars\n",
             c_result->gen_lines, c_result->gen_chars);
      passed_tests++;
    } else if (c_result->timed_out) {
      printf("TIMEOUT\n");
    } else {
      printf("FAIL: %s\n", c_result->error_msg);
    }

    /* Test NASM backend */
    printf("  [NASM Codegen] ");
    fflush(stdout);

    if (!have_nasm) {
      printf("SKIPPED (nasm not available)\n");
    } else {
      test_nasm_backend(programs[i], tr->basename, &tr->backends[BACKEND_NASM],
                        tr->reference_output, tr->reference_output_size);

      BackendResult *nasm_result = &tr->backends[BACKEND_NASM];
      total_tests++;

      if (nasm_result->passed) {
        printf("PASS (gen: %.2f ms, asm+link: %.2f ms, run: %.2f ms)\n",
               nasm_result->gen_time_ms, nasm_result->compile_time_ms,
               nasm_result->run_time_ms);
        printf("               Generated: %d lines, %ld chars\n",
               nasm_result->gen_lines, nasm_result->gen_chars);
        passed_tests++;
      } else if (nasm_result->timed_out) {
        printf("TIMEOUT\n");
      } else {
        printf("FAIL: %s\n", nasm_result->error_msg);
      }
    }

    printf("\n");
  }

  /* Print summary table */
  printf("\n");
  printf("╔════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╗\n");
  printf("║                                         SUMMARY TABLE              "
         "                                    ║\n");
  printf("╚════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╝\n");
  printf("\n");

  /* Header */
  printf("%-15s │ %-12s │ %-8s │ %-12s │ %-12s │ %-10s │ %-6s\n", "Program",
         "Backend", "Status", "Gen (ms)", "Build (ms)", "Run (ms)", "Output");
  printf("────────────────┼──────────────┼──────────┼──────────────┼───────────"
         "───┼────────────┼────────\n");

  for (int i = 0; i < program_count; i++) {
    TestResult *tr = &results[i];

    for (int b = 0; b < BACKEND_COUNT; b++) {
      if (b == BACKEND_NASM && !have_nasm)
        continue;

      BackendResult *br = &tr->backends[b];

      const char *status;
      if (br->timed_out) {
        status = "TIMEOUT";
      } else if (br->passed) {
        status = "PASS";
      } else if (br->error_msg[0] == '\0') {
        status = "SKIP";
      } else {
        status = "FAIL";
      }

      if (b == 0) {
        printf("%-15s │ ", tr->basename);
      } else {
        printf("%-15s │ ", "");
      }

      printf("%-12s │ %-8s │ ", backend_names[b], status);

      if (b == BACKEND_INTERPRETER) {
        printf("%-12s │ %-12s │ ", "-", "-");
      } else {
        if (br->gen_time_ms > 0) {
          printf("%10.2f   │ ", br->gen_time_ms);
        } else {
          printf("%-12s │ ", "-");
        }
        if (br->compile_time_ms > 0) {
          printf("%10.2f   │ ", br->compile_time_ms);
        } else {
          printf("%-12s │ ", "-");
        }
      }

      if (br->run_time_ms > 0) {
        printf("%8.2f   │ ", br->run_time_ms);
      } else {
        printf("%-10s │ ", "-");
      }

      if (br->outputs_match) {
        printf("MATCH");
      } else if (br->timed_out) {
        printf("-");
      } else if (br->error_msg[0] != '\0') {
        printf("ERROR");
      } else {
        printf("-");
      }
      printf("\n");
    }
    if (i < program_count - 1) {
      printf("────────────────┼──────────────┼──────────┼──────────────┼───────"
             "───────┼────────────┼────────\n");
    }
  }

  printf("\n");

  /* Generated code statistics */
  printf("╔════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╗\n");
  printf("║                                   GENERATED CODE STATISTICS        "
         "                                    ║\n");
  printf("╚════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╝\n");
  printf("\n");

  printf("%-15s │ %-20s │ %-20s\n", "Program", "C (lines / chars)",
         "NASM (lines / chars)");
  printf("────────────────┼──────────────────────┼─────────────────────\n");

  for (int i = 0; i < program_count; i++) {
    TestResult *tr = &results[i];
    BackendResult *c = &tr->backends[BACKEND_C];
    BackendResult *nasm = &tr->backends[BACKEND_NASM];

    printf("%-15s │ ", tr->basename);

    if (c->gen_lines > 0) {
      printf("%6d / %-11ld │ ", c->gen_lines, c->gen_chars);
    } else {
      printf("%-20s │ ", "-");
    }

    if (have_nasm && nasm->gen_lines > 0) {
      printf("%6d / %-11ld", nasm->gen_lines, nasm->gen_chars);
    } else {
      printf("%-20s", "-");
    }
    printf("\n");
  }

  printf("\n");

  /* Performance comparison */
  printf("╔════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╗\n");
  printf("║                                    PERFORMANCE COMPARISON          "
         "                                    ║\n");
  printf("╚════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╝\n");
  printf("\n");

  printf("%-15s │ %-15s │ %-15s │ %-15s │ %-12s │ %-12s\n", "Program",
         "Interp (ms)", "C (ms)", "NASM (ms)", "C Speedup", "NASM Speedup");
  printf("────────────────┼─────────────────┼─────────────────┼────────────────"
         "─┼──────────────┼─────────────\n");

  for (int i = 0; i < program_count; i++) {
    TestResult *tr = &results[i];
    BackendResult *interp = &tr->backends[BACKEND_INTERPRETER];
    BackendResult *c = &tr->backends[BACKEND_C];
    BackendResult *nasm = &tr->backends[BACKEND_NASM];

    printf("%-15s │ ", tr->basename);

    if (interp->run_time_ms > 0) {
      printf("%13.2f   │ ", interp->run_time_ms);
    } else {
      printf("%-15s │ ", "-");
    }

    if (c->run_time_ms > 0) {
      printf("%13.2f   │ ", c->run_time_ms);
    } else {
      printf("%-15s │ ", "-");
    }

    if (have_nasm && nasm->run_time_ms > 0) {
      printf("%13.2f   │ ", nasm->run_time_ms);
    } else {
      printf("%-15s │ ", "-");
    }

    /* Speedups */
    if (interp->run_time_ms > 0 && c->run_time_ms > 0) {
      double speedup = interp->run_time_ms / c->run_time_ms;
      printf("%10.1fx  │ ", speedup);
    } else {
      printf("%-12s │ ", "-");
    }

    if (have_nasm && interp->run_time_ms > 0 && nasm->run_time_ms > 0) {
      double speedup = interp->run_time_ms / nasm->run_time_ms;
      printf("%10.1fx ", speedup);
    } else {
      printf("%-12s", "-");
    }
    printf("\n");
  }

  printf("\n");

  /* Final summary */
  printf("╔════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╗\n");
  printf("║                                        FINAL RESULTS               "
         "                                    ║\n");
  printf("╚════════════════════════════════════════════════════════════════════"
         "════════════════════════════════════╝\n");
  printf("\n");
  printf("  Total tests run:  %d\n", total_tests);
  printf("  Tests passed:     %d\n", passed_tests);
  printf("  Tests failed:     %d\n", total_tests - passed_tests);
  printf("  Success rate:     %.1f%%\n",
         total_tests > 0 ? (100.0 * passed_tests / total_tests) : 0.0);
  printf("\n");

  if (passed_tests == total_tests) {
    printf("  ✓ All tests passed!\n");
  } else {
    printf("  ✗ Some tests failed\n");
  }
  printf("\n");

  /* Cleanup */
  for (int i = 0; i < program_count; i++) {
    if (results[i].reference_output) {
      free(results[i].reference_output);
    }
  }
  free(results);

  return (passed_tests == total_tests) ? 0 : 1;
}
