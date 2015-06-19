#include "io.h"
#include "alloc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_string(const char *string) {
  printf("%s", string);
  fflush(stdout);
}

void print_int(int value) {
  printf("%d", value);
  fflush(stdout);
}

void print_string_line(const char *string) {
  printf("%s\n", string);
  fflush(stdout);
}

void print_int_line(int value) {
  printf("%d\n", value);
  fflush(stdout);
}

const char *read_line() {
  char *line = NULL;
  size_t line_capacity = 0;
  ssize_t line_len;
  if ((line_len = getline(&line, &line_capacity, stdin)) == -1) {
    panic("read_line: failed to read from stdin");
  }
  char *gc_str = alloc(line_capacity);
  strncpy(gc_str, line, line_capacity);
  free(line);
  return gc_str;
}

void panic(const char *message) {
  printf("aborting: %s\n", message);
  abort();
}
