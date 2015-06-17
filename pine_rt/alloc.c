#include "alloc.h"
#include <gc.h>

void *alloc(size_t size) {
  return GC_MALLOC(size);
}

void gc_init(void) {
  GC_INIT();
}
