#include <stdio.h>

// This method is defined in test.s -- see RUN-TEST! in compile.scm
int scheme_entry();

#define FIXNUM_MASK   0x03
#define FIXNUM_TAG    0
#define FIXNUM_SHIFT  2

int main(int argc, char** argv) {
  int val = scheme_entry();

  if ((val & FIXNUM_TAG) == FIXNUM_TAG) {
    printf("%d", val >> FIXNUM_SHIFT);
  }

  return 0;
}
