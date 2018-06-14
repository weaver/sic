#include <stdio.h>

// This method is defined in test.s -- see RUN-TEST! in compile.scm
int scheme_entry();

#define FIXNUM_MASK   0x03
#define FIXNUM_TAG    0
#define FIXNUM_SHIFT  2
#define CHAR_MASK     0xFF
#define CHAR_TAG      0x0F
#define CHAR_SHIFT    8
#define BOOL_MASK     0xFF
#define BOOL_TAG      0x3F
#define BOOL_SHIFT    8
#define EMPTY_LIST    0x2F

int main(int argc, char** argv) {
  int val = scheme_entry();

  if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("%d", val >> FIXNUM_SHIFT);
  } else if ((val & CHAR_MASK) == CHAR_TAG) {
    printf("#\\%c", val >> CHAR_SHIFT);
  } else if ((val & BOOL_MASK) == BOOL_TAG) {
    if ((val >> BOOL_SHIFT) == 0) {
      printf("#f");
    } else {
      printf("#t");
    }
  } else if (val == EMPTY_LIST) {
    printf("'()");
  } else {
    printf("#unknown");
  }

  return 0;
}
