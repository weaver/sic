#include <stdio.h>

// This method is defined in test.s -- see RUN-TEST! in compile.scm
int scheme_entry();

int main(int argc, char** argv) {
  printf("%d", scheme_entry());
  return 0;
}
