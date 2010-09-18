#include <stdio.h>

#define __ISTYPE(val, type) (type##_TAG == (val & type##_MASK))
#define __VAL(val, type) (val >> type##_SHIFT)

#define FIXNUM_SHIFT 0x02
#define FIXNUM_MASK  0x03
#define FIXNUM_TAG   0x00

#define CHAR_SHIFT   0x08
#define CHAR_MASK    0xff
#define CHAR_TAG     0x0f

#define BOOL_SHIFT   0x07
#define BOOL_MASK    0x7f
#define BOOL_TAG     0x1f
#define FALSE_VAL    0x1f
#define TRUE_VAL     0x9f

#define NULL_VAL     0x2f

void write(int val) {
  if (__ISTYPE(val, FIXNUM))
    printf("%d", __VAL(val, FIXNUM));
  else if (__ISTYPE(val, CHAR))
    printf("#\\%c", __VAL(val, CHAR));
  else
    switch(val) {
    case FALSE_VAL:
      printf("#f");
      break;

    case TRUE_VAL:
      printf("#t");
      break;

    case NULL_VAL:
      printf("()");
      break;

    default:
      printf("ERROR: unrecognized immediate representation: %d", val);
    }
}

int main() {
  write(scheme_entry());
  return 0;
}
