#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct BreakTest;
void* BreakTest_runLoop(struct BreakTest* this);
struct BreakTest {
  void* (**vtable)();
};
void* (*BreakTest_vtable[])() = { BreakTest_runLoop };
void* BreakTest_runLoop(struct BreakTest* this) {
  int i;
  for (i = 0; (i < 10); ({ i = (i + 1); })) {
    if ((i > 4)) {
      break;
    }
    printf("%d\n", i);
  }
  return (void*)(0);
}
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  {
    printf("%d\n", ({ struct BreakTest* tmp1 = ({ struct BreakTest* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = BreakTest_vtable; res; }); (int) tmp1->vtable[0](tmp1); }));
  }
  tgc_stop(&gc);

  return 0;
}
