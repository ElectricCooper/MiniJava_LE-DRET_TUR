#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct Test;
void* Test_run(struct Test* this);
struct Test {
  void* (**vtable)();
};
void* (*Test_vtable[])() = { Test_run };
void* Test_run(struct Test* this) {
  int i;
  for (i = 0; (i < 5); i = (i + 1)) {
    printf("%d\n", i);
  }
  return (void*)(0);
}
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  printf("%d\n", ({ struct Test* tmp1 = ({ struct Test* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = Test_vtable; res; }); (int) tmp1->vtable[0](tmp1); }));
  tgc_stop(&gc);

  return 0;
}
