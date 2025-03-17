#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct TestWhile;
struct TestDoWhile;
void* TestWhile_run(struct TestWhile* this);
void* TestDoWhile_run(struct TestDoWhile* this);
struct TestWhile {
  void* (**vtable)();
};
struct TestDoWhile {
  void* (**vtable)();
};
void* (*TestWhile_vtable[])() = { TestWhile_run };
void* (*TestDoWhile_vtable[])() = { TestDoWhile_run };
void* TestWhile_run(struct TestWhile* this) {
  int j;
  int size;
  size = 5;
  j = 5;
  while ((j < size)) {
    printf("%d\n", j);
    j = (j + 1);
  }
  return (void*)(0);
}
void* TestDoWhile_run(struct TestDoWhile* this) {
  int j;
  int size;
  size = 5;
  j = 5;
  do {
    printf("%d\n", j);
    j = (j + 1);
  } while ((j < size));
  return (void*)(0);
}
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  printf("%d\n", ({ struct TestDoWhile* tmp1 = ({ struct TestDoWhile* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = TestDoWhile_vtable; res; }); (int) tmp1->vtable[0](tmp1); }));
  tgc_stop(&gc);

  return 0;
}
