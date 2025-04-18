#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct DeclarationInitializationTest;
void* DeclarationInitializationTest_declare(struct DeclarationInitializationTest* this);
struct DeclarationInitializationTest {
  void* (**vtable)();
};
void* (*DeclarationInitializationTest_vtable[])() = { DeclarationInitializationTest_declare };
void* DeclarationInitializationTest_declare(struct DeclarationInitializationTest* this) {
  int i;
  i = 10;
  return (void*)(i);
}
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  {
    printf("%d\n", ({ struct DeclarationInitializationTest* tmp1 = ({ struct DeclarationInitializationTest* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = DeclarationInitializationTest_vtable; res; }); (int) tmp1->vtable[0](tmp1); }));
  }
  tgc_stop(&gc);

  return 0;
}
