#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct TestString;
struct TestFor;
struct TestWhile;
struct TestDoWhile;
void* TestString_run(struct TestString* this);
void* TestFor_run(struct TestFor* this);
void* TestWhile_run(struct TestWhile* this);
void* TestDoWhile_run(struct TestDoWhile* this);
struct TestString {
  void* (**vtable)();
};
struct TestFor {
  void* (**vtable)();
};
struct TestWhile {
  void* (**vtable)();
};
struct TestDoWhile {
  void* (**vtable)();
};
void* (*TestString_vtable[])() = { TestString_run };
void* (*TestFor_vtable[])() = { TestFor_run };
void* (*TestWhile_vtable[])() = { TestWhile_run };
void* (*TestDoWhile_vtable[])() = { TestDoWhile_run };
void* TestString_run(struct TestString* this) {
  char* s1;
  char* s2;
  char* s;
  s1 = "Hello ";
  s2 = "World";
  s = ({ char* tmp1 = s1; char* tmp2 = s2; char* result = malloc(strlen(tmp1) + strlen(tmp2) + 1); strcpy(result, tmp1); strcat(result, tmp2); result; });
  return (void*)(s);
}
void* TestFor_run(struct TestFor* this) {
  int i;
  for (i = 0; (i < 5); i = (i + 1)) {
    printf("%d\n", i);
  }
  return (void*)(0);
}
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
  {
    printf("%s\n", ({ struct TestString* tmp1 = ({ struct TestString* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = TestString_vtable; res; }); (char*) tmp1->vtable[0](tmp1); }));
  }
  tgc_stop(&gc);

  return 0;
}
