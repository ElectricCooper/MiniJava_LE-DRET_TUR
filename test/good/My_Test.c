/*
class My_Test {
  public static void main(String[] a) {
    if (false || true) System.out.println(3);
    else System.out.println(4);
  }
}
*/
#include <stdio.h>
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  if ((0 || 1)) printf("%d\n", 3);
  else printf("%d\n", 4);
  tgc_stop(&gc);

  return 0;
}
