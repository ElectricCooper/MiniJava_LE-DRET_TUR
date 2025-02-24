/*
class My_Test {
  public static void main(String[] a) {
    if (2 > 1) System.out.println(1);
    else System.out.println(2);
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
  if ((2 > 1)) printf("%d\n", 1);
  else printf("%d\n", 2);
  tgc_stop(&gc);

  return 0;
}
