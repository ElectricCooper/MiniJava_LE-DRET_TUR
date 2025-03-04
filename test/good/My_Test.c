/*
class My_Test {
  public static void main(String[] a) {
    System.out.println(true);
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
  if(1) printf("true\n"); else printf("false\n"); 
  tgc_stop(&gc);

  return 0;
}
