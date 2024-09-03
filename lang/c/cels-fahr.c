/*  shows fahrenheit and celsius table thingy */

#include <stdio.h>

int main(void) {
  float fahr, cels;
  int low, up, step;

  low = 0;
  up = 300;
  step = 20;

  cels = low;

  printf("  c    f\n");

  while (cels <= up) {
    fahr = cels * (9.0/5.0) + 32.0;
    printf("%3.0f %6.1f\n", cels, fahr);
    cels += step;
  }

  return 0;
}
