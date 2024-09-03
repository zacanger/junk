#include <stdio.h>

int main (void) {
  int x;
  int y;
  int z;

  while (1) {
    x = 0;
    y = 1;

    do {
      printf("%d\n", x);

      z = x + y;
      x = y;
      y = z;
    } while (x < 1000);
  }
}
