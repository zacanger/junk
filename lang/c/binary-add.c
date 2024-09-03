#include <stdio.h>

int and (int a, int b) {
  return a && b;
}

int xor (int a, int b) {
  return a ^ b;
}

int or (int a, int b) {
  return a || b;
}

int main (int ct, char **args) {
  int b1, b2;
  int a, b, i;
  int cI, cO, sum[10];

  cI = 0;

  sscanf (args[1], "%d", &b1);
  sscanf (args[2], "%d", &b2);

  for (i = 0; i <= 3; i++) {
    a = b1 % 10;
    b = b2 % 10;
    b1 = b1 / 10;
    b2 = b2 / 10;

    sum[i] = xor(xor(a, b), cI);
    cO = or(and(a, b), and(xor(a, b), cI));
    cI = cO;
  }

  sum[i] = cO;

  for (i = 4; i >= 0; i--) {
    printf("%d", sum[i]);
  }

  printf("\n");
}
