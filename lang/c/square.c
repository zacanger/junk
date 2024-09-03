#include <stdlib.h>

int int_square (int x) {
  return x * x;
}

float float_square (float x) {
  return x * x;
}

complex complex_square (complex z) {
  complex tmp;
  tmp.real = z.real * z.real - z.img * z.img;
  tmp.img = 2 * z.img * z.real;
}
complex x, y;
y = complex_square(x);
