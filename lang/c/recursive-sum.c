#include <stdlib.h>

int sum (int n, int *list) {
  int x;
  int *xs;

  if (*list == 0) {
    return n;
  } else {
    x = list[0];
    xs = list + 1;

    if (0 == (x % 2)) {
      return sum(n + x, xs);
    } else {
      return sum(n, xs);
    }
  }
}
