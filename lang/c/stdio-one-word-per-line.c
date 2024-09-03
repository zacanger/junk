/*  stdin to stdout, one word per line */

#include <stdio.h>

int main(void) {
  int c;
  int i = 0;

  while ((c = getchar()) != EOF) {
    if (c == ' ' || c == '\t' || c == '\n') {
      if (i == 0) {
        i = 1;
        putchar('\n');
      }
    } else {
      i = 0;
      putchar(c);
    }
  }
  return 0;
}
