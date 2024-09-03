/*  replaces multiple spaces with just one */

#include <stdio.h>

int main(void) {
  int c;

  while ((c = getchar()) != EOF) {
    if (c == ' ') {
      putchar(c);
      while((c = getchar()) == ' ') {  }
    }
    if (c != EOF) {
      putchar(c);
    }
  }
  return 0;
}
