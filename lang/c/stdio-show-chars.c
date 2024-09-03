/*  this copies input to output, showing
 *  \t, \b, and \\ */

#include <stdio.h>

#define ESC '\\'

int main(void) {
  int c;

  while((c = getchar()) != EOF)
    switch(c) {
    case '\t':
      putchar(ESC);
      putchar('t');
      break;
    case '\b':
      putchar(ESC);
      putchar('b');
      break;
    case ESC:
      putchar(ESC);
      putchar(ESC);
      break;
    default:
      putchar(c);
      break;
    }
  return 0;
}
