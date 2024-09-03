/*  counts tabs, spaces, and newlines in stdin */

#include <stdio.h>

int main(void) {
  int spaces, tabs, newlines, c;

  spaces = tabs = newlines = 0;

  while ((c = getchar()) != EOF) {
    if (c == ' ') {
      ++spaces;
    }
    if (c == '\t') {
      ++tabs;
    }
    if (c == '\n') {
      ++newlines;
    }
  }
  printf("Spaces: %d\nTabs: %d\nNewlines: %d\n", spaces, tabs, newlines);
  return 0;
}
