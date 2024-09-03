/*  prints random temp filename */

#include <stdio.h>

int main(int argc, char **argv) {
  printf("%s\n", tmpnam(NULL));
  return 0;
}
