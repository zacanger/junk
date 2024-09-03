#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const* argv[]) {
  int i, j=10;
  for(i = 0; i < j; i++) {
    printf("\r%3d%%", (int)(100L * i / j));
    fflush(stdout);
    system("sleep 1");
  }
  printf("\ndone.\n");
  return 0;
}
