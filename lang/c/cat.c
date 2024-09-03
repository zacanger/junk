#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char **argv) {
  int fd = 0;
  size_t len = 0;
  char buf[LINE_MAX];

  while (*(++argv) != NULL) {

    if ((*argv)[0] == '-' && (*argv)[1] == 0) {
      fd = 0;
    } else {
      fd = open(*argv, O_RDONLY);
    }

    if (fd < 0) {
      perror(*argv);
    } else {
      while ((len = read(fd, buf, LINE_MAX)) > 0) {
        write(1, buf, len);
      }
      close(fd);
    }
  }
  return 0;
}
