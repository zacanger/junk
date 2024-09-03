#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <string.h>

const char MESSAGE[] = "Press any key to continue ...";
const char ENDL[] = "\n";

int fd;

tcflag_t get_flag() {
  struct termios tty;
  tcgetattr(fd, &tty);
  return tty.c_lflag;
}

void set_flag(const tcflag_t flag) {
  struct termios tty;
  tcgetattr(fd, &tty);
  tty.c_lflag = flag;
  tcsetattr(fd, TCSANOW, &tty);
}

int main() {
  char tty[L_ctermid + 1] = { 0 };
  ctermid(tty);
  fd = open(tty, O_RDWR);
  write(fd, MESSAGE, strlen(MESSAGE));
  tcflag_t flag = get_flag();
  set_flag(flag & ~ICANON & ~ECHO);
  char ch;
  read(fd, &ch, 1);
  set_flag(flag);
  write(fd, ENDL, 1);
  return 0;
}
