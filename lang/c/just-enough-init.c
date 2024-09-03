#define NULL 0

int main() {
  int status;
  const char *shell[] = { "/bin/sh", NULL };
  if (!fork()) {
    execve(shell[0], shell, NULL);
  }
  while (1) {
    wait(&status);
  }
}
