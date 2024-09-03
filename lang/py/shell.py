#!/usr/bin/env python

import os
import subprocess


def main():
    while True:
        inp = input("$ ")
        if inp == "exit":
            break
        elif inp[:3] == "cd ":
            cd(inp[3:])
        elif inp == "help":
            print("no help here")
        else:
            execute_commands(inp)


def execute_commands(command):
    try:
        if "|" in command:
            s_in, s_out = (0, 0)
            s_in = os.dup(0)
            s_out = os.dup(1)

            fdin = os.dup(s_in)

            for cmd in command.split("|"):
                os.dup2(dfin, 0)
                os.close(fdin)

                if cmd == command.split("|")[-1]:
                    fdout = os.dup(s_out)
                else:
                    fdin, fdout = os.pipe()

                os.dup2(fdout, 1)
                os.close(fdout)

                try:
                    subprocess.run(cmd.strip().split())
                except Exception:
                    print("oh no {}".format(cmd.strip()))

            os.dup2(s_in, 0)
            os.dup2(s_out, 1)
            os.close(s_in)
            os.close(s_out)

        else:
            subprocess.run(command.split())
    except Exception:
        print("oh no! {}".format(command))


def cd(path):
    try:
        os.chdir(os.path.abspath(path))
    except Exception:
        print("no dir {}".format(path))


if __name__ == "__main__":
    main()
