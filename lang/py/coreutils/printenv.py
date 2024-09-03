#!/usr/bin/env python3

import os


def format_env(e):
    s = ""
    for k, v in dict(e).items():
        s += k + "=" + v + "\n"
    return s


print(format_env(os.environ))
