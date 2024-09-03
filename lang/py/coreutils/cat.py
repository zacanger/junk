#!/usr/bin/env python3

import sys

args = sys.argv[1:]

if len(args) < 1:
    sys.exit(0)

for a in args:
    with open(a, "r") as f:
        print(f.read())
