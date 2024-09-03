#!/usr/bin/env python3

import sys

text = sys.argv[1:]

if len(text) == 0:
    text = "y"
else:
    text = " ".join(text)

try:
    while True:
        print(text)
except KeyboardInterrupt:
    sys.exit(0)
