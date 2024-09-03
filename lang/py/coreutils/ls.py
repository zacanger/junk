#!/usr/bin/env python3

import os
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('dir', default=os.curdir, nargs="?")
parser.add_argument(
    '-a',
    help='include hidden files',
    action=argparse.BooleanOptionalAction)
args = parser.parse_args()

files = os.listdir(args.dir)

if args.a is None:
    files = [f for f in files if not f.startswith(".")]

print("\n".join(files))
