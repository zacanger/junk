#!/usr/bin/env python3


import os
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('file')
parser.add_argument(
    '-l',
    help='lines',
    action=argparse.BooleanOptionalAction)
parser.add_argument(
    '-c',
    help='chars',
    action=argparse.BooleanOptionalAction)

args = parser.parse_args()


with open(args.file, "r") as f:
    contents = f.read()
    count = []
    if args.l:
        count = contents.split("\n")
    elif args.c:
        count = list(contents)
    else:
        count = contents.split(" ")
    print(len(count))
