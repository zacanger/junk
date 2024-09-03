#!/usr/bin/env python

from sys import argv


def find_factors(n):
    for i in range(1, n + 1):
        if n % i == 0:
            print(i)


find_factors(int(argv[1]))
