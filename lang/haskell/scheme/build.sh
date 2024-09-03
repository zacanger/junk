#!/bin/sh

ghc --make -o lang main.hs

# ghc -package parsec -fglasgow-exts -o lang --make main.hs
