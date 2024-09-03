#!/usr/bin/env runhaskell
-- cat foo | ./linecount.hs
main = interact lineCount
  where lineCount input = show (length (lines input)) ++ "\n"
