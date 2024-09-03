#!/usr/bin/env runhaskell

module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "What's your name? "
  n <- getLine
  putStrLn $ "Heyo, " ++ n
