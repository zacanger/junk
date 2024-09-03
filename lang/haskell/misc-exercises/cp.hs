-- https://twitter.com/romanzolotarev/status/866180248328654849
-- copying (lol) here because i like it

import System.Environment

main = do
  [ a, b ] <- getArgs
  string   <- readFile a
  writeFile b string
