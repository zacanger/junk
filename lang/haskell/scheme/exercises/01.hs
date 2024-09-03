main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)
