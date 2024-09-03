main :: IO ()
main = do
  putStrLn "name plz"
  name <- getLine
  putStrLn $ "sup " ++ name
