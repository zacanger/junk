parseNumber :: Parser LispVal
parseNumber = do
  a <- many1 digit
  (return . Number . read) a
