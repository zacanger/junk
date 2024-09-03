escapedChars :: Parser Char
escapedChars = do
  char '\\'
  a <- oneOf "\\\""
  return a

parseString :: Parser LispVal
parseString = do
  char '"'
  a <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String a
