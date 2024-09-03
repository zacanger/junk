import Numeric

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseBool

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  a <- many1 digit
  (return . Number . read) a

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  a <- many1 hexDigit
  return $ Number (hex2dig a)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  a <- many1 octDigit
  return $ Number (oct2dig a)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  a <- many1 (oneOf "10")
  return $ Number (bin2dig a)

oct2dig a = fst $ readOct a !! 0
hex2dig a = fst $ readHex a !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                             bin2dig' old xs
