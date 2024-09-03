parseNumber = many1 digit >>= return . Number . read
