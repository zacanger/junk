parseNumber = many1 digit >>= \a -> (return . Number . read) a
