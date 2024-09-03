escapedChars :: Parser Char
escapedChars = do char '\\'
                  a <- oneOf "\\\"nrt"
                  return $ case a of
                             '\\' -> a
                             '"'  -> a
                             'n'  -> '\n'
                             'r'  -> '\r'
                             't'  -> '\t'
                             -- don't know if i should do this too, or not, but
                             'b'  -> '\b'
