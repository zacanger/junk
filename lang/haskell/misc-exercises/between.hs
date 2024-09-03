between :: Double -> Double -> Double -> Bool
between a b c =
  | a <= b = b <= c
  | otherwise = False
