fact = product . enumFromTo (1 :: Integer)
comb n k = fact n `div` (fact k * fact (n - k))
