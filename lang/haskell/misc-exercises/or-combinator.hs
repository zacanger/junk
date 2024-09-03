orCombinator :: (a -> Bool) -> (a -> Bool) -> Bool
orCombinator f g = \a -> (f a) || (g a)
