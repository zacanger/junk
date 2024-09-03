loeb :: Functor f => f (f b -> b) -> f b
loeb x = xs where xs = fmap ($ xs) x
