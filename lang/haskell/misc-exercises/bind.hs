bind f x = concat (map f x)

bind f x s = let (x', s') = x s in f x' s'
