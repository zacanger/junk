type Stack a = [a]

swap :: Stack a -> Stack a
swap []       = []
swap (a:[])   = a:[]
swap (a:b:cs) = b:a:cs
