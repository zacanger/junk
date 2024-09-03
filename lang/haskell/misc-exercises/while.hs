while :: (Monad m) => m Bool -> m () -> m ()
while cond action = do
  c <- cond
  if c
     then action >> while cond action
     else return ()

-- another definition without do sugar
while = \cond body ->
  cond >>= (\testResult ->
    if testResult
       then (body >> while cond body)
       else (return ()))

while cond body = loop
  where loop = do
    res <- cond
    if res then do {body; loop}
           else return ()''
