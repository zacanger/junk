module CompareThings where

import Batteries

data Stuff
  = A
  | B
  | C

derive instance stuffEq :: Eq Stuff
derive instance stuffOrd :: Ord Stuff

{- instance stuffEq :: Eq Stuff where -}
  {- eq A A = true -}
  {- eq B B = true -}
  {- eq C C = true -}
  {- eq _ _ = false -}

compare :: Stuff -> Stuff -> Ordering
compare a b =
  case a, b of
       A, A -> EQ
       A, _ -> LT
       B, B -> EQ
       B, A -> GT
       B, C -> LT
       C, C -> EQ
       C, _ -> GT

compareTwo :: Stuff -> Stuff -> Ordering
compareTwo A A = EQ
compareTwo A _ = LT
compareTwo B B = EQ
compareTwo B A = GT
compareTwo B C = LT
compareTwo C C = EQ
compareTwo C _ = GT

{- lessThan :: Stuff -> Stuff -> Boolean -}
{- lessThan a b = a < b -}

{-
  compiled js will _always_ be fns that take 1 arg-so, always partial application
  js modules must always be in filename of same name as ps file
  should be exports.foo = function(){} (no es2015)
  =<< -- bind
  >>= -- also monadic bind
  parseInt = toMaybe <<< parseIntImpl -- this is the same as
  parseInt a = toMaybe $ parseIntImpl a
-}

newFn :: Int -> Int -> Int -> Int
newFn a b c = a + b + c

foreign import parseIntImpl :: String -> Nullable Int

parseInt :: String -> Maybe Int
parseInt = toMaybe <<< parseIntImpl

neuFn :: String -> String -> String -> String
neuFn a b c =
  let
    a' = parseInt a
    b' = parseInt b
    c' = parseInt c
  in
    case a' of
      Nothing -> "NaN"
      Just a'' ->
        case b' of
          Nothing -> "NaN"
          Just b'' ->
            case c' of
              Nothing -> "NaN"
              Just c'' -> show (newFn a'' b'' c'')

better :: String -> String -> String -> String
better a b c = maybe "NaN" show $ newFn <$> parseInt a <*> parseInt b <*> parseInt c

maybeBetter :: String -> String -> String -> String
maybeBetter a b c = maybe "NaN" show do
  a' <- parseInt a
  b' <- parseInt b
  c' <- parseInt c
  pure $ newFn a' b' c'
