module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array
import Data.Foldable

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "hi" -- avg 10 20

-- 1. Write a function which averages two Numbers
avg :: Number -> Number -> Number
avg a b = (a + b) / 2.0

-- 2. Write a function which averages a List of numbers
--    (This will be very challenging (probably impossible)
--     if it's your first time, but that's ok.
--     Explore Data.List a little on pursuit.purescript.org,
--     take a guess, then we'll do it together next week)

-- avgLst :: List Number -> Number
-- avgLst l = (sum l) / (length l)

avgList :: Array Int -> Int
avgList l = div (sum l) (length l)

avL :: List Number -> Number
avL xs = sum xs / toNumber (length xs)
