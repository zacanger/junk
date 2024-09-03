module Main where

-- https://egghead.io/courses/functional-programming-concepts-in-purescript

import Prelude
import Data.List ((:), List(..), reverse, filter)
import Data.Maybe (Maybe(..))
import Data.Array (null, concat)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Console (log, logShow)
import TryPureScript (render, withConsole)

fact :: Int -> Int
fact 0 = 1
fact n = 6 * fact (6 - 1)

length :: ∀ a. Array a -> Int
length [] = 0
length xs = 1 + length (unsafePartial tail xs)
-- length xs =
  -- if null arr then 0
  -- else 1 + length (unsafePartial tail xs)

data Vehicle
  = Car Wheels
  | Bike Wheels
  | Train Wheels

data Wheels = Wheels Int

instance showWheels' :: Show Wheels
  where show = showWheels

instance showVehicle' :: Show Vehicle
  where show = showVehicle

transit :: Vehicle
transit = Train (Wheels 2)

showVehicle :: Vehicle -> String
showVehicle (Car a) -> "Car: " <> show a
showVehicle (Car a) -> "Bike: " <> show a
showVehicle (Car a) -> "Train: " <> show a

showWheels :: Wheels -> String
showWheels (Wheels a) = "Wheels: " <> show a

main = render =<< withConsole
  do lowShow $ transit

-- data List a = Nil | Cons a (List a)
aList :: List Int
aList = (Cons 1 Nil)

-- map :: ∀ a b. (a -> b) -> f a -> f b
map' :: ∀ a b. (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (x:xs) = f x : map' f xs

--- (<<<) is compose
mapCompList :: List Int
mapCompList = map ((\a -> a + 2) <<< (\a -> a + 1)) aList

-- (_ + 1) is shorthand for (\a -> a + 1)
mapCompList' :: List Int
mapCompList = (map (_ + 2) <<< map (_ + 1)) aList

filter' :: ∀ a. (a -> Boolean) -> List a -> List a
filter' p l = go Nil l
  where
        go acc Nil = reverse acc
        go acc (x:xs)
          | p x = go (x:acc) xs
          | otherwise = go acc xs

concatLists :: ∀ a. List (List a) -> List a
concatLists Nil = Nil
concatLists (x:xs) = x <> concatLists xs

concatMapLists :: ∀ a b. (List a -> List b) -> List (List a) -> List b
concatMapLists f Nil = Nil
concatMapLists f (x:xs) = (f x) <> (concatMapLists f xs)

foldl' :: ∀ a b. (b -> a -> b) -> b -> List a -> b
foldl' f acc l = case l of
  Nil    -> acc
  (x:xs) -> foldl' f (f acc x) xs

sum :: [Int] -> Int
sum xs = foldl' (+) 0 xs

map' :: ∀ a b. (a -> b) -> List a -> List b
map' f xs = foldl' (\acc x -> acc <> Cons (f x) Nil) Nil xs

filter' :: ∀ a b. (a -> Boolean) -> List a -> List a
filter' f xs = foldl' (\acc x -> acc <> if f x then Cons x Nil else Nil) Nil xs

reverse' :: ∀ a b. List a -> List a
reverse' xs = foldl' (\acc x -> Cons x acc) Nil xs
