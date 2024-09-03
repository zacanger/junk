-- i did not write these. found around.

[max(show x)(concat[n|(f,n)<-[(3,"Fizz"),(5,"Buzz")],mod x f==0])|x<-[1..100]]

-- gh:ryoia
-- i like this one. it's a lot like how i thought it was supposed to be
-- which means i can read it, except it's cleaner so i can see a simple way
-- to improve on my own super limited hs
module FizzBuzz where

divBy :: Int -> Int -> Bool
divBy d x = x `mod` d == 0

fizzBuzz :: Int -> String
fizzBuzz x
  | divBy 15 x = "FizzBuzz"
  | divBy 5  x = "Buzz"
  | divBy 3  x = "Fizz"
  | otherwise  = show x


-- the below are from haskellquiz

-- a FizzBuzz (and FizzBuzzBaz) solution by Aaron Contorer.
-- This implementation is designed for extensibility,
-- as the list of tags can be easily edited, loaded from a file, etc.
-- Number range is set >100 so as to demonstrate the FizzBuzzBaz case.
fizzBuzz i = if null desc then show i else desc where
  desc = concat [label | (j,label) <- tags, 0 == rem i j]
  tags = [ (3,"Fizz"), (5,"Buzz"), (7,"Baz") ]

main = mapM_ (putStrLn . fizzBuzz) [1..120]


-- don't know if this works, but looks not nice.
module Main where

  main :: IO ()
  main = do
    mapM_ (putStrLn) [fizzBuzz x | x < [0..100]]

    fizz :: Int -> String
    fizz x = if x `mod` 3 == 0 then "fizz" else ""

    buzz :: Int -> String
    buzz x = if x `mod` 5 == 0 then "buzz" else ""

    fizzBuzz :: Int -> String
    fizzBuzz x = if fizz(x) ++ buzz(x) == ""
                    then show x
                    else fizz(x) ++ buzz(x)


{-
Fizz comes before Buzz comes before an integer. Fizz and Buzz stick to each other, but hide integers.
The lists for Fizz and Buzz are infinite, but zipping together with a finite list of integers,
the result is finite.
-}

module Main where

main :: IO ()
main = mapM_ putStrLn $ zipWith3 join (loop 3 "Fizz") (loop 5 "Buzz") [1..100]
  where
    xor s t = if null s then t else s
    loop n s = cycle $ replicate (n-1) [] ++ [s]
    join s t n = xor (s ++ t) (show n)

{-
If one has enabled all warnings as errors, then the integers need an explicit type, as shown below.
The hiding logic can also be implemented by filtering for the first non-null element of a list:
-}

module Main where
  module Main where

  main :: IO ()
  main = sequence_ $ zipWith3 join (loop 3 "Fizz") (loop 5 "Buzz") [1..100 :: Int]
    where
      loop n s = cycle $ replicate (n-1) "" ++ [s]
      join s t n = putStrLn . head $ filter (not . null) [s ++ t, show n]


fizzbuzz :: Int -> String
fizzbuzz n = case (rem n 3 == 0, rem n 5 == 0) of
  (True, True) -> "FizzBuzz"
  (False, True) -> "Buzz"
  (True, False) -> "Fizz"
  (False, False) -> show n

main :: IO ()
main = do
  mapM_ (putStrLn.fizzbuzz) [1..100]


module FizzBuzz (
toFizz
) where

import Control.Monad.Instances
toFizz :: Int -> String
toFizz = do
  f <- (==0) . (flip mod 3)
  b <- (==0) . (flip mod 5)
  p <- show
  return $ case [f,b] of
             [True,True] -> "fizzbuzz"
             [True,False] -> "fizz"
             [False,True] -> "buzz"
             [False,False] -> p

m >~ str = ZipList . cycle $ replicate (m - 1) empty ++ [pure str]
fizzbuzzS = getZipList $ fromMaybe . show <$> ZipList [1..] <*> 3 >~ "fizz" <> 5 >~ "buzz"

let (m ~> str) x = str <$ guard (x `mod` m == 0)
    in map (fromMaybe . show <*> 3 ~> "fizz" <> 5 ~> "buzz")
