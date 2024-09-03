-- compile : ghc filename.hs ; should build executable at filename

import Data.List
import System.IO

{-
running through learnxiny
and the sample from haskell book
and a couple of other things
this is a multiline comment
-}

-- this is a single-line comment
3 -- is three, a number
1 + 9 - (8 * (6 / 345)) -- 9.860869..etc....
9 `div` 4 -- 2

True
False -- primitives

-- commenting out the below because hlint wants me to just say True
-- not False -- True
1 == 1 -- True
10 > 1000 -- False

-- not is a function that takes one value.
-- arguments are just lined up after the function call (no parens)

"Here's a string."
'z' -- is a character
-- 'This is not valid.'
"This string " ++ "is concatenated with this one."
['T', 'h', 'i', 's', ' ', 'i', 's', ' '] -- a string (just arr of chars)
"String." !! 4 -- 'n'

-- lists need to be all the same type. destructuring works.
[1..10] == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
['a'..'c'] == ['a', 'b', 'c']
-- can skip
[2,4..8] == [2,4,6,8]
-- can't go backwards by default, so [10..0] breaks but the following is okay:
[10,8..0]
-- this could be infinite, so ['A'..], or [0..], for example. yeek.
-- can pass to get index of, like [0..] !! 100 is 100
[0..10] ++ [11..100]
1:[2..10] -- adds to beginning of list
head [0..4] -- 0
-- also last, for last of list
-- tail for all but first
-- init is... all except last?

-- list comprehensions: creating list based on extant list
[x+1 | x <- [2..8]] -- [3,4,5,6,7,8,9]
-- this has a conditional
[x+1 | x <- [2..8], x+2 > 4] -- [4,5,6,7,8,9]

-- tuples can be of mixed type. tuples have fixed length.
("things", 4) -- is a tuple
-- i think the below only work on sets of two.
fst ("things", 4) -- "things"
snd ("things", 4) -- 4

-- FUNCTIONS. THIS IS THE IMPORTANT BIT, OBVIOUSLY.
-- fns always take 1 arg, produce 1 result.
-- when multiple args passed, actually applying to nested fns (currying).
-- format is:
-- fnName param1 param2ifmoreparamsetc = expression
-- (in ghci, needs keyword `let` beforehand)
-- fnName must not start with capital letter. = is for the actual declaration.
add x y = x + y
-- this works in source. with the repl, need the let keyword. for example:
-- let mult q r = q * r
-- fn call can be before or between (only two?) args
add 4 4 -- 8
16 `add` 16 -- 32
-- function definitions have few (or no?) restrictions, i think.
(**) t u = t * (t * u)
(**) 4 4 -- i think without parens in source, with in the repl
-- guards are like conditionals (if/if...else) ??
-- from the thing:
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)
-- you can use a sort of pattern matching to choose which fn
-- definition to use?! i think this is the same thing as above.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)
-- that thing again, works on tuples and lists
bar (a, b) = (a * b, b + a)
-- writing map; x is first, xs is rest
aMap func [] = []
aMap func (x:xs) = func x:(aMap func xs)
-- anonymous functions: backslash, then args
aMap (\x -> x + x) [0..10]
-- fold (inject?): foldl1 means fold left, use first val in
-- list as initial for accumulator (wat?)
foldl1 (\acc x -> acc + x) [0..10]

-- currying(?) (or partial application, anyway... are they different?)
mult x y = x * y
quux = mult 4
quux 4 -- 16
-- different syntax for the same:
quux (4*)
quux 4

-- composition uses . to chain them
-- baz multiplies argument by 4, then adds 4 to tha
baz = (4+) . (4*)
baz 4 -- 20

-- there's a $ operator. it applies fn to param. it has priority of 0
-- and is 'right-associative' (wat?).
-- regular calls are 10 priority (highest) and are 'left-associative' (again, wat?)
-- so... expression on right is param to fn on left?
even (fib 7) -- false
even $ fib 7 -- same thing
even . fib $ 7 -- also false
-- so
fn $ a = fn a
(2*) $ 2 - 2 == (2*) (2 - 2)
-- so, it kinda says 'evaluate the stuff on my right FIRST'
-- it's a **weak infixr**

-- TYPES! i mean, type signatures. or something. this matters a lot, i believe.
-- because it's haskell. which is strongly typed. which is important.
False :: Bool
4 :: Integer
"Yo" :: String
-- not :: Bool -> Bool -- takes Bool, returns Bool
-- mult :: Integer -> Integer -> Integer
triple :: Integer -> Integer
triple q = q * 3

-- if
something = if 1 == 1 then "asdf" else "ghjkl;" -- something = "asdf"
another = if 2 < 0
             then "wat"
             else "wut"
-- another = "wut"
-- note that the indentation is significant

-- case
case args of
  "halp" -> showHalp
  "start" -> doStuff
  _ -> putStrLn "do better naxt tym"

-- loops? what loops? we don't need loops. we recurse! ...and stuff.
map (+10) [1..10] -- MAP
for array func = map func array -- whaaaat is this
for [0..10] $ \i -> show i -- i don't know what's happening
for [0..10] show -- this is the same thing?
--foldl (or foldr) fn initial-value list
foldr (\x y -> 4*x + y) 4 [0..4] -- 44
foldl (\x y -> 4*x + y) 4 [0..4] -- 4208

-- data types
-- new:
data Foo = Bar | Quux | Baz
say :: Foo -> String
say Bar = "sup"
say Quux = "brah"
say Baz = "asdfghjkl;"
-- can take params
data Something x = Wat | Wut x
Wut "oi" -- type : Something String
Wut 8    -- type : Something Int
Wat      -- type : Something x (for any 'x')

-- io
-- when hs is run, main is called. it's of type IO x for some x.
main :: IO ()
main = putStrLn $ "sup " ++ (say Quux)
-- putStrLn :: String -> IO ()
-- interact takes fn of String -> String
-- interact :: (String -> String) -> IO ()
countStuff :: String -> String
countStuff = show . length . lines
main' = interact countStuff
-- so type IO () means it's a series of things for computer to do.
-- that sounds awfully imperative and procedural. do makes it even moreso.
sayHi :: IO ()
sayHi = do
  putStrLn "who are you?"
  name <- getLine -- gets line as name
  putStrLn $ "hey, " ++ name
-- getLine :: IO String
-- means that String is generated when getLine is executed
something :: IO String
something = do
  putStrLn "this is a string"
  input1 <- getLine
  input2 <- getLine
  return (input1 ++ "\n" ++ input2)
  -- return is not a keyword here! it's a function.
  -- return :: String -> IO String
  -- so, its type is implicit? i guess actual annotations are
  -- superfluous a lot of the time? i think?
main'' = do
  putStrLn "this is one line"
  return <- something
  putStrLn result
  putStrLn "this is the next line"
-- so, IO is a monad. uhm. this is where i get lost. what.
-- fns that interact with externals are IO in the type signature
-- (because they do IO).

-- ghci
-- needs the keyword let before things
-- :q, :t (type) (let foo = 4 ; :t foo : foo :: Num a => a)
-- :t (:) is (:) :: a -> [a] -> [a]
-- :l is :load
-- :m is :module (like unload, i guess, to return to Prelude>)
-- so you can inspect punctuation functions with parens, eg
-- (+), ($).
-- :i is info(?)
-- so :i (*) for example
-- all IO types can be run directly, i think.

-- vars
-- TYPE variables usually start at a (a, b, c, etc.)
-- sometimes you'll append integers (a1, a2, etc.)
-- fns are usually labeled starting with _f_, then _g_, etc. (wat).
-- but sometimes with ints appended.
-- also with ' (a single apostrophe. this is what i personally
-- have seen the most, thus far).
-- this would be pronounced "f prine" (for `f'`), apparently (wat).
-- args usually seem to start at `x`.
-- a list of things that are named x might be called `xs` (that is, 'exes',
-- as in, the plural of `x`).
-- see e.g. `(x:xs)` (x, at the head of a list, then the rest of the xs).

-- this is me writing a function all on my own whaaaaattt?!
piMult a b = pi * (a * b)

-- some fns can be either prefix OR infix, like mod
-- for example:
divBy :: Int -> Int -> Bool
divBy d x = x `mod` d == 0
-- could also be
divBy d x = mod x d == 0

-- in ghci :i will show the association of infix operators
-- for example, infixl means left-associative
-- so, since * is infixl
2 * 4 * 8 --is evaluated as
(2 * 4) * 8

-- in source, order of declarations is irrelevant
-- in the repl, obviously, it does kinda matter though

-- module names are capitalized
-- start a module (file) with something like
module Asdfghjkl where
  -- this is how we'll import this module elsewhere
  m = "foo"
  n = " bar"

  o p q = p ++ q

  o m n

-- whitespace significant
-- LINE THINGS UP YAY I LIKE THIS
-- both of the following would be valid:
let r = 4
    s = 8

let
  t = 16
  u = 32

-- all declarations must start at the same column

-- values : final, irreducable expressions

-- where, let
module SomeMod where

printInc n = print plusTwo
  where plusTwo = n + 2

module SomeOther where

printInc2 n = let plusTwo = n + 2
               in print plusTwo

-- let, followed by in, makes a let expression

-- freaking math, man
let a = b in c -- is the same as
(\a -> c) b
c where a = b

-- VOCAB
-- argument is obvs (also called paramater sometimes i guess?)
-- expression is combination of symbols that can be evaluated
-- redex is reducible expression (most of them)
-- usually the term value for an irreducable expression
-- function is also obvious, but the book says:
-- a list of ordered pairs of inputs and outputs
-- infix is used in maths and logic. means the operator is between the operands
-- operators are functions that, by default, are infix. these use symbols.

-- char: single quotes. str: double quotes.
-- str will show as type [Char] in the repl
-- print "foo" => "foo"
-- putStr "foo" => foo
-- putStrLn "foo" => foo\n
-- print :: Show a => a -> IO ()
-- putStr (and putStrLn) :: String -> IO ()

-- strings and things
module PrintStuff where

greetz :: String
greetz = "yo, sup"

yo  :: String
yo  = "yo, "

sup :: String
sup = "sup"

main :: IO ()
main = do
  putStrLn greetz
  putStrLn greetzTwo
--    where greetzTwo = concat [yo, sup]
    where greetzTwo = yo ++ sup

-- type signatures
-- (++) :: [a] -> [a] -> [a]
-- this means that ++ takes an argument of [a] (a list of elements of unknown (yet) type)
-- then acts with another [a] (must be same type; a === a)
-- returns type [a]
-- [a] is polymorphic. so it could be [Char] (that is, Str) or [Int] (or whatever??)
-- concatenation needs lists of the same type

-- cons:
-- :
-- works like
'z' : "ac"
'z' : ""
head "zac" -- 'z'
tail "zac" -- 'ac'
take 2 "zac" -- "za"
take 0 "zac" -- ""
drop 9 "zac" -- ""
drop 1 "zac" -- "ac"
"zac" !! 0 -- 'z'
"zac" !! 2 -- 'c'

-- Integer will expand as much as needed (like a bignum or whatever
-- that is), within (memory) limits.

-- Int is limited to usually either 32 or 64 (in every implementation,
-- at least 30) bits.

-- Just x -- this means that x is a thing!
-- bool is implemented basically as: Bool = True | false
-- maybe a = Nothing | Just a

-- Int - -2^63 through 2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer - unbounded
-- Use Double over Float for precision
-- Chars are unicode

sumOfNums l = sum l
-- avgOfNums l = div (sum l) (length l) -- maybe?

-- covert Int to Float
nine = 9 :: Int
sqrtNine = sqrt (fromIntegral nine)
-- sqrt has t Floating a => a -> a

-- truncate 1.11111 is the same as 1.11111 | 0 in js

-- && and || work fine
-- not works fine

-- null [1] -- check empty list
-- reverse is list reverse
-- head, init, tail, last

-- list contains
4 `elem` [1..5] -- True

-- generate a list of ten twos
take 10 $ repeat 2
replicate 10 2
take 10 $ cycle [2]

listTimesTwo = [x * 2 | x <- [1..10]]

-- filters!
byNineAndThirteen = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sumOfLists = zipWith (+) [1..5] [6..10]

evenUpTo20 = takeWhile (<= 20) [2,4..]

multOfList = foldl (*) 1 [2..8]
multOfListR = foldR -- etc

powerOfThree = [3^n | n <- [1..10]]
-- so i think this works kinda like
-- power of three of n where n is -- etc.
-- in other word, for n in-range 1..10 do n^3

-- lists are so badass
multTable = [[a * b | a <- [1..10]] | b <- [1..10]]

-- tuples don't have to be same type. need to remember this.

-- fst, snd are tuple operators
-- zip takes lists to tuples

-- fun fact, i still don't know what a factorial is
factorial a = a * factorial $ a - 1

getListItems :: [Int] -> String
getListItems []       = "empty list"
getListItems (x:[])   = "list starts with " ++ show x
getListItems (x:y:[]) = "list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs)   = "list starts with " ++ show x " and has " show xs

getFirstItem :: String -> String
getFirstItem [] = "empty string"
getFirstItem all@(x:xs) = "first letter in " ++ all " is " ++ show x

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]
fib200 = fib !! 200
