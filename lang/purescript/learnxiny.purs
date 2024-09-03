-- you can replace a thing with an underscore to get a more descriptive error out of pulp
-- invalid instance for type of class is a valid error
-- these errors aren't really super friendly....

-- comment
-- psci is repl
-- doesn't have a lot of the options that ghci has, but looks okay anyway
-- :q still quits it, so that's nice
-- can run the repl with --multi-line-mode
-- :t works. :i does not

import Prelude

1.2 :: Number
1 :: Int
-- math as expected
-- type inference works
-- can't mix Int and Number, so
-- 1+2 / 3.4 -- this would fail

--negation either with `-` or `negate` eg
10 + -10 -- and
10 + negate 10 -- are the same

true :: Boolean
false :: Boolean

-- operators
not false == true
10 >= 0
0 /= 10
-- < <= > >=
compare 10 0 -- GT
compare 0 0 -- EQ
compare 0 10 -- LT

-- strings can be multiline like so
"This is\
\multiline"
"""and so
is this"""
"sup " ++ "brah" -- same as hs

-- arrays are same as js, must be same type
[1, 2, 3, 4] :: Array Number
-- cons is familiar
0 :: [1, 2, 3, 4]
[0, 1, 2, 3, 4] !! 2 -- 1
0..5 == [0, 1, 2, 3, 4, 5]
length [0, 1, 2, 3, 4, 5] --  6
drop 2 [0, 1, 2, 3, 4, 5] -- [2, 3, 4, 5]
take 2 [0, 1, 2, 3, 4, 5] -- [0, 1, 2, 3]
append [0, 1, 2] [3, 4, 5] -- [0, 1, 2, 3, 4, 5]

-- records ~= js objs. can have mixed types. as with ghci, you need to `let`
-- for an obj in psci. these can be accessed with dot notation as in js, but also
-- with shorthand:
firstRecord = { firstProp : "something", secondProp: "another" }
_.firstProp firstRecord -- "something"
firstRecord.firstProp -- "something"
-- updating records i think is the same as in elm(?)

doubler :: Int -> Int
doubler x = x * 2
doubler 2 -- 4
-- and infix is a thing too, eg
15 `mod` 5

-- if then else
-- guards are a thing

-- here!
let fib 1 = 1
    fib 2 = 2
    fib x = fib (x-1) + fib (x-2)
fib 10 -- 89

-- underscore matches _any_
let isZac "zac" = true
    isZac _     = false

-- the backslash is i think same as in hs
(\a -> a + a) 2 -- 4

-- there's <<< and >>> which are composition in each direction i guess

-- hof
let evenFinder a = a `mod` 2 == 0
filter evenFinder (0..10)
map (\x -> x + x) (0..10)

-- any, all, sum, product, foldr (and i assume foldl) etc. are available,
-- but need a... third party module? i'm a little confused about how modules work here.

