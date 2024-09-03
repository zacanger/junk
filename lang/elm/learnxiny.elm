-- comment
{- mutliline comment
so, same as in haskell
-}

-- math is as expected
2 + 2
8 / 4
16 ^ 32

not True -- false
not False -- true

1 == 1  -- true
1 /= 1  -- false
10 < 20 -- true

"this is a string"
'c' -- char
"sup " ++ "brah" -- again, same as in hs

-- lists must be of same type
[1, 2, 3, 4]
[1..4]
['t', 'h', 'i', 's']
["is", "a", "sentence"]
[1..10] ++ [11..20] == [1..20]
0 :: [1..9] -- [0..9]
List.head [0..10] -- Just 0
List.tail [0..10] -- Just [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
List.head [] -- Nothing
-- List.method, basically

-- tuples can be mixed, but are fixed
fst ("foo", 4)
snd ("bar", 8)
() -- type "Unit"; placeholder

-- records are tuples with named fields
{ a = 4, b = 8 }.a -- 4
.d { c = 16, d = 32 } -- 32

-- updating records
{foo |
 bar = "quux" }

{ foo |
  baz = foo.baz + foo.bar,
  bar = foo.quux + foo.asdfghjkl }

-- if statements MUST have an 'else'
-- 'branches' must be of same type
if foo = 'c' then
  "hi!"
else
  "bye!"

if i == 0 then
  "i is zero"
else if i > 0 then
  "i is positive"
else
  "i is something else, i guess"

-- case
case someThing of
  [] -> "empty list"
  [x] -> "this has one item, which is " ++ toString x
  x::xs -> "this list has at least one item. its head is " ++ toString x
-- no fallthrough, so if the x::xs case was before [x], we'd never see [x]

-- functions!
-- no returns
adder a b =
  a + b
-- is the newline necessary, for short functions?
adder 4 4 -- 8

addFour =
  adder 4

-- constants
fourFours =
  16

List.map addFour [1..4]

-- anon func
List.map (\a -> a + 4) [1..4]

-- taking a tuple intead of args
foo (bar, quux) =
  bar ++ quux

-- similarly, working against a record
foo {bar, quux, baz} =
  let
    asdf = bar ++ quux
  in
    asdf ++ baz

foo { bar = "asdf", quux = "ghjkl;", baz = "qwerty" }

-- recursion
fib  =
  if n < 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

List.map fib [0..100]

-- parens for precedence. calls before infix operators.

-- TYPES. annotated with a single colon.
4 : Int
10.1 : Float
"hi" : String
False : Bool
not : Bool -> Bool
quadrupler : Int -> Int
quadrupler a = a * 4

List.map : (a -> b) -> List a -> List b

-- on records
whatever : { a : Int, b : Int, c : Int, d : Int }
whatever =
  {a = 4, b = 8, c = 16, d = 32}

-- type aliases (?)
type alias MyInts =
  {a : Int, b : Int, c : Int, d :Int}
ints =
  {a = 4, b = 8, c = 16, d = 32}

ints == whatever -- true

-- union types are previously nonexistent types
-- this is too confusing for myself at this point in time. i'll come back to this later.

-- modules
-- without a declaration, you're in Main
module Foo where
-- with explicit exports
module Bar (SomeNewType, someVal) where

-- imports
import Graphics as G
import Dict exposing (Dict)

-- ports
-- only in Main
-- this is the actual interfacing with outside-of-elm
port something : Int
port listString : List String
port listString = ["This", "is", "a", "list", "of", "strings"]

{- CLI stuff
elm make SomeThing.elm
elm reactor # server
elm repl
elm package install githubusername/reponame
-}

