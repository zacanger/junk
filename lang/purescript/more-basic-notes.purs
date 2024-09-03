moduole Main where

import Control.Monad.Eff.Console

main = log "sup, brah"

-- this means Me is a data type from constructor of Person
data Me = Person { name :: String, age :: Number }

aboutMe :: Me -> String
aboutMe (Me o) = o.name ++ ", age " ++ show o.age

thisIsMe :: Me
thisIsMe = Me { name : "Zac", age : 26}

-- i THINK i'm getting this right.

{-
  oh right. multiline comments are the same.
-}

-- | this is considered documentation.
-- | pursuit (which i think is like a big old meta/doc thingy)
-- | will grab these automatically and turn into docs.
-- note: this line would not be included in the documentation.

plusThings x y = x + y
plusTwo = plusThings 2
plusTwo 2 -- 4

64 :: Int
64.0 :: Float -- or Number, apparently the same thing i guess i think

\a b -> a + b -- in javascript, this is: `a => b => a + b`

{ foo : _, bar : _ } -- is equal to:
\foo bar => { foo : foo, bar : bar }

-- updating records: someRecord { someKey = someValue, someOtherKey = someOtherValue }

-- negate is - in js
-- not is ! in js
-- complement is ~ in js

-- else is always required (if ... then ... else ...)

-- type classes and instances and superclasses are things
-- and i need to understand them better i think because i have no idea what's up there
-- that's partially a lie, actually

class Something z where
  show :: z -> String

instance Something :: Show String where
  show y = y

-- ...i think.

-- but superclasses are beyond me at the moment.

-- import Something (stuff, morestuff) is like
-- import { x, y } from 'z'
-- import Quux.Baz as Bar
-- you can import from js?
-- foreign import sqrt :: Number -> Number -> Number -- (i think)

foreign import data DOM :: *

foreign import document :: {
  createElement :: String -> DOM
}


-- okay so you name/specify exports like
module Foo (thisThingIsExported, thisThingIsAsWell) where

thisThingIsExported :: Int -> Int
thisThingIsAsWell :: String -> String
butThisIsNotExported :: Char -> String

-- and if you have a (..) in your exports, you're exporting
-- that thing AND its constructors
module Bar (Thing(..)) where
data Thing x =  Someconstructor x | Whatever

-- to export a class you need to export all its members as well

