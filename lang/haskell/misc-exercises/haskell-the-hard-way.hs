-- yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way

-- very imperative
evenSum :: [Integer] -> Integer
evenSum l = accumSum 0 1
accumSum n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                        in if even x
                              then accumSum (n + x) xs
                              else accumSum n xs

-- better
evenSum :: Integral a => [a] -> a
evenSum l = accumSum 0 l
  where accumSum n l =
    if l == []
       then n
       else let x = head l
                xs = tail l
             in if even x
                   then accumSum (n + x) xs
                   else accumSum n xs

-- betterer
evenSum l = accumSum 0 l
  where
    accumSum n [] = n
    accumSum n (x:xs) =
      if even x
         then accumSum (n + x) xs
         else accumSum n xs

-- eta-reduced
evenSum = accumsum 0
  where
    accumSum n [] = n
    accumSum n (x:xs) =
      if even x
         then accumSum (n + x) xs
         else accumSum n xs

-- using basic HOFs
evenSum l = newSum 0 (filter even l)
  where newSum n [] = n
        newSum n (x:xs) = newSum (n + x) xs

import Data.List (foldl')

evenSum l = foldl' aSum 0 (filter even l)
  where aSum acc val = acc + val

evenSum l = foldl' (\a b -> a + b) 0 (filter even l)

evenSum l = foldl (+) 0 (filter even l)

evenSum = (foldl' (+) 0) . (filter even)

-- with clearer names for things:
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0
evenSum = sum' . (filter even)

--
squareEvenSum = sum' . (filter even) . (map (^2))
--
squareEvenSum' = evenSum . (map (^2))

--
-- binary tree
--
import Data.List
data BinTree a
  = Empty
  | Node a (BinTree a) (BinTree a)
      deriving (Show)

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (< x) xs))
                             (treeFromList (filter (> x) xs))

--
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Eq, Ord)
-- instance Show (BinTree a) where
instance (Show a) => Show (BinTree a)
  where
    show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
      where
        treeshow pref Empty = ""
        treeshow pref (Node x Empty Empty) =
          (pshow pref x)
          treeshow pref (Node x left Empty) =
            (pshow pref x) ++ "\n" ++
              (showSon pref "`__" "   " left)
          treeshow pref (Node x Empty right) =
            (pshow pref x) ++ "\n" ++
              (showSon pref "`__" "   " right)
          treeshow pref (Node x left right) =
            (pshow pref x) ++ "\n" ++
              (showSon pref "|--" "|  " left) ++ "\n" ++
                (showSon pref "`__" "   " right)
          showSon pref before next t =
            pref ++ before ++ treeshow (pref ++ next) t
          pshow pref x = replace '\n' ("\n" ++ pref) (show x)
          replace c new string =
            concatMap (change c new) string
              where
                change c new x
                  | x == c = new
                  | otherwise = x:[]

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (< x) xs))
                             (treeFromList (filter (> x) xs))

main = do
  putStrLn "Int binary tree:"
  print $ treeFromList [1,2,3,4,8,65,23,9,456,23,12] -- whatever, strings, other trees, whatever

-- same as take
take' n [] = []
take' 0 l = []
take' n (x:xs) = x:take' (n-1) xs

treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f Empty = Empty
treeMap f (Node x left right) = Node (f x)
                                     (treeMap f left)
                                     (treeMap f right)

infTree = Node 0 (dec infTree) (inc infTree)
  where
    dec (Node x l r) = Node (x - 1) (dec l) (dec r)
    inc (Node x l r) = Node (x + 1) (inc l) (inc r)

-- example
takeDepth _ Empty = Empty
takeDepth 0 _ = Empty
takeDepth n (Node x left right) = let
  nl = takeDepth (n - 1) left
  nr = takeDepth (n - 1) right
  in Node x nl nr

infiniteTree :: BinTree Int
infiniteTree = Node 0 (treeMap (\x -> x - 1) infiniteTree)
                      (treeMap (\x -> x + 1) infiniteTree)

tryInfTree = print $ takeDepth 4 infiniteTree

-- ask a user to enter a list of numbers
-- print the sum of the numbers
toList :: String -> [Integer]
toList inp = read ("[" ++ inp ++ "]")

main = do
  putStrLn "enter a list of comma-separated numbers"
  ns <- getLine
  print $ sum (toList ns)

--
import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString s = maybeRead $ "[" ++ s ++ "]"

main :: IO ()
main = do
  putStrLn "enter a list of comma-separated numbers"
  ns <- getLine
  let maybeList = getListFromString ns in
      case maybeList of
        Just l  -> print (sum l)
        Nothing -> error "you suck."

--
askUser :: IO [Integer]
askUser = do
  putStrLn "enter a list of comma-separated numbers"
  ns <- getLine
  let maybeList = getListFromString ns in
      case maybeList of
        Just l  -> return l
        Nothing -> askUser

main :: IO ()
main = do
  ls <- askUser
  print $ sum ls

--
main w0 =
  let (ls, w1) = askUser w0 in
      let (x, w2) = print (sum ls, w1) in
          x

askUser w0 =
  let (_, w1)  = putStrLn "enter a list of nums" in
  let (ns, w2) = getLine w1 in
  let (l, w3)  = case getListFromString ns of
    Just l  -> (l, w2)
    Nothing -> askUser w2
  in
    (l, w3)

--
--
--
import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString s = maybeRead $ "[" ++ s ++ "]"

askUser :: IO [Integer]
askUser =
  putStrLn "please enter a comma-separated list of numbers" >>
  getLine >>= \ns ->
  let maybeList = getListFromString ns in
      case maybeList of
        Just l  -> return l
        Nothing -> askUser

main :: IO ()
main = askUser >>=
  \ls -> print $ sum ls

--
-- what Monad kind of looks like:
--
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
  (>>)   :: m a -> m b -> m b
  f >> g = f >>= \_ -> g
  -- also for some reason:
  fail :: String -> m a
  fail = error

--
-- fake random thing
shuffle = map(\a -> (a & 3123) `mod` 4331) [1..]

shuffle = map rand [1..]
  where
    rand x = ((p x) `mod` (x + c)) - ((x + c) `div` 2)
    p x = m * x ^ 2 + n * x + o
    m = 3123
    n = 31
    o = 7641
    c = 1237

treeFromList :: (Ord a, Show a) => [a] -> BinTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x left right
  where
    left  = treeFromList $ safefilter (< x) xs
    right = treeFromList $ safefilter (> x) xs

safefilter :: (a -> Bool) -> [a] -> [a]
safefilter f l = safefilter' f l nbTry
  where
    nbTry = 10000
    safefilter' _ _ 0      = []
    safefilter' _ [] _     = []
    safefilter' f (x:xs) n =
      if f x
         then x : safefilter' f x nbTry
         else safefilter' f xs (n - 1)
