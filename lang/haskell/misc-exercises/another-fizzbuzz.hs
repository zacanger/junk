data Fizzbuzz
  = Natural Int
  | Fizz
  | Buzz
  | FizzBuzz
  deriving (Show)

fizzbuzz :: Int -> Fizzbuzz
fizzbuzz n =
  case (mod n 3, mod n 5) of
    (0, 0) -> FizzBuzz
    (0, _) -> Fizz
    (_, 0) -> Buzz
    _      -> Natural n
