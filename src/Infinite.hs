module Infinite
  ( collatz
  , fizzBuzz
  , fib
  ) where

-- | Define the infinite list of the collatz conjecture
--
-- Rules: given argument n
--   n is even -> n/2
--   n is odd  -> n * 3 + 1
--
-- e.g. collatz 5 = [5, 16, 8, 4, 2, 1, 4, 2, 1 ..]
--
-- Hint: Integer division is performed by 'div', not /
collatz :: Integral a => a -> [a]
collatz = undefined

-- | Define the infinite list of FizzBuzz
--
-- Rules:
--   "FizzBuzz" if multiple of 3 and 5
--   "Fizz" if multiple of 3
--   "Buzz" if multiple of 5
--   else just the number as string
--
-- e.g. fizzBuzz = [1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13 14, FizzBuzz]
--
-- Hint: use `show` to make an int into a string
--       use `mod` for modulo operation
fizzBuzz :: [String]
fizzBuzz = undefined 

-- | Define the infinite list of Fibonacci in O(n)
--
-- Rules:
--  The first two numbers in the sequence are 0 and 1 respectively
--  All other numbers are the sum of their previous two numbers
--
-- e.g. fib = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ..]
--
-- Hint: You can define this using `scanl`
fib :: Num a => [a]
fib = undefined
