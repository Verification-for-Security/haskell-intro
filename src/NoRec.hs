module NoRec
  ( product
  , factorial
  , length
  , replicate
  , concat
  ) where

import Prelude hiding (product, length, replicate, concat, mconcat)

-----------
-- Note: --
-----------
-- In this file, you may not use explicit recursion
-- to define functions. This means that you may call
-- functions that use recursion internally but cannot
-- apply recursion yourself!
--
-- To illustrate, you will not get points by writing
-- sum [] = 0
-- sum (x:xs) = x + sum xs
--
-- And instead have to write something like
-- sum = foldl (+) 0
--
-- Using some of the list functions you've defined
-- earlier is highly encouraged (practically mandatory)
--
-- Good luck!

-- | The product of all values in the list
-- equals 1 if the list was empty
product :: Num a => [a] -> a
product = undefined

-- | Basic math factorial: 4! = 4 * 3! = 4 * 3 * 2! = ... = 24
-- Hint: You may define lists as follows [1..n]
--       which will create [1, 2, 3, .., n]
factorial :: (Num a, Enum a) => a -> a
factorial = undefined

-- | Length of the list
-- (Don't call length from List.hs...)
length :: [a] -> Int
length = undefined

-- | replicate an element n times
replicate :: Int -> a -> [a]
replicate = undefined

-- | Turn a list of list into a flattened list
-- e.g. concat [[a, b, c], [d, e], [], [f, g]] = [a, b, c, d, e, f, g]
concat :: [[a]] -> [a]
concat = undefined
