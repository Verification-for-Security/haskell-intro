module List
  ( head
  , length
  , replicate
  , take
  , foldl
  , foldr
  , filter
  , fmap
  , reverse
  , get
  , (<>)
  ) where

import Prelude hiding (head, length, replicate, take, foldl, foldr, filter, map, fmap, reverse, (<>), (++))

-- | Return the head of a list, if any
head :: [a] -> Maybe a
head = undefined

-- | Return the length of a list
-- e.g. length [a, b, c] = 3
length :: [a] -> Int
length = undefined

-- | Replicate the element n number of times in a list
-- e.g. replicate 4 'a' = ['a', 'a', 'a', 'a']
replicate :: Int -> a -> [a]
replicate = undefined

-- | Take n xs, should return the prefix of xs of length n.
-- If length xs <= n, return xs
-- e.g. take 2 [a, b, c, d] = [a, b]
--      take 2 [a]          = [a]
take :: Int -> [a] -> [a]
take = undefined

-- | Fold a list left-ways
-- e.g. foldl (+) 0 [a, b, c, d] = (((0 + a) + b) + c) + d
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl = undefined

-- | Fold a list right-ways
-- e.g. foldr (+) 0 [a, b, c, d] = a + (b + (c + (d + 0)))
--
-- Hint: It can be nice to write this using ($), which allows you to 
-- reduce parentheses. Try searching for it on hoogle!
-- e.g. f (g x) == f $ g x
-- Do try it out! It is a very common ideom to use ($) in Haskell.
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr = undefined

-- | Return a list containing only the elements that satisfy the predicate
-- e.g. filter odd [1, 2, 3, 4] = [1, 3]
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- | Apply the passed function to all elements of the list
-- e.g. fmap (+3) [0, 5, -3] = [3, 8, 0]
fmap :: (a -> b) -> [a] -> [b]
fmap = undefined

-- | Reverse the passed list
-- e.g. reverse [a, b, c] = [c, b, a]
--
-- If you'd like a small challenge, you can actually try to implement this
-- without binding the input argument. I.e. by starting the function with 
-- reverse = ...
-- Try using one of your previously implemented functions for this :)
reverse :: [a] -> [a]
reverse = undefined

-- | Get the element at the given index
-- e.g. get 1  [a, b, c, d] = Just b
--      get 10 [a, b, c, d] = Nothing
get :: Int -> [a] -> Maybe a
get = undefined

-- | Append two lists to each other
-- e.g. [3, 4] <> [1, 5] = [3, 4, 1, 5]
--
-- Hint: You may use both prefix and infix form 
--       pattern matching:
-- (<>) _ _ = ...
-- _ <> _ =  ...
(<>) :: [a] -> [a] -> [a]
(<>) = undefined
