module Monads
  ( add
  , mul
  , bogus
  , oneEighth
  , Expr (..)
  , eval
  , vars
  ) where

import Data.Functor.Identity
import Control.Monad
import Control.Applicative
import Control.Monad.Writer

----------- 
-- Note: --
-----------
-- While monads are very useful in functional programming languages,
-- they are also notoriously difficult to grasp.
--
-- Make sure to read at least the monad tutorial on the course
-- page before attempting to fill in the stubs here! There's tons
-- of resources on monads, which may also aid in your understanding
-- of them.
--
-- And as always, don't hesitate to reach out to us if things remain unclear.

-- | Arithmetic addition (but with Identity monad).
--
-- Identity is kind of like Maybe without the Nothing case:
-- data Identity a = Identity a
add :: Int -> Int -> Identity Int
add = undefined

-- | Arithmetic multiplication (but with Identity)
mul :: Int -> Int -> Identity Int
mul = undefined

-- | Bogus function that implements:
-- bogus x = x * (x + 2)
--
-- But using add and mul as defined above.
-- Hint: try using (>>=) or do notation.
bogus :: Int -> Identity Int
bogus = undefined

----------- 
-- Note: --
-----------
-- So you might be wondering why you did this wonderfully useless exercise!
-- 
-- It's because you basically did everything that a monad can do!
-- - You've used 'Identity' or maybe even 'return' to wrap values into a Monad.
-- - You've used bind (>>=) either explicitely or implicitely via do-notation.
--
-- But if this is all there is to monads, why would we ever want them? They're 
-- useless!
--
-- Identity is definitely the trivial monad that does nothing! But that's why
-- it is so good to study. You don't get distracted by other things. 
--
-- All the monads we will see from this point actually do serve a purpose.
-- The interesting bit is though that they're all very comparable to Identity.

-- | Returns the integer divided by two on even numbers.
-- On odd numbers, returns Nothing.
--
-- e.g. half 4  = Just 2
--      half 15 = Nothing
half :: Int -> Maybe Int
half x 
  | r == 0 = Just q
  | otherwise = Nothing
  where (q, r) = divMod x 2

-- | Returns one eighth of the number passed. Use 'half' to implement this.
--
-- e.g. oneEighth 32 = Just 4
-- e.g. oneEighth 30 = Nothing
--
-- While you can solve this by pattern matching. This quickly becomes
-- unreadable! Instead try to use (>>=) or do-notation.
--
-- If you were able to do it using one of those, you could even try it
-- using (>=>).
oneEighth :: Int -> Maybe Int
oneEighth = undefined

----------- 
-- Note: --
-----------
-- Notice how Maybe is kind of like Identity, with an added Nothing case. The
-- bind (>>=) operation also does something interesting. It stops computation,
-- when Nothing occured during any operation in the sequence!

-- | A simple arithmetic expression.
--
-- The next stubs will use this structure.
data Expr a
  = Var a
  -- ^ Variable
  | Num Int
  -- ^ Number
  | Minus (Expr a)
  -- ^ Unary negation
  | Expr a :+: Expr a
  -- ^ Addition
  | Expr a :*: Expr a
  -- ^ Multiplication
  | Expr a :/: Expr a
  -- ^ Division

-- | Evaluates the expression. This returns Nothing if the expression contains
-- a variable, or if the denominator of a division is 0.
--
-- e.g. eval $ Minus (Num 4) :+: (Num 2 :*: Num 3) = Just 2
-- e.g. eval $ Var "x" :+: Num 5 = Nothing
-- e.g. eval $ Num 10 :/: Num 0 = Nothing
--
-- Hints:
-- - Maybe is a functor (i.e. you can call fmap on it). This is usefull in
--   the Minus case. Also look at 'negate'.
-- - You can use 'empty' as a "more general" 'Nothing'.
-- - You can use 'guard' in the (:/:) case. 
-- - This function will actually be a nightmare if you do not use Maybe 
--   as a monad.
-- 
-- As always, look new function up on hoogle!
eval :: Expr a -> Maybe Int
eval = undefined

----------- 
-- Note: --
-----------
-- Next up, we will use the Writer monad to accumulate some output.
--
-- You can use 'tell' to report data to the monad.
--
-- One thing to try and demistify Writer: As long as you don't call 'tell',
-- it functions exactly like Identity!
--
-- With this in mind, 'tell' really doesn't change anything how we use the
-- monad. It is just a way for us to report data.
--
-- For later reference, this is the same for State and Reader, and their
-- accompanying functions.

-- | Reports all variables in the expression, using the writer monad.
--
-- Use 'tell' to report any variable you encounter.
--
-- The other constructors should just recurse all their operands.
vars :: Expr a -> Writer [a] ()
vars = undefined
