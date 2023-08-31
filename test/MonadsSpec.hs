module MonadsSpec
  ( rubric
  ) where

import Test.Hspec
import Test.Hrubric
import Test.HUnit

import Control.Applicative
import Control.Monad.Writer

import Monads (Expr (..))
import qualified Monads

rubric :: Rubric
rubric = do
  criterion "add" 0.05 . passOrFail $ do
    it "adds its operands" $ do
      Monads.add 4 5 @?= return 9
      Monads.add 6 1 @?= return 7

  criterion "mul" 0.05 . passOrFail $ do
    it "multiplies its operands" $ do
      Monads.mul 4 5 @?= return 20
      Monads.mul 6 1 @?= return 6

  criterion "bogus" 0.1 . passOrFail $ do
    it "does x * (x + 2)" $ do
      Monads.bogus 5 @?= return 35
      Monads.bogus 2 @?= return 8

  criterion "oneEighth" 0.1 . passOrFail $ do
    it "returns one eighth of input, if possible" $ do
      Monads.oneEighth 8 @?= return 1
      Monads.oneEighth 32 @?= return 4
      Monads.oneEighth 3 @?= empty
      Monads.oneEighth 36 @?= empty

  let expr0 = (Var 'x' :/: Minus (Num 4)) :+: (Var 'y' :*: Var 'z')
  let expr1 = (Num 8 :/: Minus (Num 4)) :*: (Num 5 :+: Num 4)
  let expr2 = ((Num 3 :+: Num 4) :+: Num 5) :+: Num 6
  let expr3 = Num 3 :*: (Num 4 :/: (Num 4 :+: Minus (Num 4)))
  let expr4 = Var 'x' :*: (Var 'y' :*: Num 4)

  criterion "eval" 0.4 . passOrFail $ do
    it "evaluates an expression, if possible" $ do
      Monads.eval (Var 'x') @?= empty
      Monads.eval (Num 4) @?= return 4
      Monads.eval (Minus (Num 4)) @?= return (-4)
      Monads.eval expr0 @?= empty
      Monads.eval expr1 @?= return (-18)
      Monads.eval expr2 @?= return 18
      Monads.eval expr3 @?= empty 
      Monads.eval expr4 @?= empty

  criterion "vars" 0.3 . passOrFail $ do
    it "reports all variables in an expression" $ do
      let vars' = execWriter . Monads.vars
      vars' (Var 'x') @?= ['x']
      vars' expr0 @?= ['x', 'y', 'z']
      vars' expr1 @?= ([] :: [Char])
      vars' expr2 @?= ([] :: [Char])
      vars' expr3 @?= ([] :: [Char])
      vars' expr4 @?= ['x', 'y']
