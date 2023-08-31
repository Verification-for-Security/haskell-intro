module NoRecSpec
  ( rubric
  ) where

import Test.Hspec
import Test.Hrubric
import Test.HUnit

import qualified NoRec

rubric :: Rubric
rubric = distribute $ do
  dcriterion "product" . passOrFail $ do
    it "produces 1 on empty list" $ do
      NoRec.product [] @?= product ([] :: [Int])
    it "calculates product of a list of numbers" $ do
      NoRec.product [4, 2, 3] @?= product ([4, 2, 3] :: [Int])
      NoRec.product [62, 3, 3, 6] @?= product ([62, 3, 3, 6] :: [Int])
      NoRec.product [1..10] @?= product ([1..10] :: [Int])

  dcriterion "factorial" . passOrFail $ do
    it "computes the base case" $ do
      NoRec.factorial 0 @?= (1 :: Int)
    it "computer larger factorials" $ do
      NoRec.factorial 3 @?= (6 :: Int)
      NoRec.factorial 4 @?= (24 :: Int)

  dcriterion "length" . passOrFail $ do
    it "produces 0 on empty list" $ do
      NoRec.length [] @?= length ([] :: [Int])
    it "computes the length of non-empty lists correctly" $ do
      NoRec.length [1..10 :: Int] @?= 10
      NoRec.length [1..100 :: Int] @?= 100

  dcriterion "replicate" . passOrFail $ do
    it "creates correct empty list" $ do
      NoRec.replicate 0 'a' @?= []

    it "creates correct finite list" $ do
      NoRec.replicate 3 'a' @?= replicate 3 'a'
      NoRec.replicate 5 'a' @?= replicate 5 'a'

  dcriterion "concat" . passOrFail $ do
    it "does nothing for the empty list" $ do
      NoRec.concat [] @?= ([] :: [[Int]])
    it "correctly concats lists" $ do
      let lt = [[1, 2, 3], [4, 5], [], [6, 7]] :: [[Int]]
      NoRec.concat lt @?= concat lt
