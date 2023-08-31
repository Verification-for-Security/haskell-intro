module ListSpec
  ( rubric
  ) where

import Test.Hspec
import Test.Hrubric
import Test.HUnit

import qualified List

rubric :: Rubric
rubric = distribute $ do
  dcriterion "head" . passOrFail $ do
    it "returns Nothing on empty list" $ do
      List.head ([] :: [Int]) @?= Nothing

    it "returns the head for non-empty lists" $ do
      List.head [5, 1, 2, 3] @?= Just (5 :: Int)
      List.head [2] @?= Just (2 :: Int)
      List.head [1..] @?= Just (1 :: Int)
  
  dcriterion "length" . passOrFail $ do
    it "computes correct empty list length" $ do
      List.length [] @?= 0

    it "computes correct length for finite lists" $ do
      List.length [1 :: Int, 2, 3, 4, 5] @?= length [1 :: Int, 2, 3, 4, 5]
      List.length [1 :: Int, 2, 3, 4] @?= length [1 :: Int, 2, 3, 4]

  dcriterion "replicate" . passOrFail $ do
    it "creates correct empty list" $ do
      List.replicate 0 'a' @?= []

    it "creates correct finite list" $ do
      List.replicate 3 'a' @?= replicate 3 'a'
      List.replicate 5 'a' @?= replicate 5 'a'

  dcriterion "take" . passOrFail $ do
    it "takes correct prefix" $ do
      List.take 2 [1, 2, 3] @?= take 2 [1 :: Int, 2, 3]
      List.take 4 [1, 2, 3, 4] @?= take 4 [1 :: Int, 2, 3, 4]

    it "returns the whole list if length was too short" $ do
      List.take 20 [1, 2] @?= take 20 [1 :: Int, 2]

  dcriterion "foldl" . passOrFail $ do
    it "folds the empty list correctly" $ do
      List.foldl (+) 5 [] @?= (5 :: Int)

    it "accumulates left-ways" $ do
      List.foldl div 18 [3, 2, 1] @?= foldl div 18 [3 :: Int, 2, 1]
      List.foldl (+) 4 [3, 2, 1] @?= foldl (+) 4 [3 :: Int, 2, 1]
      List.foldl (flip (:)) [] [3, 2, 1] @?= foldl (flip (:)) [] [3 :: Int, 2, 1]

  dcriterion "foldr" . passOrFail $ do
    it "folds the empty list correctly" $ do
      List.foldr (+) 5 [] @?= (5 :: Int)

    it "accumulates right-ways" $ do
      List.foldr (*) 18 [3, 2, 1] @?= foldr (*) 18 [3 :: Int, 2, 1]
      List.foldr (+) 4 [3, 2, 1]  @?= foldr (+) 4 [3 :: Int, 2, 1]
      List.foldr (:) [] [3, 2, 1] @?= foldr (:) [] [3 :: Int, 2, 1]

  dcriterion "filter" . passOrFail $ do
    it "filters the empty list" $ do
      List.filter (>3) [] @?= filter (>3) ([] :: [Int])

    it "filters non-empty lists" $ do
      List.filter (>3) [8, 3, 2, 5] @?= filter (>3) [8 :: Int, 3, 2, 5]
      List.filter (>3) [1, 3, 2, -2] @?= filter (>3) [1 :: Int, 3, 2, -2]
      List.filter id [True, False] @?= filter id [True, False]

  dcriterion "fmap" . passOrFail $ do
    it "maps the empty list" $ do
      List.fmap not [] @?= fmap not []

    it "correctly maps non-empty lists" $ do
      List.fmap not [True, False, False] @?= fmap not [True, False, False]
      List.fmap (+1) [4, 5, 1, 3] @?= fmap (+1) [4 :: Int, 5, 1, 3]

  dcriterion "reverse" . passOrFail $ do
    it "reverses the empty list" $ do
      List.reverse ([] :: [Int]) @?= reverse []

    it "reverses non-empty lists" $ do
      List.reverse [True, False, False] @?= reverse [True, False, False]
      List.reverse [0, 3, 1, 5] @?= reverse [0 :: Int, 3, 1, 5]

  dcriterion "get" . passOrFail $ do
    it "returns Nothing on out of bounds" $ do
      List.get 10 [2 :: Int, 3] @?= Nothing
      List.get 2  [2 :: Int, 3] @?= Nothing

    it "returns Just when in bounds" $ do
      List.get 2 [4 :: Int, 2, 1, 5]    @?= Just 1
      List.get 4 [0 :: Int, 2, 1, 3, 5] @?= Just 5

  dcriterion "(<>)" . passOrFail $ do
    it "appends empty left list" $ do
      [] List.<> [] @?= ([] :: [Int])
      [] List.<> [1, 2, 3] @?= [1 :: Int, 2, 3]

    it "appends correctly on non-empty left list" $ do
      [1, 2, 3] List.<> [] @?= [1 :: Int, 2, 3]
      [1, 2, 3, 4] List.<> [5, 6] @?= [1 :: Int, 2, 3, 4, 5, 6]
      [1] List.<> [2] @?= [1 :: Int, 2]
