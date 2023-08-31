module TreeSpec
  ( rubric
  ) where

import Test.Hspec
import Test.Hrubric
import Test.HUnit

import qualified Tree
import Tree (Tree (..))

list :: Tree a -> [a]
list (Node l x r) = list l <> [x] <> list r
list _ = []

tree0 :: Tree Int
tree0 = Node 
  (Node Leaf 0 Leaf)
  2
  (Node Leaf 9 Leaf)

tree1 :: Tree Int
tree1 = Node
  (Node 
    (Node Leaf (-4) Leaf)
    0
    Leaf)
  2
  (Node 
    (Node
      Leaf
      4
      (Node Leaf 6 Leaf))
    9 
    (Node Leaf 11 Leaf))

list0 :: [Int]
list0 = list tree0

list1 :: [Int]
list1 = list tree1

rubric :: Rubric
rubric = distribute $ do
  dcriterion "insert" . passOrFail $ do
    let insert' e = list . Tree.insert e

    it "inserts into the empty tree" $ do
      insert' True Leaf @?= [True]

    it "inserts in the correct branch" $ do
      insert' 3 tree0 @?= [0, 2, 3, 9]

      insert' 5 tree1 @?= [-4, 0, 2, 4, 5, 6, 9, 11]
      insert' 8 tree1 @?= [-4, 0, 2, 4, 6, 8, 9, 11]
      insert' (-2) tree1 @?= [-4, -2, 0, 2, 4, 6, 9, 11]

    it "ignores duplicates" $ do
      insert' 2 tree0 @?= list0

      insert' (-4) tree1 @?= list1
      insert' 2 tree1 @?= list1
      insert' 4 tree1 @?= list1
      insert' 9 tree1 @?= list1
      insert' 11 tree1 @?= list1

  dcriterion "contains" . passOrFail $ do
    it "doesn't find elements in the empty tree" $ do
      Tree.contains (4 :: Int) Leaf @?= False

    it "finds elements that are in a tree" $ do
      Tree.contains 2 tree0 @?= True

      Tree.contains 2 tree1 @?= True
      Tree.contains 0 tree1 @?= True
      Tree.contains 9 tree1 @?= True
      Tree.contains 4 tree1 @?= True
      Tree.contains 6 tree1 @?= True
      Tree.contains (-4) tree1 @?= True

    it "doesn't find elements not in the tree" $ do
      Tree.contains 1 tree0 @?= False

      Tree.contains 1 tree1 @?= False
      Tree.contains 3 tree1 @?= False
      Tree.contains 5 tree1 @?= False

  dcriterion "fmap" . passOrFail $ do
    let fmap' f = list . Tree.fmap f
    it "correctly maps the empty tree" $ do
      Tree.fmap not Leaf @?= Leaf

    it "correctly maps non-empty trees" $ do

      fmap' (+2) tree0 @?= fmap (+2) list0
      fmap' (+2) tree1 @?= fmap (+2) list1
      fmap' (`div` 2) tree1 @?= fmap (`div` 2) list1

  dcriterion "foldl" . passOrFail $ do
    it "correctly folds the empty tree" $ do
      Tree.foldl (+) 0 Leaf @?= (0 :: Int)

    it "correctly folds non-empty trees" $ do
      Tree.foldl (+) 0 tree1 @?= sum list1
      Tree.foldl (flip (:)) [] tree1 @?= reverse list1

  dcriterion "foldr" . passOrFail $ do
    it "correctly folds the empty tree" $ do
      Tree.foldr (+) 0 Leaf @?= (0 :: Int)

    it "correctly folds non-empty trees" $ do
      Tree.foldr (+) 0 tree1 @?= sum list1
      Tree.foldr (:) [] tree1 @?= list1
