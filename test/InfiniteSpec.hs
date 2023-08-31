module InfiniteSpec
  ( rubric
  ) where

import Test.Hspec
import Test.Hrubric
import Test.HUnit

import qualified Infinite

rubric :: Rubric
rubric = distribute $ do
  dcriterion "collatz" . passOrFail $ do
    it "correctly calculates sequences" $ do
      take 20 (Infinite.collatz 24) @?= [24 :: Int, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1, 4, 2, 1, 4, 2, 1]
      take 30 (Infinite.collatz 99) @?= [99 :: Int, 298, 149, 448, 224, 112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1, 4]
      take 40 (Infinite.collatz 1691) @?= [1691 :: Int, 5074, 2537, 7612, 3806, 1903, 5710, 2855, 8566, 4283, 12850, 6425, 19276, 9638, 4819, 14458, 7229, 21688, 10844, 5422, 2711, 8134, 4067, 12202, 6101, 18304, 9152, 4576, 2288, 1144, 572, 286, 143, 430, 215, 646, 323, 970, 485, 1456]

  dcriterion "fizzBuzz" . passOrFail $ do
    it "plays FizzBuzz" $ do
      take 45 Infinite.fizzBuzz @?= ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz", "16", "17", "Fizz", "19", "Buzz", "Fizz", "22", "23", "Fizz", "Buzz", "26", "Fizz", "28", "29", "FizzBuzz", "31", "32", "Fizz", "34", "Buzz", "Fizz", "37", "38", "Fizz", "Buzz", "41", "Fizz", "43", "44", "FizzBuzz"]
      take 33 Infinite.fizzBuzz @?= ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz", "16", "17", "Fizz", "19", "Buzz", "Fizz", "22", "23", "Fizz", "Buzz", "26", "Fizz", "28", "29", "FizzBuzz", "31", "32", "Fizz"]

  dcriterion "fib" . passOrFail $ do
    it "computes the fibonacci sequence" $ do
      take 45 Infinite.fib @?= [0 :: Int, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296, 433494437, 701408733]
      take 49 Infinite.fib @?= [0 :: Int, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903, 2971215073, 4807526976]
