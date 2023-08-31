import Test.Hspec
import Test.Hrubric
import Test.HUnit

import System.Environment
import System.Console.ANSI
import Text.Printf

import Control.Monad
import Data.Maybe

import qualified ListSpec as List
import qualified TreeSpec as Tree
import qualified NoRecSpec as NoRec
import qualified InfiniteSpec as Infinite
import qualified MonadsSpec as Monads

-- TODO: See if this distribution makes sense
-- TODO: Make sure that students won't be able to
-- copy implementations from tests!
rubric :: Rubric
rubric = do
  criterion "It compiles" (1/10) . passOrFail $ 
    it "..." $ True @?= True
  criterion "List" (2.5/10) List.rubric
  criterion "Tree" (2/10) Tree.rubric
  criterion "NoRec" (1.5/10) NoRec.rubric
  criterion "Infinite" (1.5/10) Infinite.rubric
  criterion "Monads" (1.5/10) Monads.rubric

-- Output the weight as grade
output :: Float -> IO ()
output g = do
  let color = if g > 0.55 then Green else Red
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR [SetColor Foreground Vivid color]
  putStr $ printf "%.1f" (g * 10)
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStr "/10.0]\n"
  setSGR [Reset]

  -- Output the weight as a grade between 0 and 1 for codegrade
  codegrade <- lookupEnv "CG_INFO"
  when (isJust codegrade) $ print g

main :: IO ()
main = do
  result <- hrubric rubric
  case result of
    Left p -> putStrLn $ "Error in rubric nesting: '" ++ p ++ "'"
    Right g -> output g
