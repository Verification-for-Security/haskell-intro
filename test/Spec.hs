import Test.Hspec
import Test.Hrubric
import Test.HUnit

import Control.Monad
import Control.Monad.Trans.Maybe
import Grade

import qualified ListSpec as List
import qualified TreeSpec as Tree
import qualified NoRecSpec as NoRec
import qualified InfiniteSpec as Infinite
import qualified MonadsSpec as Monads

-- | The complete test suite
rubric :: Rubric
rubric = do
  criterion "It compiles" (1/10) . passOrFail $ 
    it "..." $ True @?= True
  criterion "List" (2.5/10) List.rubric
  criterion "Tree" (2/10) Tree.rubric
  criterion "NoRec" (1.5/10) NoRec.rubric
  criterion "Infinite" (1.5/10) Infinite.rubric
  criterion "Monads" (1.5/10) Monads.rubric

main :: IO ()
main = void . runMaybeT $ do
  result <- MaybeT $ hrubric rubric
  pretty result
  autograde result
