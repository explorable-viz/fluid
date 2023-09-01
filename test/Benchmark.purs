module Test.Benchmark where

import Prelude (Unit, ($))
import Data.Traversable (traverse_)
import Effect (Effect)
import Test.Util (run)
import Test.Main

main :: Effect Unit
main = do
   traverse_ run $ tests true