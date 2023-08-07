module Test.Graph where

import Prelude
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Set as S
import Debug (trace)
import Graph (class Graph, WithGraph, new, runHeap, runGraphAccumT)
import Graph (empty) as G
import Test.Util (Test)
import Util ((×))

doTest :: forall g. Graph g => WithGraph g Unit
doTest = do
   α <- lift $ new S.empty
   trace (show ("New vertex: " <> show α)) \_ ->
      pure unit

testGraph :: Test Unit
testGraph = do
   let _ × δg = runHeap $ runGraphAccumT $ runExceptT doTest
   let g = δg G.empty
   trace (show g) \_ ->
      pure unit
